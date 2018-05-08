import cats.free.Free
import cats.free.Free.liftF
import cats.implicits._
import cats.~>

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import cats._


object SagaPattern {

  type Script[A] = Free[Action, A]
  type Result[A] = Either[_, A]
  type Transaction[A] = Future[Result[A]]
  type ComputationResult[A] = (A, List[CompensatingTransactionList])
  type CompensatingTransaction[A] = A => Transaction[_]
  type CompensatingTransactionList = Eval[Transaction[_]]

  sealed trait SagaStatus extends { def message: String }
  case class SagaFailed(message: String) extends Exception with SagaStatus
  case class SagaRollback(message: String) extends Exception with SagaStatus
  case class SagaRollbackFailed(message: String) extends Exception with SagaStatus

  sealed trait Action[A] {
    def lift: Script[A] = liftF(this)
  }

  case class SagaAction[A](
    mainTransaction: Transaction[A],
    compensatingTransaction: Eval[CompensatingTransaction[A]],
    compensatingTransactionList: List[CompensatingTransactionList])
    extends Action[ComputationResult[A]]

  def noAction[A](a: A): Script[A] = Monad[Script].pure(a)

  def action[A](
    mainTransaction: Transaction[A],
    compensatingTransaction: => CompensatingTransaction[A] = (a: A) => Future.successful(a.asRight))(
    transactionPoint: List[CompensatingTransactionList] = List.empty[CompensatingTransactionList]
  ): Script[ComputationResult[A]] =
    new SagaAction[A](mainTransaction, Eval.later(compensatingTransaction), transactionPoint).lift
}

class InterpreterSaga() {
  import SagaPattern._
  private val interpret: Action ~> Future = new (Action ~> Future) {

    override def apply[T](action: Action[T]): Future[T] = action match {

      case SagaAction(mainTransaction, compensatingTransaction, compensatingTransactionList) => {
        mainTransaction.map {
          case Left(err) => {
            {
              Future
                .sequence(
                  compensatingTransactionList
                    .map(_.value.recover {
                      case thr @ _ =>
                        SagaFailed(s"Saga recover fail with message = ${thr.getMessage}").asLeft
                    }))
                .flatMap {
                  case results if results.forall(_.isRight) =>
                    Future.failed(SagaRollback(s"Saga has been rolled back, by reason = $err"))
                  case _ =>
                    Future.failed(SagaRollbackFailed("Saga could not be rolled back!"))
                }
            }
          }
          case Right(v) => {
            Future.successful(
              (
                v,
                compensatingTransaction
                  .map(x => x.apply(v)) :: compensatingTransactionList))
          }

        }.flatten
      }
    }
  }

  def run[A](appAction: Script[A]): Future[A] = appAction.foldMap(interpret)
}
