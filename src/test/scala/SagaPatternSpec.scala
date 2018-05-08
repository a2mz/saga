
import SagaPattern._
import cats.free.Free
import cats.implicits._
import org.mockito.Mockito.{times, verify, when}
import org.scalatest.mockito.MockitoSugar
import org.scalatest.{AsyncFlatSpec, Matchers}

import scala.concurrent.Future

class SagaPatternSpec extends AsyncFlatSpec with Matchers with MockitoSugar {

  private val saga = new InterpreterSaga()

  private def runAction(action: Free[Action, Int]): Future[Int] = saga.run(action).recover {
    case SagaFailed(mes) => println(s"err: $mes"); 1000
    case SagaRollback(mes) => println(s"info: $mes"); 2000
    case SagaRollbackFailed(mes) => println(s"err: $mes"); 3000
  }

  "Service" should "execute mainTransactionA" in {
    val mockFunctions = mock[MockFunction]
    when(mockFunctions.mainTransactionA(1)).thenReturn(Future.successful { 13.asRight })
    when(mockFunctions.compensatingTransactionA(1)).thenReturn(Future.successful { 10.asRight })

    def testCase =
      for {
        a <- action(mockFunctions.mainTransactionA(1), mockFunctions.compensatingTransactionA)()
      } yield a._1
    runAction(testCase).map { r =>
      verify(mockFunctions, times(0)).compensatingTransactionA(1)
      verify(mockFunctions, times(1)).mainTransactionA(1)
      r shouldEqual 13
    }

  }

  it should "execute mainTransactionA with failure" in {
    val mockFunctions = mock[MockFunction]
    when(mockFunctions.mainTransactionA(1)).thenReturn(Future.successful { "err".asLeft })
    when(mockFunctions.compensatingTransactionA(1)).thenReturn(Future.successful { 10.asRight })
    when(mockFunctions.mainTransactionB(1)).thenReturn(Future.successful { 100.asRight })
    when(mockFunctions.compensatingTransactionB(1)).thenReturn(Future.successful { 10.asRight })

    def testCase =
      for {
        a <- action(mockFunctions.mainTransactionA(1), mockFunctions.compensatingTransactionA)()
        b <- action(mockFunctions.mainTransactionB(1), mockFunctions.compensatingTransactionB)(a._2)
      } yield b._1

    runAction(testCase).map { r =>
      verify(mockFunctions, times(1)).mainTransactionA(1)
      verify(mockFunctions, times(0)).compensatingTransactionA(1)
      verify(mockFunctions, times(0)).mainTransactionB(1)
      verify(mockFunctions, times(0)).compensatingTransactionB(1)
      r shouldEqual 2000
    }
  }

  it should "execute mainTransactionA and B" in {
    val mockFunctions = mock[MockFunction]
    when(mockFunctions.mainTransactionA(1)).thenReturn(Future.successful { 10.asRight })
    when(mockFunctions.compensatingTransactionA(10)).thenReturn(Future.successful { 10.asRight })
    when(mockFunctions.mainTransactionB(10)).thenReturn(Future.successful { 20.asRight })
    when(mockFunctions.compensatingTransactionB(20)).thenReturn(Future.successful { 20.asRight })

    def testCase =
      for {
        a <- action(mockFunctions.mainTransactionA(1), mockFunctions.compensatingTransactionA)()
        b <- action(mockFunctions.mainTransactionB(a._1), mockFunctions.compensatingTransactionB)(a._2)
      } yield b._1

    runAction(testCase).map { r =>
      verify(mockFunctions, times(1)).mainTransactionA(1)
      verify(mockFunctions, times(0)).compensatingTransactionA(10)
      verify(mockFunctions, times(1)).mainTransactionB(10)
      verify(mockFunctions, times(0)).compensatingTransactionB(20)
      r shouldEqual 20
    }
  }
  it should "execute mainTransactionA and B with failure then rollback by compensatingTransactionA" in {
    val mockFunctions = mock[MockFunction]
    when(mockFunctions.mainTransactionA(1)).thenReturn(Future.successful { 10.asRight })
    when(mockFunctions.compensatingTransactionA(10)).thenReturn(Future.successful { 10.asRight })
    when(mockFunctions.mainTransactionB(10)).thenReturn(Future.successful { "err".asLeft })
    when(mockFunctions.compensatingTransactionB(20)).thenReturn(Future.successful { 20.asRight })

    def testCase =
      for {
        a <- action(mockFunctions.mainTransactionA(1), mockFunctions.compensatingTransactionA)()
        b <- action(mockFunctions.mainTransactionB(a._1), mockFunctions.compensatingTransactionB)(a._2)
      } yield b._1

    runAction(testCase).map { r =>
      verify(mockFunctions, times(1)).mainTransactionA(1)
      verify(mockFunctions, times(1)).compensatingTransactionA(10)
      verify(mockFunctions, times(1)).mainTransactionB(10)
      verify(mockFunctions, times(0)).compensatingTransactionB(20)
      r shouldEqual 2000
    }
  }

}

abstract class MockFunction {
  def mainTransactionA(i: Int): Future[Either[String, Int]]
  def compensatingTransactionA(i: Int): Future[Either[String, Int]]
  def mainTransactionB(i: Int): Future[Either[String, Int]]
  def compensatingTransactionB(i: Int): Future[Either[String, Int]]
}
