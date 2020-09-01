package com.adobe.aep

import akka.actor.ActorSystem
import org.scalatest.{Matchers, WordSpecLike}

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._
import scala.language.postfixOps

class FutureFailureSpec extends WordSpecLike with Matchers {

  val actorSystem: ActorSystem = ActorSystem("ProfileActorSystem")
  implicit val ec: ExecutionContext = actorSystem.dispatcher

  private trait Generator {
    def seed(): Future[Int]
    def process(n: Int): Future[Int] = Future.successful(n)
    def generate(): Future[Int] = {
      seed().flatMap(n =>
        process(n)
      ).recover {
        case _: NumberFormatException => -1
        case th => throw th
      }
    }
  }

  private trait GenSuccessfullyContext {
    val processor: Generator = new Generator {
      override def seed(): Future[Int] = Future.successful(1)
    }
  }

  private trait GenFailureContext {
    val processor: Generator = new Generator {
      override def seed(): Future[Int] = throw new NumberFormatException
    }
  }

  private trait GenFutureNumberFormatExceptionContext {
    val processor: Generator = new Generator {
      override def seed(): Future[Int] = Future.failed(new NumberFormatException)
    }
  }

  private trait GenFutureIllegalArgumentExceptionContext {
    val processor: Generator = new Generator {
      override def seed(): Future[Int] = Future.failed(new IllegalArgumentException)
    }
  }

  private trait ProcessFailureContext {
    val processor: Generator = new Generator {
      override def seed(): Future[Int] = Future.successful(1)

      override def process(n: Int): Future[Int] = throw new NumberFormatException
    }
  }

  private trait ProcessFutureNumberFormatExceptionContext {
    val processor: Generator = new Generator {
      override def seed(): Future[Int] = Future.successful(1)

      override def process(n: Int): Future[Int] = Future.failed(new NumberFormatException)
    }
  }

  private trait ProcessFutureIllegalArgumentExceptionContext {
    val processor: Generator = new Generator {
      override def seed(): Future[Int] = Future.successful(1)

      override def process(n: Int): Future[Int] = Future.failed(new IllegalArgumentException)
    }
  }

  "Generator seed successfully" should {
    "start without any error" in new GenSuccessfullyContext {
      assertResult(1) {
        Await.result(processor.generate(), 10 seconds)
      }
    }
  }

  "Generator seed fails" should {
    "throws exception when seed() does not throw exception in future" in new GenFailureContext {
      intercept[NumberFormatException] {
        Await.result(processor.generate(), 10 seconds)
      }
    }

    "returns default value of -1 when seed() throws exception in future" in new GenFutureNumberFormatExceptionContext {
      assertResult(-1) {
        Await.result(processor.generate(), 10 seconds)
      }
    }

    "throws exception when seed() throw uncaught exception in future" in new GenFutureIllegalArgumentExceptionContext {
      intercept[IllegalArgumentException] {
        Await.result(processor.generate(), 10 seconds)
      }
    }
  }

  /**
   * Pay attention on the behavior differences compare to the previous case.
   * - exception is throw in the nested future
   */
  "Generator process fails" should {
    "throws exception when process(n) does not throw exception in future" in new ProcessFailureContext {
      assertResult(-1) {
        Await.result(processor.generate(), 10 seconds)
      }
    }

    "throws exception when process(n) throws exception in future" in new ProcessFutureNumberFormatExceptionContext {
      assertResult(-1) {
        Await.result(processor.generate(), 10 seconds)
      }
    }

    "throws exception when process(n) throws uncaught exception in future" in new ProcessFutureIllegalArgumentExceptionContext {
      intercept[IllegalArgumentException] {
        Await.result(processor.generate(), 10 seconds)
      }
    }
  }
}
