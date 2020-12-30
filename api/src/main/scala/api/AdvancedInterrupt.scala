package api

import cats.effect.{ContextShift, IO, Timer}
import fs2.Stream
import fs2.concurrent.SignallingRef

import scala.concurrent.ExecutionContext

object AdvancedInterrupt extends App {

  implicit val contextShift: ContextShift[IO] = IO.contextShift(ExecutionContext.global)
  implicit val timer: Timer[IO] = IO.timer(ExecutionContext.global)

  val limit: Long = 150 // Produces Exception limit is under 100 (as expected)

  val flagIo: IO[SignallingRef[IO, Boolean]] = SignallingRef[IO, Boolean](false)
  val dataStream: Stream[IO, (Long, Char)] = Stream
    .constant('a')
    .repeat
    .mapAccumulate(0L)((acc, b) => (acc + 1L, b))
    .take(100)
    .covary[IO]

  val result = for {
    f <- Stream.eval(flagIo)
    s <- dataStream.interruptWhen(f)
    _ <- Stream.eval(f.set(s._1 > limit))
    x3 <- Stream.eval(f.get)
  } yield {
    if (x3) Left(new Exception("Too much data"))
    else Right(s, x3)
  }

  val io: Either[Throwable, ((Long, Char), Boolean)] = result
    .flatMap {
      case Right(v) => Stream.emit(v)
      case Left(e) => Stream.raiseError[IO](e)
    }
    .attempt
    .evalTap(e => IO(println(e)))
    .compile
    .lastOrError
    .unsafeRunSync()

  println
  println(s"Final result = $io")
}
