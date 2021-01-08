package api

import cats.effect.{ContextShift, IO, Timer}
import fs2._
import fs2.concurrent.SignallingRef

import scala.concurrent.ExecutionContext

object SimpleInterrupt extends App {

  implicit val contextShift: ContextShift[IO] = IO.contextShift(ExecutionContext.global)
  implicit val timer: Timer[IO] = IO.timer(ExecutionContext.global)

  val flagIo: IO[SignallingRef[IO, Boolean]] = SignallingRef[IO, Boolean](false)
  val dataStream: Stream[IO, (Long, Char)] = Stream
    .constant('a')
    .repeat
    .mapAccumulate(0L)((acc, b) => (acc + 1L, b))
    .covary[IO]

  val result: Stream[IO, ((Long, Char), Boolean)] = for {
    f <- Stream.eval(flagIo)
    s <- dataStream.interruptWhen(f)
    _ <- Stream.eval(f.set(s._1 > 2))
    x3 <- Stream.eval(f.get)
  } yield (s, x3)

  result
    .evalMap(e => IO(println(e)))
    .compile
    .drain
    .unsafeRunSync()

}
