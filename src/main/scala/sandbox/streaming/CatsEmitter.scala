package sandbox.streaming

import cats.effect.{ Concurrent, ExitCode, IO, IOApp, Timer }
import cats.effect.Console.io.putStrLn
import cats.effect.concurrent.Ref
import fs2.Stream
import enumeratum.{ Enum, EnumEntry }
import io.circe.{ Decoder, Encoder }

import scala.concurrent.duration._

object CatsEmitter extends IOApp {

  sealed trait Color extends EnumEntry
  object Color extends Enum[Color] {

    case object white extends Color
    case object black extends Color
    case object red extends Color
    case object green extends Color

    object jsonCodecs {
      implicit val colorEncoder: Encoder[Color] =
        Encoder[String].contramap(color => color.toString)

      implicit val colorDecoder: Decoder[Color] = Decoder[String].emap {
        case "white" => Right(white)
        case "black" => Right(black)
        case "red"   => Right(red)
        case "green" => Right(green)
        case other   => Left(s"Invalid color: $other")
      }
    }

    lazy val values: IndexedSeq[Color] = findValues
  }

  sealed trait Name extends EnumEntry
  object Name extends Enum[Name] {

    case object Jack extends Name
    case object Daniel extends Name
    case object Ziggy extends Name
    case object Willie extends Name

    object jsonCodecs {
      implicit val nameEncoder: Encoder[Name] =
        Encoder[String].contramap(name => name.toString)

      implicit val nameDecoder: Decoder[Name] = Decoder[String].emap {
        case "Jack"   => Right(Jack)
        case "Daniel" => Right(Daniel)
        case "Ziggy"  => Right(Ziggy)
        case "Willie" => Right(Willie)
        case other    => Left(s"Invalid name: $other")
      }
    }

    lazy val values: IndexedSeq[Name] = findValues
  }

  final case class Cat(name: Name, body: Color, eyes: Color) {
    override def toString: String = s"$name is a $body cat with $eyes eyes"
  }
  object Cat {
    object jsonCodecs {
      import Color.jsonCodecs._
      import Name.jsonCodecs._
      import io.circe.derivation._

      implicit val catEncoder: Encoder[Cat] = deriveEncoder
      implicit val catDecoder: Decoder[Cat] = deriveDecoder
    }

    def random: Cat = {
      def randomNumber = scala.util.Random.nextInt(4)

      val name      = Name.values(randomNumber)
      val bodyColor = Color.values(randomNumber)
      val eyesColor = Color.values(randomNumber)

      Cat(name, bodyColor, eyesColor)
    }
  }

  // it's implied that `effect` contains some function which yields different results on each evaluation
  def streamOfRandomData[F[_]: Concurrent: Timer, A, B](
      effect: F[A],
      emitEvery: FiniteDuration,
      stopEmittingAfter: FiniteDuration
  )(f: A => F[B]): Stream[F, B] =
    Stream
      .repeatEval(effect)
      .metered(emitEvery)
      .evalMap(f)
      .interruptAfter(stopEmittingAfter)

  def run(args: List[String]): IO[ExitCode] =
    for {
      catsCounter <- Ref.of[IO, Int](0)
      _ <- streamOfRandomData(IO(Cat.random), 150.milliseconds, 2.seconds) { cat =>
            catsCounter.update(_ + 1).flatMap { _ =>
              putStrLn(cat.toString)
            }
          }.compile.drain
      _ <- catsCounter.get.flatMap { c =>
            putStrLn(s"\nStreamed $c random cats in total")
          }
    } yield ExitCode.Success
}
