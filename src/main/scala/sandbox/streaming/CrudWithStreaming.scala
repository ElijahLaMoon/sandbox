package sandbox.streaming

import CatsEmitter.Cat
import cats._
import cats.syntax.all._
import cats.effect._
import cats.effect.concurrent.Ref
import fs2.{ io => _, _ }
import org.http4s._
import org.http4s.dsl.{ io => _, _ }
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder

import scala.concurrent.duration._
import java.util.UUID

object CrudWithStreaming extends IOApp {

  type CatId = UUID

  final case class CatWithId(id: CatId, cat: Cat)
  object CatWithId {
    def apply(cat: Cat): CatWithId = CatWithId(UUID.randomUUID, cat)

    object jsonCodecs {
      import Cat.jsonCodecs._
      import io.circe.{ Decoder, Encoder }
      import io.circe.derivation.{ deriveDecoder, deriveEncoder }

      implicit val catWithIdEncoder: Encoder[CatWithId] = deriveEncoder
      implicit val catWithIdDecoder: Decoder[CatWithId] = deriveDecoder
    }
  }

  object server {

    trait CatsRepo[F[_]] {
      def addCat(cat: Cat): F[CatWithId]
      def getCat(id: CatId): F[Option[CatWithId]]
      def getCats(offset: Long): F[Vector[CatWithId]]
      def getCatsStream: F[Stream[F, CatWithId]]
      def deleteCat(id: CatId): F[Unit]
      def updateCat(id: CatId, cat: Cat): F[Option[CatWithId]]
    }

    object CatsRepo {

      object impls {

        final case class InMemoryCatsRepo[F[_]: Concurrent: Timer](db: Ref[F, Map[CatId, Cat]]) extends CatsRepo[F] {

          def addCat(cat: Cat): F[CatWithId] = {
            val catWithId = CatWithId(cat)
            db.update { m =>
                m + (catWithId.id -> cat)
              }
              .flatMap(_ => catWithId.pure[F])
          }

          def getCat(id: CatId): F[Option[CatWithId]] =
            db.get.map { m =>
              m.get(id).map(CatWithId(id, _))
            }

          def getCats(offset: Long): F[Vector[CatWithId]] =
            db.get.flatMap { m =>
              Stream
                .fromIterator[F](m.iterator)
                .drop(offset)
                .take(100)
                .map { case (id, cat) => CatWithId(id, cat) }
                .compile
                .toVector
            }

          def getCatsStream: F[Stream[F, CatWithId]] =
            Stream
              .repeatEval { Defer[F].defer(CatWithId(Cat.random).pure[F]) }
              .metered(250.milliseconds)
              .interruptAfter(3.seconds)
              .pure[F]

          def deleteCat(id: CatId): F[Unit] =
            db.update(_ - id)

          def updateCat(id: CatId, cat: Cat): F[Option[CatWithId]] =
            getCat(id).flatMap {
              case Some(_) =>
                val newCat = id -> cat
                db.update(_ + newCat).flatMap(_ => CatWithId(id, cat).some.pure[F])

              case None => none[CatWithId].pure[F]
            }
        }

      }

    }

    final case class CatsRoutes[F[_]: Sync](catsRepo: CatsRepo[F]) extends Http4sDsl[F] {
      import Cat.jsonCodecs._
      import CatWithId.jsonCodecs._
      import org.http4s.circe._, CirceEntityCodec._
      import io.circe._, syntax._

      lazy val routes: HttpRoutes[F] = catsRoutes

      private[streaming] lazy val catsPath = "cats"

      private lazy val catsRoutes: HttpRoutes[F] = HttpRoutes.of[F] {

        // GET /cats/`offset`
        case GET -> Root / `catsPath` / LongVar(offset) =>
          catsRepo.getCats(offset).flatMap(Ok(_))

        // GET /cats/`id`
        case GET -> Root / `catsPath` / UUIDVar(id) =>
          catsRepo.getCat(id).flatMap {
            case Some(cat) => Ok(cat)
            case None      => NotFound()
          }

        // GET /cats/stream
        case GET -> Root / `catsPath` / "stream" =>
          catsRepo.getCatsStream.flatMap { s =>
            Ok(s.map(_.asJson))
          }

        // POST /cats
        case req @ POST -> Root / `catsPath` / "single" =>
          req.decodeJson[Cat].flatMap { cat =>
            catsRepo.addCat(cat).flatMap(Ok(_))
          }

        // PUT /cats/`id`
        case req @ PUT -> Root / `catsPath` / UUIDVar(id) =>
          req.decodeJson[Cat].flatMap { cat =>
            catsRepo.updateCat(id, cat).flatMap {
              case Some(c) => Ok(c)
              case None    => NotFound()
            }
          }

        // DELETE /cats/`id`
        case DELETE -> Root / `catsPath` / UUIDVar(id) =>
          catsRepo.deleteCat(id).flatMap(Ok(_))
      }
    }
  }

  object client {}

  def run(args: List[String]): IO[ExitCode] = {
    import scala.concurrent.ExecutionContext.global
    import server._

    for {
      catsDb <- Ref.of[IO, Map[CatId, Cat]](Map.empty)
      catsRepo <- CatsRepo.impls.InMemoryCatsRepo(catsDb).pure[IO]
      app <- CatsRoutes(catsRepo).routes.orNotFound.pure[IO]

      _ <- BlazeServerBuilder[IO](global)
            .bindLocal(13080)
            .withHttpApp(app)
            .serve
            .compile
            .drain
    } yield ExitCode.Success
  }
}
