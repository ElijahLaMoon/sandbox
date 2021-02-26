package sandbox.http

import cats._
import cats.syntax.all._
import cats.effect._
import cats.effect.concurrent._

import org.http4s._
import org.http4s.dsl.{ io => _, _ }
import org.http4s.implicits._
import org.http4s.circe._, CirceEntityEncoder._
import org.http4s.server.blaze.BlazeServerBuilder

import io.circe.generic.JsonCodec

import java.util.UUID

object BooksCrud extends IOApp {

  type Title  = String
  type Author = String
  type BookId = UUID

  @JsonCodec
  final case class Book(title: Title, author: Author)

  @JsonCodec
  final case class BookWithId(id: BookId, book: Book)
  final case object BookWithId {
    def apply(book: Book): BookWithId = BookWithId(UUID.randomUUID(), book)
  }

  trait BooksRepo[F[_]] {
    def addBook(book: Book): F[BookWithId]
    def getBook(id: BookId): F[Option[BookWithId]]
    def getBooks: F[Vector[BookWithId]]
    def deleteBook(id: BookId): F[Unit]
    def updateBook(id: BookId, book: Book): F[Option[BookWithId]]
  }
  object BooksRepo {

    object impls {

      final case class TestBookRepo[F[_]: Monad](db: Ref[F, Map[BookId, Book]]) extends BooksRepo[F] {

        def addBook(book: Book): F[BookWithId] = {
          val bookWithId = BookWithId(book)
          db.update(_ + (bookWithId.id -> book)).flatMap(_ => bookWithId.pure[F])
        }

        def getBook(id: BookId): F[Option[BookWithId]] =
          db.get.map(_.get(id).map(BookWithId(id, _)))

        def getBooks: F[Vector[BookWithId]] =
          db.get
            .map(_.toVector.map {
              case (id, book) => BookWithId(id, book)
            })

        def deleteBook(id: BookId): F[Unit] =
          db.update(_ - id)

        def updateBook(id: BookId, book: Book): F[Option[BookWithId]] =
          getBook(id).flatMap {
            case Some(_) =>
              val newBook = id -> book
              db.update(_ + newBook).flatMap(_ => BookWithId(id, book).some.pure[F])

            case None => none[BookWithId].pure[F]
          }
      }

    }

  }

  final class BookRoutes[F[_]: Sync](booksRepo: BooksRepo[F]) extends Http4sDsl[F] {

    lazy val routes: HttpRoutes[F] = bookRoutes

    private[http] lazy val booksPath = "books"

    private lazy val bookRoutes: HttpRoutes[F] = HttpRoutes.of[F] {

      // GET all books
      case GET -> Root / `booksPath` => booksRepo.getBooks.flatMap(Ok(_))

      // GET book by its ID
      case GET -> Root / `booksPath` / UUIDVar(bookId) =>
        booksRepo.getBook(bookId).flatMap {
          case Some(book) => Ok(book)
          case None       => NotFound()
        }

      // POST new book
      case req @ POST -> Root / `booksPath` =>
        req.decodeJson[Book].flatMap { book =>
          booksRepo.addBook(book).flatMap(Ok(_))
        }

      // PUT new details about certain book
      case req @ PUT -> Root / `booksPath` / UUIDVar(bookId) =>
        req.decodeJson[Book].flatMap { book =>
          booksRepo.updateBook(bookId, book).flatMap {
            case Some(b) => Ok(b)
            case None    => NotFound()
          }
        }

      // DELETE certain book
      case DELETE -> Root / `booksPath` / UUIDVar(bookId) =>
        booksRepo.deleteBook(bookId).flatMap(Ok(_))
    }
  }

  def run(args: List[String]): IO[ExitCode] = {
    import scala.concurrent.ExecutionContext.global

    for {
      booksDb <- Ref.of[IO, Map[BookId, Book]](Map.empty)
      booksRepo <- BooksRepo.impls.TestBookRepo(booksDb).pure[IO]
      app <- new BookRoutes(booksRepo).routes.orNotFound.pure[IO]

      _ <- BlazeServerBuilder[IO](global)
            .bindLocal(14080)
            .withHttpApp(app)
            .serve
            .compile
            .drain
    } yield ExitCode.Success
  }
}
