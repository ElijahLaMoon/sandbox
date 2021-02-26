package sandbox.http

import cats.effect.Console.io.{ putStr, putStrLn }
import cats.effect.{ ExitCode, IO, IOApp }
import cats.effect.concurrent.Ref
import Users.impls.TestUsers

object Business extends IOApp {

  def run(args: List[String]): IO[ExitCode] = {
    def printAllUsers(implicit users: TestUsers[IO]): IO[Unit] =
      users.getAll.flatMap { allUsers =>
        val usersNamesAsString = allUsers.map(_.name).mkString(", ")
        putStrLn(usersNamesAsString)
      }

    val ioUsers: IO[TestUsers[IO]] = Ref.of[IO, Vector[User]](Vector.empty[User]).map(TestUsers(_))

    for {
      implicit0(users: TestUsers[IO]) <- ioUsers

      _ <- users.create(User(1, "Jack"))
      _ <- users.create(User(2, "Elijah"))

      _ <- putStr("Should be Jack and Elijah: ")
      _ <- printAllUsers

      _ <- users.update(User(1, "Bob"))
      _ <- putStr("Jack should be Bob now: ")
      _ <- printAllUsers

      _ <- users.delete(1)
      _ <- putStr("There's should be only Elijah now: ")
      _ <- printAllUsers
    } yield ExitCode.Success
  }
}
