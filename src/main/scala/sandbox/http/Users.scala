package sandbox.http

import cats.syntax.functor._
import cats.effect.Sync
import cats.effect.concurrent.Ref

trait Users[F[_]] {
  def create(user: User): F[Unit]
  def update(user: User): F[Unit]
  def get(id: Int): F[Option[User]]
  def getAll: F[Vector[User]]
  def delete(id: Int): F[Unit]
}

object Users {
  object impls {

    case class TestUsers[F[_]: Sync](repo: Ref[F, Vector[User]]) extends Users[F] {
      def create(user: User): F[Unit] = repo.update(_ :+ user)
      def update(user: User): F[Unit] =
        repo.update(_.map { u =>
          if (u.id == user.id) user
          else u
        })
      def get(id: Int): F[Option[User]] = repo.get.map(_.find(_.id == id))
      def getAll: F[Vector[User]]       = repo.get
      def delete(id: Int): F[Unit]      = repo.update(_.filter(_.id != id))
    }
  }
}
