package cats

import simulacrum.typeclass

@typeclass trait Applicative[F[_]] extends Functor[F] {

  def ap[A, B](fa: F[A])(ff: F[A => B]): F[B]

  def unit[A](a: A): F[A]
}
