package cats

import simulacrum.typeclass

@typeclass trait Monad[F[_]] {

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def join[A](ffa: F[F[A]]): F[A]

}