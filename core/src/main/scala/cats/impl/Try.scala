package cats.impl

import cats._

sealed trait Try[+E, +A]
case class Failure[+E](e: E) extends Try[E, Nothing]
case class Success[+A](a: A) extends Try[Nothing, A]


object Try {

  implicit def tryA[E](implicit EE: Semigroup[E]):
  Monad[({type f[x] = Try[E, x]})#f] with Applicative[({type f[x] = Try[E, x]})#f] with Functor[({type f[x] = Try[E, x]})#f] =
  //using type lambda syntax to partially apply types to higher kinded types
  //https://github.com/non/kind-projector - a compiler plugin that provides a better syntax for the same
    new Monad[({type f[x] = Try[E, x]})#f] with Applicative[({type f[x] = Try[E, x]})#f] with Functor[({type f[x] = Try[E, x]})#f] {

      //functor
      override def map[A, B](fa: Try[E, A])(f: (A) => B): Try[E, B] = {
        ap(fa)(unit(f))
      }

      def map2[A, B, C](fa: Try[E, A])(fb: Try[E, B])(f: (A, B) => C): Try[E, C] = {
        ap(fb)(ap(fa)(unit(f.curried)))
      }

      //applicative
      override def ap[A, B](fa: Try[E, A])(ff: Try[E, A => B]): Try[E, B] = (ff, fa) match {
        case (Success(f), Success(a)) => Success(f(a))
        case (Success(f), Failure(e)) => Failure(e)
        case (Failure(e), Success(a)) => Failure(e)
        case (Failure(e1), Failure(e2)) => Failure(EE.append(e1, e2))
      }

      //applicative
      override def unit[A](a: A): Try[E, A] = {
        Success(a)
      }

      //monad
      override def flatMap[A, B](fa: Try[E, A])(f: A => Try[E, B]): Try[E, B] = {
        join(map(fa)(f))
      }

      def flatMap2[A, B, C](fa: Try[E, A])(fb: Try[E, B])(f: (A, B) => Try[E, C]): Try[E, C] = {
        flatMap(fa)(a => flatMap(fb)(b => f(a, b)))
      }

      //monad
      override def join[A](ffa: Try[E, Try[E, A]]): Try[E, A] = ffa match {
        case Success(Success(a)) => Success(a)
        case Success(Failure(e)) => Failure(e)
        case Failure(e) => Failure(e)
      }

    }

}
