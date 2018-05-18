package cats.impl

import cats.{Applicative, Functor, Monad}

sealed trait Maybe[+A]
case class Just[A](a: A) extends Maybe[A]
case object Nothyng extends Maybe[Nothing]

object Maybe {

  implicit val MaybeA = new Monad[Maybe] with Functor[Maybe] with Applicative[Maybe] {

    //functor
    override def map[A, B](fa: Maybe[A])(f: (A) => B): Maybe[B] = {
      ap(fa)(unit(f))
    }

    //applicative
    override def ap[A, B](fa: Maybe[A])(ff: Maybe[(A) => B]): Maybe[B] = (ff, fa) match {
      case (Just(f), Just(a)) => Just(f(a))
      case (Just(f), Nothyng) => Nothyng
      case (Nothyng, Just(a)) => Nothyng
      case (Nothyng, Nothyng) => Nothyng
    }

    //applicative
    override def unit[A](a: A): Maybe[A] = {
      Just(a)
    }

    //monad
    override def flatMap[A, B](fa: Maybe[A])(f: A => Maybe[B]): Maybe[B] = {
      val ffa: Maybe[Maybe[B]] = map(fa)(f)
      join(ffa)
    }

    //monad
    override def join[A](ffa: Maybe[Maybe[A]]): Maybe[A] = ffa match {
      case Just(fa) => fa
      case _ => Nothyng
    }
  }
}
