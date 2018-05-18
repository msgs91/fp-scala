package cats.impl

import cats.{Applicative, Functor, Monad}

object Reader {
  type Reader[R, +A] = R => A

  implicit def readerA[R] =
    new Monad[({type f[x] = Reader[R, x]})#f] with Applicative[({type f[x] = Reader[R, x]})#f] with Functor[({type f[x] = Reader[R, x]})#f]{

      override def map[A, B](fa: Reader[R, A])(f: (A) => B): Reader[R, B] =  {
        ap(fa)(unit(f))
      }

      override def flatMap[A, B](fa: Reader[R, A])(f: (A) => Reader[R, B]): Reader[R, B] = {
        join(map(fa)(f))
      }

      override def join[A](ffa: Reader[R, Reader[R, A]]): Reader[R, A] = r => {
        ffa(r)(r)
      }

      override def ap[A, B](fa: Reader[R, A])(ff: Reader[R, (A) => B]): Reader[R, B] = r => {
        ff(r)(fa(r))
      }

      override def unit[A](a: A): Reader[R, A] = r => a

    }


}