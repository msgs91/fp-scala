package cats.impl

import cats.{Applicative, Functor, Monad}

object State {

  type State[S, A] = S => (S, A)

  implicit def stateA[S]:
    Monad[({type f[x] = State[S, x]})#f] with Applicative[({type f[x] = State[S, x]})#f] with Functor[({type f[x] = State[S, x]})#f] =

    new Monad[({type f[x] = State[S, x]})#f] with Applicative[({type f[x] = State[S, x]})#f] with Functor[({type f[x] = State[S, x]})#f] {

      override def flatMap[A, B](fa: State[S, A])(f: (A) => State[S, B]): State[S, B] = {
        join(map(fa)(f))
      }

      override def join[A](ffa: State[S, State[S, A]]): State[S, A] = s => {
        val (s1, fa) = ffa(s)
        fa(s1)
      }

      override def ap[A, B](fa: State[S, A])(ff: State[S, (A) => B]): State[S, B] = s => {
        val (s1, a) = fa(s)
        val (s2, f) = ff(s1)
        (s2, f(a))
      }

      override def unit[A](a: A): State[S, A] = s => (s, a)


      override def map[A, B](fa: State[S, A])(f: (A) => B): State[S, B] = {
        ap(fa)(unit(f))
      }
    }

}
