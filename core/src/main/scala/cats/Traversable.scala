package cats

import cats.impl.{Failure, Reader, Success, Try}
import simulacrum._

import Applicative.ops._

@typeclass trait Traversable[F[_]] {

  def sequence[M[_], A](fmas: F[M[A]])(implicit A: Applicative[M]): M[F[A]]

}

object Traversable {
  import Reader._
  import Try._

  def sequence[M[_]: Applicative, A](mas: List[M[A]]): M[List[A]] = {
   mas.foldLeft(Applicative[M].unit(List[A]()))((acc, ma) => {
      acc.ap(ma.ap(Applicative[M].unit(((a:A, z: List[A]) => z ++ List(a)).curried)))
   })
  }


  def isEven: Reader[Int, Try[List[String], Int]] = r => {
    if (r % 2 == 0) Success(r)
    else Failure(List("Not an even number"))
  }

  def is3: Reader[Int, Try[List[String], Int]] = r => {
    if (r % 3 == 0) Success(r)
    else Failure(List("Not divisible by 3"))
  }

  def is6(r: Int) = {
    val y: List[Try[List[String], Int]] = List(isEven, is3) map (f => f(r))
    val z: Try[List[String], List[Int]] = sequence[({type f[x] = Try[List[String], x]})#f, Int](y)
    println(z)
  }

  def is6A(r: Int) = {
    val y: List[Reader[Int, Try[List[String], Int]]] = List(isEven, is3)
    val z = sequence[({type f[x] = Reader[Int, x]})#f, Try[List[String], Int]](y)
    val a = sequence[({type f[x] = Try[List[String], x]})#f, Int](z(r))
    println(a)
  }

  def is7: Reader[Int, Boolean] = r => {
    if(r % 7 == 0) true
    else false
  }

  def main(args: Array[String]): Unit = {
    is6A(5)

    val y = List(is7)
    val z: Reader[Int, List[Boolean]] = sequence[({type f[x] = Reader[Int, x]})#f, Boolean](y)
    println(z(10))

  }

}

