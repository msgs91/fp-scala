package cats

import simulacrum.typeclass

@typeclass trait Semigroup[A] {

  def append(a1: A, a2: A): A

}

object Semigroup {
  implicit val strSemiGroup = new Semigroup[String] {
    override def append(a1: String, a2: String): String = s"$a1-$a2"
  }

  implicit def listSemiGroup[A] = new Semigroup[List[A]] {
    override def append(a1: List[A], a2: List[A]): List[A] = a1 ++ a2
  }
}
