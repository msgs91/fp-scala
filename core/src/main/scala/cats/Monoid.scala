package cats

import simulacrum.typeclass

@typeclass trait Monoid[A] {
  def zero: A
  def append(a1: A, b1: A): A
}

object Monoid {

}