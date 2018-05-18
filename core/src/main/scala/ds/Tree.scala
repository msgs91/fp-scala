package ds

import scala.annotation.tailrec

//sealed abstract class Tree[+A](a: A)
case class Node[A](a: A, left: Node[A] = null, right: Node[A] = null)

object Tree {

  def map[A, B](root: Node[A])(f: A => B): Node[B] = {
    if(root == null) return null

    val Node(a, left, right) = root
    val b = f(a)
    val newLeft =
      if(left != null){
        map(left)(f)
      } else null

    val newRight =
      if(right != null){
        map(right)(f)
      } else null

    Node(b, newLeft, newRight)

  }

  def main(args: Array[String]): Unit = {
    val root = Node(1, Node(2, right = Node(3, right = Node(4))), Node(5, Node(6, right = Node(7, right = Node(8)))))

    def f(a: Int, maxLeftLength: Int, depth: Int): Int = {
      if(depth > maxLeftLength) {
        printf(s"$a")
        maxLeftLength + 1
      } else maxLeftLength
    }

    def print(a: Int, b: Int, depth: Int): Int = {
      printf(s"$a")
      b
    }



    foldPreOrder(root, 1)(0)(print)
    println()
    foldPreOrder(root, 1)(0)(f)
  }


  def foldPostOrder[A, B](root: Node[A])(z: B)(f: (A, B) => B): B = {
    val Node(a, left, right) = root

    val newLeftZ: B =
      if(left != null)
        foldPostOrder(left)(z)(f)
      else z

    val newRightZ =
      if(right != null)
        foldPostOrder(right)(newLeftZ)(f)
      else newLeftZ

    f(a, newRightZ)
  }

  def breadth[A](root: Node[A]): Int = {
    val Node(a, left, right) = root
//    breadth()
    3
  }

  def foldPreOrder[A, B](root: Node[A], depth: Int)(z: B)(f: (A, B, Int) => B): B = {
    val Node(a, left, right) = root

    val newZ = f(a, z, depth)

    val newLeftZ =
      if(left != null)
        foldPreOrder(left, depth + 1)(newZ)(f)
      else newZ

    val newRightZ =
      if(right != null)
        foldPreOrder(right, depth + 1)(newLeftZ)(f)
      else newLeftZ

    newRightZ
  }

  def fold[A, B](root: Node[A])(z: B)(f: (A, B) => B): B = {
    val Node(a, left, right) = root
    val newLeftZ: B =
      if(left != null)
        fold(left)(z)(f)
      else z

    val newRightZ =
      if(right != null)
        fold(right)(z)(f)
      else newLeftZ

    f(a, newRightZ)

  }



  def foldInOrder[A, B](root: Node[A])(z: B)(f: (A, B) => B): B = {
    val Node(a, left, right) = root

    val newLeftZ: B =
      if(left != null)
        fold(left)(z)(f)
      else z

    val newZ = f(a, newLeftZ)

    val newRightZ =
      if(right != null)
        fold(right)(newZ)(f)
      else newZ

    newRightZ
  }

}

