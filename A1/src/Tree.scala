sealed trait Tree[+A]
case class Node[A](value: A, left: Tree[A], right: Tree[A]) extends Tree[A]
case class Leaf[A](value: A) extends Tree[A]
case object Nil extends Tree[Nothing]
  //def append[A](l1: List[A], l2: List[A]): List[A] = {
  //  scanRight
  //}

object Tree {

  def inOrder[A](tree: Tree[A]): Unit = {
    tree match {
      case Node(value, left, right) =>
        inOrder(left)
        print(value)
        inOrder(right)

      case Leaf(value) => print(value)
    }
  }


  def main(args: Array[String]): Unit = {
    val t = Node(1, Node(2, Leaf(4), Leaf(5)), Node(3, Leaf(6), Leaf(7)))
    val l = List().empty
    inOrder(t)

  }

}
