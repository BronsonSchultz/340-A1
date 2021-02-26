import scala.Console.println

sealed trait Tree[+A]



  //def append[A](l1: List[A], l2: List[A]): List[A] = {
  //  scanRight
  //}

object Tree {

  case class Node[A](value: A, left: Option[Node[A]]=None, right: Option[Node[A]]=None) extends Tree[A]{
    def map[B](f: A => B): Node[B] = {
      Node(f(value), left.map(l => l.map(f)), right.map(r => r.map(f)))
    }
  }
  case class Leaf[A](value: A, left: Option[Node[A]]=None, right: Option[Node[A]]=None) extends Tree[A]
//  def Leaf[A](value : A) = Node(value,None,None)
//  case class Nil() extends Tree[Nothing]



//
//  def inOrder[A](tree: Tree[A]): Unit = {
//    tree match {
//      case Node(value, left, right) =>
//        inOrder(left)
//        print(value)
//        inOrder(right)
//
//      case Leaf(value) => print(value)
//    }
//  }
//
//  def postOrder[A](tree: Tree[A]): Unit = {
//    tree match {
//      case Node(value, left, right) =>
//        postOrder(left)
//        postOrder(right)
//        print(value)
//
//      case Leaf(value) => print(value)
//    }
//  }
//
  def preOrder[A](tree: Node[A]): List[A] = {
    var list = List[A]()
    def _preOrder(node: Node[A]): Unit = {
      list = node.value :: list
      if (node.left.isDefined) {
        _preOrder(node.left.get)
      }

      if (node.right.isDefined) {
        _preOrder(node.right.get)
      }

    }
    _preOrder(tree)
    list.reverse
  }


  def search[A](tree: Node[A], key: A): Boolean = {
    var list = List[A]()
    tree.map(t => (list = t :: list))

    list.contains(key)
  }
//    tree match {
//      case Node(value,left,right) =>
//        if (value == key) {
//          return true
//        } else {
//          search(left, key)
//          search(right, key)
//        }
//
//    case Leaf(value) =>
//        if (value == key){
//          true
//        } else {
//          false
//        }
//    }
  def main(args: Array[String]): Unit = {
    val t = Node[Int](1, Some(Node(2, Some(Node(4)), None)), Some(Node(3, Some(Node(5)), Some(Node(6)))))

    val p = preOrder(t)
    println(p)
    println(search(t, 3))
  }
}
