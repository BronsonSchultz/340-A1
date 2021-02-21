import scala.::

case object Luhn {

  def luhnDouble(x: Int): Int = {
    if (x * 2 > 9) {
      x * 2 - 9
    }
    else {
      x * 2
    }
  }

  def altMap[B,C](funcs: List[B => C], list: List[B], index: Int): List[C] = {
//    list match {
//      case Nil => Nil
//      case head :: tail => funcs(index)(head) :: altMap(funcs, tail, index+1)
//    }
    val first_func = funcs.head
    funcs match {
        // else apply the next function in funcs to the next element in list
      case head :: tail => head (list(index)) :: altMap(tail, list, index+1)
      // when we reach the last function, apply the first one again
      case Nil => first_func (list(index)) :: Nil
    }
  }

  def main(args: Array[String]): Unit = {
    val funcs = List( (x: Int) => x + 10, (x: Int) => x + 100)
    val nums = List(0,1,2,3,4)
    println(altMap(funcs, nums, 0))
  }
}
