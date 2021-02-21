case object Faro_shuffle{

  def shuffle[A](l1: List[A], l2: List[A]): List[A] ={
    l1 match {
      case head :: tail => head :: shuffle(l2,tail)
      case _ => l2
    }
  }

  def split[A](list: List[A], n:Int): List[List[A]] = {
    List(list.take(n), list.drop(n))
  }

  def outShuffle[A](list: List[A]): List[A] = {
    val halves = split(list, list.length / 2)
    shuffle(halves.head, halves.last)
  }

  def inShuffle[A](list: List[A]): List[A] = {
    val halves = split(list, list.length / 2)
    shuffle(halves.last, halves.head)
  }

  def nShuffle[A](num_shuffles: Int) (list: List[A]) (shuffle_func: List[A] => List[A]): List[A] = {

    @annotation.tailrec
    def loop(i: Int, l: List[A]): List[A] = {
      if (i >= num_shuffles) l
      else loop(i+1, shuffle_func(l))
    }

    loop(0, list)
  }

  def howManyShuffles[A](l1: List[A]) (l2: List[A]) (shuffle_func: List[A] => List[A]): Int = {

    @annotation.tailrec
    def loop(i: Int, l: List[A]): Int ={
      if (l.equals(l2)) i
      else loop(i+1, shuffle_func(l))
    }
    loop(0, l1)
  }

  def main(args: Array[String]): Unit = {
    val l1 = List(0,1,2,3)
    val halves = split(l1, l1.length / 2)
    val out = outShuffle(l1)
    val in = inShuffle(l1)
    val n = 2
    val s = nShuffle(n) (l1) (outShuffle)

    val q = howManyShuffles(l1) (List(0,2,1,3)) (outShuffle)

//    println("original list", l1)
//    println("havles after split", halves)
//    println("list outshuffled", out)
//    println("list inshuffled", in)
    println(s)
    println(q)

  }
}
