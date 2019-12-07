trait MyList[+A] {
  def head: A
  def isEmpty: Boolean
  def tail: MyList[A]

  def length : Int = {
    if (isEmpty) {
      0
    } else 1 + tail.length
  }

  def drop(n: Int): MyList[A] = {
    if (isEmpty) Nil
    else if (n <= 0) this
    else  tail.drop(n-1)
  }

}

case class Cons[+A](head: A, tail: MyList[A]) extends MyList[A] {
  override def isEmpty: Boolean = false
}
case object Nil extends MyList[Nothing] {
  override def head: Nothing = throw new UnsupportedOperationException("Empty list head")
  override def tail: MyList[Nothing] = throw new UnsupportedOperationException("Empty list tail")
  override def isEmpty: Boolean = true
}

object TestList extends App {
  val l:MyList[Int] = Cons(1, Cons(2, Cons(3, Nil)))

  println(l)
  println(l.drop(2).head)
}



