package homework1

class SimpleQueue[A] private (val realSize: Int, val front: List[A], val rrear: List[A]) {
  def peek: A =
    if (size == 0)
      throw new NoSuchElementException
    else if (front != Nil)
      front.head
    else
      rrear.last

  def push(n: A): SimpleQueue[A] = new SimpleQueue(size + 1, front, n :: rrear)
  def pop: SimpleQueue[A] =
    if (size == 0)
      throw new NoSuchElementException
    else if (front != Nil)
      new SimpleQueue(size - 1, front.tail, rrear)
    else
      new SimpleQueue(size - 1, rrear.reverse.tail, Nil)

  def isEmpty: Boolean = size == 0
  def size: Int = realSize

  def toList: Seq[A] = front ++ rrear.reverse
}

object SimpleQueue {
  def empty[A]: SimpleQueue[A] = new SimpleQueue(0, Nil, Nil)

  def apply[A](xs: Seq[A]): SimpleQueue[A] = new SimpleQueue(xs.size, xs.toList, Nil)
}
