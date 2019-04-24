package homework1

import homework1.fingertree.FingerTree

class FingerTreeQueue[A] private (tree: FingerTree[A], realSize: Int) {
  def peek: A = tree.head

  def push(n: A): FingerTreeQueue[A] = new FingerTreeQueue(tree.append(n), realSize + 1)
  def pop: FingerTreeQueue[A] = new FingerTreeQueue(tree.tail, realSize - 1)

  def isEmpty: Boolean = size == 0
  def size: Int = realSize

  def toList: List[A] = if (isEmpty) Nil else peek :: pop.toList

}

object FingerTreeQueue {
  def empty[A]: FingerTreeQueue[A] = new FingerTreeQueue(FingerTree.empty[A], 0)

  def apply[A](xs: Seq[A]): FingerTreeQueue[A] = xs.foldLeft(FingerTreeQueue.empty[A])(_ push _)
}
