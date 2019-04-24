package homework1

case class Node[A] private[homework1] (val realSize: Int, val depth: Int, val root: Option[A], val priority: Int, val left: Node[A], val right: Node[A]) {
  def peek: A = root match {
    case None => throw new NoSuchElementException
    case Some(e) => e
  }

  def popLast: ((A, Int), Node[A]) = {
    val newSize = size - 1

    if (size == 0)
      throw new NoSuchElementException
    else if (size == 1)
      ((root.get, priority), new Node(0, 0, None, 0, null, null))
    else if (size == 2) {
      val (result, newLeft) = left.popLast
      (result, new Node(newSize, 0, root, priority, newLeft, right))
    }
    else if (left.depth != right.depth) {
      val (result, newLeft) = left.popLast
      val newDepth = if (left.size == 2 * (depth - 1)) depth - 1 else depth
      (result, new Node(newSize, newDepth, root, priority, newLeft, right))
    }
    else {
      val (result, newRight) = right.popLast
      (result, new Node(newSize, depth, root, priority, left, newRight))
    }
  }

  def push(n: A, p: Int): Node[A] = {
    val newSize = size + 1
    val maxChildSize = (2 << depth - 1) - 1

    // no need to sift-up, priority is auto incremented, so it should be heapified
    if (size == 0)
      new Node(newSize, depth, Some(n), p, left, right)
    else if (size == 1)
      new Node(newSize, depth + 1, root, priority, new Node(1, 0, Some(n), p, null, null), right)
    else if (size == 2)
      new Node(newSize, depth, root, priority, left, new Node(1, 0, Some(n), p, null, null))
    else if (left.size < maxChildSize)
      new Node(newSize, depth, root, priority, left.push(n, p), right)
    else if (right.size < maxChildSize)
      new Node(newSize, depth, root, priority, left, right.push(n, p))
    else
      new Node(newSize, depth + 1, root, priority, left.push(n, p), right)
  }
  def pop: Node[A] = {
    val ((n, p), newRoot) = popLast
    new Node(newRoot.size, newRoot.depth, Some(n), p, newRoot.left, newRoot.right).siftDown
  }

  def siftDown: Node[A] = {
    def swapLeft(node: Node[A]): Node[A] = {
      val newLeft: Node[A] = new Node(node.left.size, node.left.depth, node.root, node.priority, node.left.left, node.left.right).siftDown
      new Node(node.size, node.depth, node.left.root, node.left.priority, newLeft, node.right)
    }

    def swapRight(node: Node[A]): Node[A] = {
      val newRight: Node[A] = new Node(node.right.size, node.right.depth, node.root, node.priority, node.right.left, node.right.right).siftDown
      new Node(node.size, node.depth, node.right.root, node.right.priority, node.left, newRight)
    }

    if (size <= 1)
      this
    else if (size == 2) {
      if (left.priority < priority) swapLeft(this) else this
    }
    else if (left.priority < priority) {
      if (right.priority < left.priority)
        swapRight(this)
      else
        swapLeft(this)
    }
    else if (right.priority < priority) {
      if (left.priority < right.priority)
        swapLeft(this)
      else
        swapRight(this)
    }
    else
      this
  }

  def size: Int = realSize

  def toList: List[A] = if (size == 0) Nil else peek :: pop.toList
}

case class PriorityQueue[A] private (val node: Node[A], val counter: Int) {
  def peek: A = node.peek

  def push(n: A): PriorityQueue[A] = new PriorityQueue(node.push(n, counter), counter + 1)
  def pop: PriorityQueue[A] = new PriorityQueue(node.pop, counter)

  def isEmpty: Boolean = size == 0
  def size: Int = node.size

  def toList: Seq[A] = node.toList
}

object PriorityQueue {
  def empty[A]: PriorityQueue[A] = new PriorityQueue(new Node(0, 0, None, 0, null, null), 0)

  def apply[A](xs: Seq[A]): PriorityQueue[A] = xs.foldLeft(PriorityQueue.empty[A])(_ push _)
}
