package homework1.fingertree

sealed trait ViewT[A]

case class EmptyView[A] () extends ViewT[A]
case class View[A] (e: A, tree: FingerTree[A]) extends ViewT[A]
