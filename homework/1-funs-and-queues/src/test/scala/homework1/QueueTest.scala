package homework1

import org.scalatest.{FlatSpec, Matchers}

class QueueTest extends FlatSpec with Matchers {
  "an empty queue" should "produce a queue with a single element when that element is added to it" in {
    val emptyQueue = Queue.empty[Int]
    val singleElementQueue = emptyQueue.push(42)

    singleElementQueue.peek shouldBe 42
    singleElementQueue.size shouldBe 1
  }

  "small queue" should "with push pop in sequance" in {
    val emptyQueue = Queue.empty[Int]
    val smallQueue = emptyQueue.push(42).push(24)
    val smallerQueue = smallQueue.pop

    emptyQueue.size shouldBe 0

    smallQueue.size shouldBe 2
    smallQueue.peek shouldBe 42

    smallerQueue.size shouldBe 1
    smallerQueue.peek shouldBe 24
  }

  "large queue" should "handle pop" in {
    val largeQueue = Queue(100.to(30).by(-10))

    largeQueue.size shouldBe 8
    largeQueue.peek shouldBe 100

    largeQueue.pop.size shouldBe 7
    largeQueue.pop.peek shouldBe 90

    largeQueue.pop.pop.size shouldBe 6
    largeQueue.pop.pop.peek shouldBe 80

    largeQueue.pop.pop.pop.size shouldBe 5
    largeQueue.pop.pop.pop.peek shouldBe 70

    largeQueue.pop.pop.pop.pop.size shouldBe 4
    largeQueue.pop.pop.pop.pop.peek shouldBe 60

    largeQueue.pop.pop.pop.pop.pop.size shouldBe 3
    largeQueue.pop.pop.pop.pop.pop.peek shouldBe 50

    largeQueue.pop.pop.pop.pop.pop.pop.size shouldBe 2
    largeQueue.pop.pop.pop.pop.pop.pop.peek shouldBe 40

    largeQueue.pop.pop.pop.pop.pop.pop.pop.size shouldBe 1
    largeQueue.pop.pop.pop.pop.pop.pop.pop.peek shouldBe 30

    largeQueue.pop.pop.pop.pop.pop.pop.pop.pop.isEmpty shouldBe true
  }
}
