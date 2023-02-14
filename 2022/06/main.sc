import scala.io.{BufferedSource, Source}
import scala.collection.immutable.Queue
import scala.io.StdIn.readInt

def updateQueue(queue: Queue[Char], newLetter: Char): Queue[Char] =
  queue.dequeue(1).enqueue(newLetter)

def allElementsUnique(queue: Queue[Char]): Boolean =
  queue.toSet.size < queue.length


@main def process(): Unit =
  val source: BufferedSource = Source.fromFile("input.txt")

  println("Input size of message to check:")
  val inputLength = readInt()

  var currQueue: Queue[Char] = Queue(source.take(inputLength).toList: _*)
  var index: Int = inputLength
  var done: Boolean = false

  for (char <- source if char >= ' ' && !done)

    if (allElementsUnique(currQueue)) then
      index += 1
      currQueue = updateQueue(currQueue, char)
    else
      done = true
    
  println(currQueue)
  println(index)
