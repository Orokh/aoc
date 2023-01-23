import scala.io.Source
import scala.util.Try


def parseInput(file: String): List[Int] =
  val bufferedSource = Source.fromFile(file)

  var calories = 0
  val inventories = List.newBuilder[Int]

  for (line <- bufferedSource.getLines)
    if (line.isBlank())
      inventories += calories
      calories = 0
    else
      calories += line.toInt

  bufferedSource.close()

  inventories.result()

def getTopInventories(inv: List[Int], n: Int): Int =
  inv
    .sorted(Ordering.Int.reverse)
    .take(n)
    .reduce((x, y) => x+y)

@main def process(): Unit =
  val inventories = parseInput("input.txt")
  
  println(getTopInventories(inventories, 1))
  println(getTopInventories(inventories, 3))
  
