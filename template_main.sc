import scala.io.{BufferedSource, Source}


def readInput(file: String): List[Inventory] =
  val source: BufferedSource = Source.fromFile(file)
  val data = List.newBuilder[String]

  for (line <- source.getLines)
    data += Inventory(math.floor(index/3).toInt, line)

  data.result()

@main def process(): Unit =
  val data = readInput("test.txt")
  println(data)
