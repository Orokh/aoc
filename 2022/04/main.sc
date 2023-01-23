import scala.io.{BufferedSource, Source}


def readInput(file: String): Iterator[((Int, Int),(Int,Int))] =
  val source: BufferedSource = Source.fromFile(file)
  val data = List.newBuilder[String]

  for case s"$i-$j,$x-$y" <- source.getLines yield
    ((i.toInt,j.toInt),(x.toInt,y.toInt))

def isRangeIncluded(pairs: ((Int, Int),(Int, Int))): Boolean =
  val r1 = pairs(0)
  val r2 = pairs(1)
  r1(0) <= r2(0) & r1(1) >= r2(1) | r2(0) <= r1(0) & r2(1) >= r1(1)

def isRangeOverlap(pairs: ((Int, Int), (Int, Int))): Boolean =
  val r1 = pairs(0)
  val r2 = pairs(1)
  //(Start1 <= End2) and (End1 >= Start2)
  r1(0) <= r2(1) & r1(1) >= r2(0)

@main def process(): Unit =
  val data = readInput("input.txt")
  println(data.filter(isRangeIncluded).length)
  println(data.filter(isRangeOverlap).length)
