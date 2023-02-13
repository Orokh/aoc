import scala.util.matching.Regex
import scala.io.{BufferedSource, Source}
import util.control.Breaks._


def readInput(file: String): List[String] =
  val source: BufferedSource = Source.fromFile(file)
  val data = List.newBuilder[String]

  for (line <- source.getLines)
    data += line

  data.result()

def splitInputData(rawData: List[String]): (List[String], Int, List[String]) =
  val idxEmptyLine = rawData.indexOf("")
  val (stacks, rest) = rawData.splitAt(idxEmptyLine)

  val moves = rest.filter(elt => elt != "")
  val nbStacks = stacks.last.split("  ").last.trim().toInt

  (stacks.dropRight(1), nbStacks, moves)

def parseRow(row: String) =
  for i <- 0 to row.length by 4 yield
    if row(i+1) != ' ' then
      row(i+1)
    else
      ('_')

def buildStacks(stacks: List[String], nbStacks: Int): List[List[Char]]=
  val cleanedInput = stacks.map(parseRow(_))
  cleanedInput.transpose


//def applyMove(stacks: List[List[Char]], move: String): Unit =
//  val movePattern: Regex = "move ([0-9]+) from ([0-9]+) to ([0-9]+)".r
//
//  move match
//    case movePattern(nb, src, tgt) =>
//      for (i <- 1 to nb.toInt)
//      do
//        stacks(tgt.toInt - 1).push(stacks(src.toInt - 1).pop)

//def getResult(stacks: List[List[Char]]): String =
//  stacks.map(_.pop).mkString

@main def process(): Unit =
  val data = readInput("test.txt")

  val (stacksDef, nbStacks, moves) = splitInputData(data)
  val stacks = buildStacks(stacksDef, nbStacks)
  println(stacks)

 // moves.foreach(m => applyMove(stacks, m))

 // println(getResult(stacks))

