import scala.util.matching.Regex
import scala.collection.mutable.Stack
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

def buildStacks(stacks: List[String], nbStacks: Int): List[Stack[Char]]=
  val result: List[Stack[Char]] = List.fill(nbStacks)(Stack())

  for (stack <- stacks)
  do
      for (i <- 0 to nbStacks - 1)
      do
      breakable {
          val tgt = (1 + i * 4)
          if (tgt >= stack.length) break

          val elt = stack(tgt)
          if (elt != ' ')
            result(i).push(elt)
        }

  println()

  result.map(s => s.reverse)

def applyMove(stacks: List[Stack[Char]], move: String): Unit =
  val movePattern: Regex = "move ([0-9]+) from ([0-9]+) to ([0-9]+)".r

  move match
    case movePattern(nb, src, tgt) =>
      for (i <- 1 to nb.toInt)
      do
        stacks(tgt.toInt - 1).push(stacks(src.toInt - 1).pop)


def getResult(stacks: List[Stack[Char]]): String =
  stacks.map(m => m.pop).mkString

@main def process(): Unit =
  val data = readInput("input.txt")

  val (stacksDef, nbStacks, moves) = splitInputData(data)
  val stacks = buildStacks(stacksDef, nbStacks)

  moves.foreach(m => applyMove(stacks, m))

  println(getResult(stacks))

