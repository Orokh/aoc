import scala.annotation.tailrec
import scala.util.matching.Regex
import scala.io.{BufferedSource, Source}


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


def parseRow(row: String, nbStacks: Int) =
  for i <- 0 to nbStacks - 1 yield
    val pos = (1 + i*4)
    if pos >= row.length || row(pos) == ' ' then
      ('_')
    else
      row(pos)


def buildStacks(stacks: List[String], nbStacks: Int): List[List[Char]]=
  val cleanedInput = stacks.map(parseRow(_, nbStacks))
  cleanedInput.transpose.map(_.filter(_ != '_'))


def applyMove(stacks: List[List[Char]], move: String): List[List[Char]] =
  val movePattern: Regex = "move ([0-9]+) from ([0-9]+) to ([0-9]+)".r

  move match
    case movePattern(nb, src, tgt) =>
      val (moved, newSource) = stacks(src.toInt - 1).splitAt(nb.toInt)
      val newTarget = moved.reverse:::stacks(tgt.toInt - 1)

      stacks.updated(src.toInt - 1, newSource).updated(tgt.toInt - 1, newTarget)


def processMoves(stacks: List[List[Char]], moves: List[String]): List[List[Char]] =

  @tailrec def processMovesRec(state: List[List[Char]], moves: List[String]) : List[List[Char]]=
    if moves.isEmpty then
      state
    else
      val newState = applyMove(state, moves.head)
      processMovesRec(newState, moves.tail)

  processMovesRec(stacks, moves)


@main def process(): Unit =
  val data = readInput("input.txt")

  val (stacksDef, nbStacks, moves) = splitInputData(data)
  val stacks = buildStacks(stacksDef, nbStacks)

  val result1 = processMoves(stacks, moves)
  println(result1.map(_.head).mkString)
