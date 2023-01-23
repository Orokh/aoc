import java.awt.print.Paper
import scala.io.{BufferedSource, Source}
import Play.*

enum Play:
  case Rock, Paper, Scissors

  def winsAgainst: Play = fromOrdinal((ordinal + 2) % 3)
  def losesAgainst: Play = fromOrdinal((ordinal + 1) % 3)

def getOpponentAction(opponent_code: String): Play =
  opponent_code match
    // opponent
    case "A" => Play.Rock
    case "B" => Play.Paper
    case "C" => Play.Scissors
 
def getPlayerActionV1(opponent: Play, player_code: String): Play =
  player_code match
    case "X" => Play.Rock
    case "Y" => Play.Paper
    case "Z" => Play.Scissors

def getPlayerActionV2(opponent: Play, player_code: String): Play =
  player_code match
    case "X" => opponent.winsAgainst 
    case "Y" => opponent 
    case "Z" => opponent.losesAgainst

def parseInput(file: String, parsePlayerVersion: (Play, String) => Play): Int =
  val bufferedInput: BufferedSource = Source.fromFile(file)

  var scores: Int = 0

  for (line <- bufferedInput.getLines())
    val codes = line.split(" ")
    val opponent = getOpponentAction(codes(0))
    val player = parsePlayerVersion(opponent, codes(1))
    scores += getRoundScore(opponent, player)

  scores

def getRoundScore(opponent: Play, player: Play): Int =
  var result = player.ordinal + 1

  if (opponent == player)
    // draw
    result += 3
  else if (opponent == player.winsAgainst)
    // win
    result += 6
  else
    // lose
    result += 0

  result

@main def process(): Unit =
  val roundsV1 = parseInput("input.txt", getPlayerActionV1)
  val roundsV2 = parseInput("input.txt", getPlayerActionV2)
  println(roundsV1)
  println(roundsV2)
