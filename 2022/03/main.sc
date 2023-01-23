import scala.io.{BufferedSource, Source}

final case class Inventory (
  group: Int,
  items: String
)

def readInput(file: String): List[Inventory] =
  val source: BufferedSource = Source.fromFile(file)
  val rucksacks = List.newBuilder[Inventory]
  var index: Int = 0

  for (line <- source.getLines)
    rucksacks += Inventory(math.floor(index/3).toInt, line)
    index += 1

  rucksacks.result()

def getCommonItem(input: String): Char =
  val len: Int = input.length()
  val (pack1, pack2) = input.splitAt(input.length() / 2)
  pack1.intersect(pack2).head

def getItemPriority(item: Char): Int =
//  a through z have priorities 1 through 26.
//  A through Z have priorities 27 through 52.
  if (item.toUpper == item)
    item.intValue - 38
  else
    item.intValue - 96

@main def process(): Unit = 
  val data = readInput("test.txt")

  // part1
  val commonItems = data.map(inv => getCommonItem(inv.items))
  val priorities = commonItems.map(getItemPriority)
  println(priorities.sum)

  // part2
  val badges = data.groupBy(_.group)
                   .map((group, invs) => (group, invs.map(inv => inv.items.toSet)))
                   .map((group, invs) => (group, invs.reduce((a,b) => a.intersect(b))))
                   .map((group, badge) => getItemPriority(badge.head))
                   .sum 
  println(badges)
    //.map(
    //.reduce((a,b) => a(1).toSet.intersect(b(1).toSet))
