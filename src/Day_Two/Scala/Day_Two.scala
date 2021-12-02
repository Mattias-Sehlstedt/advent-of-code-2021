package Day_Two.Scala

import scala.io.Source

object Day_Two extends App {
  val posRegex = """\w+\s(\d+)""".r

  def positionCalc(posChange: List[String]) : Int = {
    val up = posChange.filter(x => x.startsWith("up"))
    val down = posChange.filter(x => x.startsWith("down"))
    val forward = posChange.filter(x => x.startsWith("forward"))

    val ups = for (m <- posRegex.findAllMatchIn(up.mkString(", "))) yield m.group(1).toInt
    val downs = for (m <- posRegex.findAllMatchIn(down.mkString(", "))) yield m.group(1).toInt
    val forwards = for (m <- posRegex.findAllMatchIn(forward.mkString(", "))) yield m.group(1).toInt
    (downs.sum - ups.sum) * forwards.sum
  }

  def aimCalc(posChange: List[String]) : Int = {
    var depth = 0
    var aim = 0
    var horizontalPosition = 0
    for (movement <- posChange) {
      val dir = movement.split(" ")(0)
      val value = movement.split(" ")(1).toInt
      if(dir == "up") {
        aim -= value
      }
      else if(dir == "down") {
        aim += value
      }
      else if(dir == "forward") {
        horizontalPosition += value
        depth += aim * value
      }
    }
    depth * horizontalPosition
  }

  val bufferedSource = Source.fromFile("src/Day_Two/input.txt")
  val lines = bufferedSource.getLines.toList
  bufferedSource.close

  println(positionCalc(lines))
  println(aimCalc(lines))
}
