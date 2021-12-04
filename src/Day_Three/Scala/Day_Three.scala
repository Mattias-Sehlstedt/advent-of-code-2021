package Day_Three.Scala

import scala.io.Source

object Day_Three extends App {
  def powerConsumption(diag: List[String]) : Int = {
    val epsilon = diag.flatMap(_.zipWithIndex)
      .groupBy{case (_, idx) => idx}
      .map{case (k, v) => if (v.map(_._1.asDigit).sum < v.length/2) (k, 0) else (k, 1)}.toList
    val gamma = diag.flatMap(_.zipWithIndex)
      .groupBy{case (_, idx) => idx}
      .map{case (k, v) => if (v.map(_._1.asDigit).sum > v.length/2) (k, 0) else (k, 1)}.toList
    binToDec(epsilon) * binToDec(gamma)
  }

  def oxygenGeneratorRating(diag: List[String], index: Int = 0) : Int = {
    if(diag.length == 1) {
      binToDecString(diag.head)
    }
    else {
      val numberOfOnes = diag.flatMap(_.zipWithIndex)
        .groupBy{case (_, idx) => idx}
        .map{case (k, v) => (k, v.map(_._1.asDigit).sum)}.get(index)
      oxygenGeneratorRating(diag.filter(x => x.charAt(index) == (if (numberOfOnes.get >= diag.length - numberOfOnes.get) '1' else '0')), index + 1)
    }
  }

  def co2ScrubberRating(diag: List[String], index: Int = 0) : Int = {
    if(diag.length == 1) {
      binToDecString(diag.head)
    }
    else {
      val numberOfOnes = diag.flatMap(_.zipWithIndex)
        .groupBy{case (_, idx) => idx}
        .map{case (k, v) => (k, v.map(_._1.asDigit).sum)}.get(index)
      co2ScrubberRating(diag.filter(x => x.charAt(index) == (if (numberOfOnes.get < diag.length - numberOfOnes.get) '1' else '0')), index + 1)
    }
  }

  def binToDec(value: List[(Int, Int)]) : Int = {
    value.map(v => v._2 * math.pow(2, (value.length - 1) - v._1)).sum.toInt
  }

  def binToDecString(value: String) : Int = {
    value.zipWithIndex.map(c => c._1.asDigit * math.pow(2, (value.length -1) - c._2)).sum.toInt
  }

  val bufferedSource = Source.fromFile("src/Day_Three/input.txt")
  val lines = bufferedSource.getLines.toList
  bufferedSource.close

  println(powerConsumption(lines))
  println(oxygenGeneratorRating(lines) * co2ScrubberRating(lines))
}
