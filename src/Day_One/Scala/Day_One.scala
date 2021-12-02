package Day_One.Scala

import scala.collection.mutable.ListBuffer
import scala.io.Source

object Day_One extends App {
  def depthCalc(depth: List[Int]) : Int = {
    var n = 0
    for( x <- 1 until depth.length ){
      if (depth(x - 1) < depth(x)) n += 1
    }
    n
  }

  def depthSlidingWindowCalc(depth: List[Int]) : Int = {
    var slidingValues = new ListBuffer[Int]()
    var slidingWindow = 0
    for( x <- 0 to depth.length - 3 ){
      slidingWindow = depth(x) + depth(x + 1) + depth(x + 2)
      slidingValues += slidingWindow
    }
    depthCalc(slidingValues.toList)
  }

  val bufferedSource = Source.fromFile("input.txt")
  val lines = bufferedSource.getLines.toList.map(_.toInt)
  bufferedSource.close

  println(depthCalc(lines))
  println(depthSlidingWindowCalc(lines))
}
