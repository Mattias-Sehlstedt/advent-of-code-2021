package Day_Four.Scala

import scala.io.Source

object Day_Four extends App {
  def bingoSetup(numbers: List[String]) : (List[(List[(Int, Boolean, Int, Int)], Int)], List[Int]) = {
    val numberDraw = numbers.head.split("\\,").map(_.toInt).toList
    val boards = numbers.tail
      .zipWithIndex
      .filter { case (_, i) => i % 6 != 0 }
      .map { case (e, _) => e }
      .zipWithIndex
      .groupBy(_._2/5)
      .map { case (_, v) => v }
      .map(_.map(x => x._1))
      .map(_.mkString(",").replace(",", " "))
      .toList
      .map(x => createBingoBoard(x))

    (boards.map((_, 0)), numberDraw)
  }

  def createBingoBoard(board: String) : List[(Int, Boolean, Int, Int)] = {
    board.split("\\s+")
      .filter(!_.isEmpty)
      .map(_.toInt)
      .zipWithIndex.map{ case (v, i) => (v, false, i % 5, i / 5)}
      .toList
  }

  def updateBoard(board: (List[(Int, Boolean, Int, Int)], Int), number: Int) : List[(Int, Boolean, Int, Int)] = {
    board._1.map(x => if (x._1 == number && board._2 == 0) (x._1, true, x._3, x._4) else x)
  }

  def updateBingoStatus(board: (List[(Int, Boolean, Int, Int)], Int), draw : Int) : (List[(Int, Boolean, Int, Int)], Int) = {
    if (board._2 != 0) {
      return board
    }
    else if (rowBingo(board._1) || colBingo(board._1)) {
      return (board._1, draw)
    }
    board
  }

  def rowBingo(board: List[(Int, Boolean, Int, Int)]) : Boolean = {
    board.map(x => (x._3, x._2)).groupBy(_._1).map{ case (_, v) => v}.map(_.map{ case (_, v) => v}).map(_.forall(_ == true)).exists(_ == true)
  }

  def colBingo(board: List[(Int, Boolean, Int, Int)]) : Boolean = {
    board.map(x => (x._4, x._2)).groupBy(_._1).map{ case (_, v) => v}.map(_.map{ case (_, v) => v}).map(_.forall(_ == true)).exists(_ == true)
  }

  def playBingo(boards: List[(List[(Int, Boolean, Int, Int)], Int)], numbers: List[Int], draw: Int = 1) : List[(List[(Int, Boolean, Int, Int)], Int)] = {
    if(numbers.isEmpty) {
      boards
    }
    else {
      playBingo(boards.map(x => updateBingoStatus((updateBoard(x, numbers.head), x._2), draw)), numbers.tail, draw + 1)
    }
  }

  def earliestWinningBoardValue(boards: List[(List[(Int, Boolean, Int, Int)], Int)], numbers: List[Int]) : Int = {
    val board = playBingo(boards, numbers).minBy(x => x._2)
    boardValue(board._1) * numbers(board._2 - 1)
  }

  def latestWinningBoardValue(boards: List[(List[(Int, Boolean, Int, Int)], Int)], numbers: List[Int]) : Int = {
    val board = playBingo(boards, numbers).maxBy(x => x._2)
    boardValue(board._1) * numbers(board._2 - 1)
  }

  def boardValue(board: List[(Int, Boolean, Int, Int)]) : Int = {
    board.filter(!_._2).map(_._1).sum
  }

  val bufferedSource = Source.fromFile("src/Day_Four/input.txt")
  val lines = bufferedSource.getLines.toList
  bufferedSource.close

  println(earliestWinningBoardValue(bingoSetup(lines)._1, bingoSetup(lines)._2))
  println(latestWinningBoardValue(bingoSetup(lines)._1, bingoSetup(lines)._2))
}
