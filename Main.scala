import java.util.Random
import IO._

abstract class Cell(isValueShown: Boolean)
    {
    val clicked: Boolean = isValueShown
    }

case class Mine(override val clicked: Boolean) extends Cell(clicked)
case class Empty(override val clicked: Boolean) extends Cell(clicked)

object Main {
  def main(args: Array[String]) {
    val rows = IO.GetResp(
          "How many rows? (12-20)",
          "Please enter a number from 12 to 20.",
          List("12", "13", "14", "15", "16", "17", "18", "19", "20")).toInt
    
    val columns = IO.GetResp(
          "How many columns? (12-20)",
          "Please enter a number from 12 to 20.",
          List("12", "13", "14", "15", "16", "17", "18", "19", "20")).toInt

    val maxBombs = rows * columns -1
    val allowedBombAmountList = List.range(1, maxBombs+1).map(_.toString)

    val bombs = IO.GetResp(
          s"How many bombs? (12-$maxBombs)",
          s"Please enter a number from 1 to $maxBombs.",
          allowedBombAmountList).toInt

    val board = GenerateNewBoard(rows, columns, bombs)
    PrintBoard(board)
  }
  def GenerateNewBoard(rows: Int, columns: Int, bombs: Int): List[List[Cell]] = {
    val board: List[List[Cell]] = List.tabulate(rows)(_ => List.tabulate(columns)(_ => new Empty(false)))
    var bombsLeft = bombs
    val rng = new scala.util.Random
    val boardSize = rows*columns
    var counter = 0
    for (rowIndex <- 0 to rows-1; columnIndex <- 0 to columns-1){
      if(rng.nextInt(boardSize - counter) < bombsLeft){
        val newBoard = putBomb(rowIndex, columnIndex, board)
        bombsLeft -= 1;
        if (counter >= boardSize -1) newBoard
      }
      counter += 1
    }
  }

  def putBomb(x: Int, y: Int, board: List[List[Cell]]):List[List[Cell]] = {
    val newBoard = board.updated(x, board(x).updated(y, Mine(false)))
  }

  def PrintBoard(board: List[List[Cell]]){
    for(i <- 0 to board.size -1){
      for(j <- 0 to board(0).size -1){
        print(board(i)(j))
        print(" ")
        }
      println()
    }
  }
}