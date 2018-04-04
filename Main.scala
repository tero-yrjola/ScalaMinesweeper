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

    val board:List[List[Cell]] = GenerateNewBoard(rows, columns, bombs)
    PrintBoard(board)
  }
  def GenerateNewBoard(rows: Int, columns: Int, bombs: Int): List[List[Cell]] = {
    val emptyBoard: List[List[Cell]] = List.tabulate(rows)(_ => List.tabulate(columns)(_ => new Empty(false)))
    // var bombsLeft = bombs
    // val rng = new scala.util.Random

    val gameBoard = PutBombs(0, 0, emptyBoard, bombs)
    // for (rowIndex <- 0 to rows-1; columnIndex <- 0 to columns-1){
    //   if(rng.nextInt(boardSize - counter) < bombsLeft){
    //     newBoard = putBomb(rowIndex, columnIndex, board)
    //     bombsLeft -= 1;
    //     if (bombsLeft == 0)
    //   }
    //   counter += 1
    // }
    gameBoard
  }

  def PutBombs(rowIndex: Int, columnIndex: Int, board: List[List[Cell]], bombs: Int):List[List[Cell]]={
    val boardHeight = board.size
    val boardWidth = board(0).size
    if (rowIndex == boardHeight-1 && columnIndex == boardWidth) return board

    val rng = new scala.util.Random
    val handledCells = rowIndex+1 + (columnIndex*(boardWidth))
    val newColumnIndex = if (rowIndex >= boardHeight-1) columnIndex +1 else columnIndex
    val newRowIndex = if (rowIndex >= boardHeight-1) 0 else rowIndex+1

    if (bombs > 0)
      if (rng.nextInt(boardHeight*boardWidth - handledCells) < bombs){
        val newBoard:List[List[Cell]] = PutBomb(newRowIndex, newColumnIndex, board)
        val newBoard2 = PutBombs(newRowIndex, newColumnIndex, newBoard, bombs-1)
        return newBoard2
      } else {
        val newBoard:List[List[Cell]] = board
        val newBoard2 = PutBombs(newRowIndex, newColumnIndex, newBoard, bombs)
        return newBoard2
}
    board
  } 

  def PutBomb(x: Int, y: Int, board: List[List[Cell]]):List[List[Cell]] = {
    val newBoard: List[List[Cell]] = board.updated(x, board(x).updated(y, Mine(false)))
    return newBoard
  }

  def PrintBoard(board: List[List[Cell]]){
    for(i <- 0 to board.size -1){
      for(j <- 0 to board(0).size -1){
        print(GetPrintable(board(i)(j)))
        print(" ")
        }
      println()
    }
  }

  def GetPrintable[T](typeOfCell: T) = typeOfCell match {
  case _: Empty    => "[ ]"
  case _: Mine => "[x]"
  case _         => "Unknown"
}
}