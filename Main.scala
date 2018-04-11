import java.util.Random
import IO._

abstract class Cell(isCellClicked: Boolean)
    {
    val clicked: Boolean = isCellClicked
    }

case class Mine(override val clicked: Boolean) extends Cell(clicked)
case class Empty(override val clicked: Boolean) extends Cell(clicked)
case class Hint(override val clicked: Boolean, val hint: Int) extends Cell(clicked) {
  val minesNear = hint
}

object Main {
  val allowedNumbers = List("4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20")
  def main(args: Array[String]) {
    val rows = IO.GetResp(
          "How many rows? (4-20)",
          "Please enter a number from 4 to 20.",
          allowedNumbers).toInt
    
    val columns = IO.GetResp(
          "How many columns? (4-20)",
          "Please enter a number from 4 to 20.",
          allowedNumbers).toInt

    val maxBombs = rows * columns -1
    val allowedBombAmountList = List.range(1, maxBombs+1).map(_.toString)

    val bombs = IO.GetResp(
          s"How many bombs? (1-$maxBombs)",
          s"Please enter a number from 1 to $maxBombs.",
          allowedBombAmountList).toInt

    val board:List[List[Cell]] = GenerateNewBoard(rows, columns, bombs)
    IO.PrintBoard(board)

    StartGame(board)
  }  

  def StartGame(board: List[List[Cell]]){
    val columns = board(0).size
    val rows = board.size
    val validChars = List.range('a', 'z').map(_.toString).take(rows)
    val validNumbers = List.range(1, columns+1).map(_.toString)
    while(true){
      val guess = IO.GetResp(
        "Guess a cell!",
        s"Make a guess from (a-${validChars.last})(1-${validNumbers.last}). For example 'b2'.",
        validChars, validNumbers)

        val newBoard = clickCell(guess, board)
      }
  }
  def ClickCell(guessInput: String, board: List[List[Cell]]):List[List[Cell]]={
    val guess = (guessInput(0).asDigit -10,        //'a' gives 10, 'b' gives 11 etc.
                 guessInput.substring(1).toInt -1)   //-1 because arrays start at 0 (and the printable board at 1)

    val newBoard = showCell(guess._1, guess._2, board)
    val newBoard2: List[List[Cell]] = board.updated(guess._1, board(guess._1).updated(guess._2, Hint(false,2)))
    IO.PrintBoard(newBoard2)
    return board
  }

  def ShowCell(x: Int, y: Int, board: List[List[Cell]]): List[List[Cell]]={
    val cell = board(x)(y)

    if (isBomb(cell)) throw new Exception("Game over.");

    return board
  }

  def IsBomb(cell: Cell)={
    cell match{
      case _: Mine => true;
      case _ => false
    }
  }
  def GenerateNewBoard(rows: Int, columns: Int, bombs: Int): List[List[Cell]] = {
    val emptyBoard: List[List[Cell]] = List.tabulate(rows)(_ => List.tabulate(columns)(_ => new Empty(false)))
    val gameBoard = PutBombs(0, 0, emptyBoard, bombs)
    gameBoard
  }

  def PutBombs(rowIndex: Int, columnIndex: Int, board: List[List[Cell]], bombs: Int):List[List[Cell]]={
    val boardHeight = board.size
    val boardWidth = board(0).size

    val handledCells = rowIndex + (columnIndex*(boardHeight))
    //exit condition
    if (bombs < 1){
      return board
    }
    val rng = new scala.util.Random    
    val newColumnIndex = if (rowIndex >= boardHeight-1) columnIndex +1 else columnIndex
    val newRowIndex = if (rowIndex >= boardHeight-1) 0 else rowIndex+1
    if (rng.nextInt(boardHeight*boardWidth - handledCells) < bombs){
      val newBoardWithOneMoreBomb = PutBomb(rowIndex, columnIndex, board)
      val newBoard = PutBombs(newRowIndex, newColumnIndex, newBoardWithOneMoreBomb, bombs-1)
      return newBoard
    } else {
      val newBoard = PutBombs(newRowIndex, newColumnIndex, board, bombs)
      return newBoard
    }
    board
  } 

  def PutBomb(x: Int, y: Int, board: List[List[Cell]]):List[List[Cell]] = {
    val newBoard: List[List[Cell]] = board.updated(x, board(x).updated(y, Mine(false)))
    return newBoard
  }


}