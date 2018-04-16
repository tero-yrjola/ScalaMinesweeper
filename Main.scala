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
    if (NoEmptyCells(board)){
      println("You won!")
    }
    else {
      val columns = board(0).size
      val rows = board.size
      val validChars = List.range('a', 'z').map(_.toString).take(rows)
      val validNumbers = List.range(1, columns+1).map(_.toString)
      val guess = IO.GetResp(
        "Guess a cell!",
        s"Make a guess from (a-${validChars.last})(1-${validNumbers.last}). For example 'b2'.",
        validChars, validNumbers)

      val newBoard = ClickCell(guess, board)
      StartGame(newBoard)
    }
  }

  def NoEmptyCells(board: List[List[Cell]]):Boolean = {
    val columns = board(0).size-1
    val rows = board.size-1
    for (column <- 0 to columns){
      for(row <- 0 to rows){
        if (board(row)(column).isInstanceOf[Empty]) return false
        }
      }
    return true
  }

  def ClickCell(guessInput: String, board: List[List[Cell]]):List[List[Cell]]={
    val guess = (guessInput(0).asDigit -10,        //'a' gives 10, 'b' gives 11 etc.
                 guessInput.substring(1).toInt -1)   //-1 because arrays start at 0 (and the printable board at 1)

    val newBoard = ShowCell(guess._1, guess._2, board)
    IO.PrintBoard(newBoard)
    return newBoard
  }

  def ShowCell(x: Int, y: Int, board: List[List[Cell]]): List[List[Cell]]={
    if (!IsWithinBoundaries(x, y, board)) return board
    val cell = board(x)(y)
    if (cell.clicked == true) return board

    if (IsMine(cell)) throw new Exception("Game over.")
    else {
      val hintValue = CountSurroundingMines(x, y, board)
      val newBoard = UpdateCellToHint(x, y, hintValue, board)
      
      if (hintValue == 0){
        return ShowSurroundingCells(x, y, 0, newBoard)
      }
      return newBoard
    }
    return board
  }

  def UpdateCellToHint(x: Int, y: Int, hintValue: Int, board: List[List[Cell]]): List[List[Cell]]={
    return board.updated(x, board(x).updated(y, Hint(true, hintValue)))
  }

  // we calculate the neighbours' coordinates (the offset from original cell) from index
  // [0] [1] [2]
  // [3] [o] [5]    o = original opened cell, that had a hintValue of 0
  // [6] [7] [8]  
  def ShowSurroundingCells(originalX: Int, originalY: Int, index: Int, board: List[List[Cell]]): List[List[Cell]]={
    if (index >= 9) return board
    if (index == 4) return ShowSurroundingCells(originalX, originalY, index +1, board)

    val xOffset = index/3 % 3 -1
    val yOffset = index % 3 -1
    val xToOpen = originalX + xOffset
    val yToOpen = originalY + yOffset
    
    if (IsWithinBoundaries(xToOpen, yToOpen, board)) {
      if(board(xToOpen)(yToOpen).clicked == false){
        val hintValue = CountSurroundingMines(xToOpen, yToOpen, board)
        if (hintValue == 0){
          val newBoard = ShowCell(originalX,originalY+1,
                         ShowCell(originalX,originalY-1,
                         ShowCell(originalX+1,originalY,
                         ShowCell(originalX-1,originalY,
                         ShowCell(originalX+1,originalY+1,
                         ShowCell(originalX-1,originalY+1,
                         ShowCell(originalX+1,originalY-1,
                         ShowCell(originalX-1,originalY-1,board))))))))

          return newBoard
        }
        return ShowSurroundingCells(originalX, originalY, index +1, UpdateCellToHint(xToOpen, yToOpen, hintValue, board))
      }
    }
    return ShowSurroundingCells(originalX, originalY, index +1, board)
  }

  def CountSurroundingMines(clickedCellX: Int, clickedCellY: Int, board: List[List[Cell]]): Int={
    var numberOfMines = 0
    for (yOffset <- -1 to 1){
      if (yOffset != 0){
        for (xOffset <- -1 to 1){
          if (IsMineAndWithinBoundaries(clickedCellX + xOffset, clickedCellY + yOffset, board)) numberOfMines += 1
          }
        } else {
          if (IsMineAndWithinBoundaries(clickedCellX-1, clickedCellY, board)) numberOfMines += 1
          if (IsMineAndWithinBoundaries(clickedCellX+1, clickedCellY, board)) numberOfMines += 1
        }
        }
        return numberOfMines
      }

  def IsMineAndWithinBoundaries(x: Int, y: Int, board: List[List[Cell]]): Boolean ={
      if (IsWithinBoundaries(x, y, board))
      return IsMine(board(x)(y))
    
    return false
  }

  def IsWithinBoundaries(x: Int, y: Int, board: List[List[Cell]]): Boolean ={
    if (x < board.size && x >= 0){
      if (y < board(0).size && y >= 0)
      return true
    } 
    return false
   }
  def IsMine(cell: Cell)={
    cell match{
      case _: Mine => true
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