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
    var rows = IO.getresp(
          "How many rows? (12-20)",
          "Please enter a number from 12 to 20.",
          List("12", "13", "14", "15", "16", "17", "18", "19", "20")).toInt
    
    var columns = IO.getresp(
          "How many columns? (12-20)",
          "Please enter a number from 12 to 20.",
          List("12", "13", "14", "15", "16", "17", "18", "19", "20")).toInt

    var maxBombs = rows * columns -1
    var allowedBombAmountList = List.range(1, maxBombs).map(_.toString)

    var bombs = IO.getresp(
          s"How many bombs? (12-$maxBombs)",
          s"Please enter a number from 1 to $maxBombs.",
          allowedBombAmountList).toInt     
  }
}