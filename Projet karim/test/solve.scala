import scala.io.Source
import scala.annotation.tailrec
import MatrixProcessor._
import Gpossib._


object solver {

  type Row = List[Int]
  type Col = List[Int]
  type Board = List[List[Boolean]]




 def solve(rowHints: List[Row], colHints: List[Col]): Option[Board] = {
  val numRows = rowHints.length
  val numCols = colHints.length

  
  val initialBoard = generateBoardCombinations(numCols, rowHints)

  def solveRec(board: Board, rowIndex: Int): Option[Board] = {
    if (rowIndex == numRows) {
      if (verif(board, rowHints, colHints)) {
        println("Solution trouvée :")
        draw(board)
        Some(board)
      } else {
        None
      }
    } else {
      val rowPossibilities = generatePossibilities(numCols, rowHints(rowIndex))
      val updatedBoard = rowPossibilities.foldLeft(Option.empty[Board]) { (acc, possibility) =>
        acc.orElse {
          val newBoard = updateBoardRow(board, rowIndex, possibility)
          println(s"Board actuel pour la ligne $rowIndex :")
          draw(newBoard)
          if (verif(newBoard, rowHints, colHints)) {
            println("Solution trouvée : \n")
            draw(newBoard)
            Some(newBoard)
          } else {
            solveRec(newBoard, rowIndex + 1)
          }
        }
      }
      updatedBoard
    }
  }

  solveRec(initialBoard, 0)
 }




  def generateBoardCombinations(size: Int, rowHints: List[Row]): Board = {
    val numRows = rowHints.size
    val numCols = size
    val emptyBoard: Board = List.fill(numRows)(List.fill(numCols)(false))
    draw(emptyBoard)
    val rowPossibilities = rowHints.map(hints => generatePossibilities(numCols, hints).head) // Tablezu qui contient les première possibilité

    rowPossibilities.zipWithIndex.foldLeft(emptyBoard) { //Ecrit rowpossibilities dans 
      case (board, (possibility, rowIndex)) =>
      updateBoardRow(board, rowIndex, possibility)
    }
  }

  // Fonction pour mettre à jour une ligne de la matrice avec une possibilité donnée
  def updateBoardRow(board: Board, rowIndex: Int, rowPossibility: List[Int]): Board = {
    board.zipWithIndex.map {
      case (row, index) =>
        if (index == rowIndex) {
          row.zipWithIndex.map {
            case (_, colIndex) =>
              rowPossibility(colIndex) == 1
          }
        } else {
          row
        }
    }
  }


    def draw(board: Board): Unit = {
  board.foreach { row =>
    row.foreach { cell =>
      print(if (cell) "■ " else "□ ")
    }
    println()
  }
  }

  def verif(board: Board, rowHints: List[Row], colHints: List[Col]): Boolean = {
    val colind = calculateColIndices(board)

    val rowind = calculateRowIndices(board)
 
    rowind.equals(rowHints) && colind.equals(colHints)
  }




  def main(args: Array[String]): Unit = {


val rowHints: List[Row] = List(List(2), List(5),List(5),List(1),List(1,1),List(1,1,1),List(1,2),List(4),List(2),List(5))

val colHints: List[Col] = List(List(2,3,1), List(3,1,1), List(3,2,1,1),List(2,4),List(3,5))


val numRows = rowHints.length
val numCols = colHints.length

solve(rowHints,colHints)

}
}

  


