import scala.io.Source
import scala.annotation.tailrec
import MatrixProcessor._
import Gpossib._


object solver {

  type Row = List[Int]
  type Col = List[Int]
  type Board = List[List[Boolean]]

  def solve(colHints: List[Row], rowHints: List[Col]): Option[Board] = {
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
          println("pas de solution trouvée")
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
    val rowPossibilities = rowHints.map(hints => generatePossibilities(numCols, hints).head)

    rowPossibilities.zipWithIndex.foldLeft(emptyBoard) {
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

  val colHints: List[Row] = List(List(2,2,2), List(1,6), List(1,3,2), List(1,3,2), List(1,2,2), List(2,3,2), List(4,1), List(6,1), List(3,4), List(5,1))
  val rowHints : List[Row]= List(List(6), List(1,1), List(4), List(4), List(4,5), List(8,1), List(5,3), List(2,3), List(6,2), List(5,2))

  val numRows = rowHints.length
  val numCols = colHints.length

  println(numRows)
  println(numCols )
  solve(colHints,rowHints)

  val rowPossibilities = rowHints.map(hints => generatePossibilities(numCols, hints))
  println(rowPossibilities(0)(0))

  }
}

  


