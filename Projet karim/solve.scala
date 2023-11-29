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

  val initialBoard = generateBoardCombinations(colHints, rowHints)

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
      val rowPossibilities = generatePossibilities(numCols, rowHints(rowIndex), firstGrid(rowIndex)) //Besoin de Gposs
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


  def generateBoardCombinations(colHints: List[Col], rowHints: List[Row]): Board = {
    val numRows = rowHints.size
    val numCols = colHints.size

    //val emptyBoard: Board = List.fill(numRows)(List.fill(numCols)(false))
    //draw(emptyBoard)

    val firstGrid = start(rowHints, colHints, Array.ofDim[Int](numRows, numCols))
    //firstGrid contient les premiers 1 qui ne peuvent pas bouger

    //val rowPossibilities = rowHints.map(hints => generatePossibilities(hints, numCols, firstGrid).head) //Besoin de Gposs
    val rowPossibilities = rowHints.zip(firstGrid).map {
      case (hints, firstGridRow) =>
        generatePossibilities(hints, numCols, firstGridRow).head
    }

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

    def start(rowIndices: List[List[Int]], colIndices: List[List[Int]], matrix: Array[Array[Int]]): Boolean = { //Fonction pour remplir les hint plus grand que la moitié
      for (i <- 0 until rowIndices.length) { //Pour chaque ligne
          val list = rowIndices(i) //Liste des indices de la lignes
          //println("Je print la liste : " + list.length)
          //println("Je print rowIndices : " + rowIndices(i))

          val IndicesSum = list.sum //Somme des indices de la ligne
          val IndicesCases = IndicesSum + (list.length - 1)
          val EmptyCase = rowIndices.length - IndicesCases //Nombre de cases vides dans la ligne

          //println("Je print la somme des indices : " + EmptyCase)

          if(EmptyCase == 0) {
              var counter = 0
              var k = 0
              var skip = false
              for (j <- 0 until rowIndices.length) {
                  if(skip) {
                    skip = false
                  } else {
                    matrix(i)(j) = 1
                    counter = counter + 1

                    if(counter == list(k) && j + 1 != rowIndices.length) {
                        matrix(i)(j+1) = 2
                        counter = 0
                        k = k + 1
                        skip = true
                    }
                  }
              }
          } else { //Vérifier que les éléments de la liste sont plus grand que EmptyCase, pour ça on itère sur tout le tableau, compteur sur la liste
              var counter = 0
              var index = 0

              if(list(counter) > EmptyCase) {
                  //val case = list(counter) - EmptyCase //Nombre de case à colorier -> 6 - 2 = 4 bloc à colorier

                  for(k <- counter until list(counter)) {
                    if(k < EmptyCase) {
                      //matrix(i)(k) = 0
                      counter = counter + 1
                    } else {
                      matrix(i)(k) = 1 
                      counter = counter + 1
                    } 
                  }
              }
          }
      }////

      for (i <- 0 until colIndices.length) { //Pour chaque ligne
          val list = colIndices(i) //Liste des indices de la lignes
          //println("Je print la liste : " + list.length)
          //println("Je print colIndices : " + colIndices(i))

          val IndicesSum = list.sum //Somme des indices de la ligne
          val IndicesCases = IndicesSum + (list.length - 1)
          val EmptyCase = colIndices.length - IndicesCases //Nombre de cases vides dans la ligne

          //println("Je print la somme des indices : " + EmptyCase)

          if(EmptyCase == 0) {
              var counter = 0
              var k = 0
              var skip = false
              for (j <- 0 until colIndices.length) {
                  if(skip) {
                    skip = false
                  } else {
                    matrix(j)(i) = 1
                    counter = counter + 1

                    if(counter == list(k) && j + 1 != colIndices.length) {
                        matrix(j+1)(i) = 2
                        counter = 0
                        k = k + 1
                        skip = true
                    }
                  }
              }
          } else { //Vérifier que les éléments de la liste sont plus grand que EmptyCase, pour ça on itère sur tout le tableau, compteur sur la liste
              var counter = 0
              var index = 0

              if(list(counter) > EmptyCase) {
                  //val case = list(counter) - EmptyCase //Nombre de case à colorier -> 6 - 2 = 4 bloc à colorier

                  for(k <- counter until list(counter)) {
                    if(k < EmptyCase) {
                      //matrix(i)(k) = 0
                      counter = counter + 1
                    } else {
                      matrix(k)(i) = 1 
                      counter = counter + 1
                    } 
                  }
              }
          }
      }
      println("J'essaye de print la matrice start:")
      for (i <- 0 until matrix.length) {
        for (j <- 0 until matrix(i).length) {
        print(matrix(i)(j) + " ")
        } 
        println()
      }
      true
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

  


