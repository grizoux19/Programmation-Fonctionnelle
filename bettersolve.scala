import scala.io.Source
import scala.annotation.tailrec


object solver {

  type Row = List[Int]
  type Col = List[Int]
  type Board = List[List[Boolean]]

def solve(rowHints: List[Row], colHints: List[Col]): Option[Board] = {
  val numRows = rowHints.length
  val numCols = colHints.length

  val matrix : Array[Array[Int]]=Array.ofDim[Int](numRows,numCols)
  
  val filledmatrix: List[List[Int]]=start(rowHints,colHints,matrix)

  val initialBoard = generateBoardCombinations(numCols, rowHints, filledmatrix)

  
 
  def solveRec(board: Board, rowIndex: Int): Option[Board] = {
    println("Je passe sans solveRec")
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
      val rowPossibilities = produceCombinations(numCols, rowHints(rowIndex),filledmatrix(rowIndex))
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




   def generateBoardCombinations(size: Int, rowHints: List[Row], filledmatrix: List[List[Int]]): Board = {
  val numRows = rowHints.size
  val numCols = size
    val emptyBoard: Board = List.fill(numRows)(List.fill(numCols)(false))
  draw(emptyBoard)

  val rowPossibilities = rowHints.zip(filledmatrix).map {
      case (hints, firstGridRow) =>
        produceCombinations(numCols, hints, firstGridRow).head
    }
  println("Je print row possibilities " + rowPossibilities) 
  println("\n")


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
      print(if (cell) "v " else "x ")
    }
    println()
  }
  //println()

  }


def start(rowIndices: List[List[Int]], colIndices: List[List[Int]], matrix: Array[Array[Int]]): List[List[Int]] = {

 //Fonction pour remplir les hint plus grand que la moitié
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
                          matrix(i)(j+1) = 0
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
                          matrix(j+1)(i) = 0
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
        matrix.map(_.toList).toList
    }



  def verif(board: Board, rowHints: List[Row], colHints: List[Col]): Boolean = {
    val colind = calculateColIndices(board)

    val rowind = calculateRowIndices(board)
 
    rowind.equals(rowHints) && colind.equals(colHints)
  }
    def calculateRowIndices(matrix: List[List[Boolean]]): List[List[Int]] = {
    matrix.map(row => {
      row.foldLeft((List[Int](), 0)) {
        case ((indices, count), cell) =>
          if (cell) {
            (indices, count + 1)
          } else {
            if (count > 0) (indices :+ count, 0)
            else (indices, 0)
          }
      } match {
        case (indices, 0) => indices
        case (indices, count) => indices :+ count
      }
    }).toList
  }

  def calculateColIndices(matrix: List[List[Boolean]]): List[List[Int]] = {
    val numRows = matrix.length
    val numCols = matrix.headOption.map(_.length).getOrElse(0)

    (0 until numCols).map { col =>
      (0 until numRows).foldLeft((List[Int](), 0)) { case ((indices, count), row) =>
        if (matrix.lift(row).flatMap(_.lift(col)).contains(true)) {
          (indices, count + 1)
        } else {
          if (count > 0) (indices :+ count, 0)
          else (indices, 0)
        }
      } match {
        case (indices, 0) => indices
        case (indices, count) => indices :+ count
      }
    }.toList
  }


  def produceCombinations( size: Int,line: List[Int], matrix: List[Int]): List[List[Int]] = {
        val solution: List[String] = combinationHelper(line, size)

        val allValues: List[List[Int]] = solution.map { elem =>
            elem.map(num => num.toInt - 48).toList
        }

        println("Je print allValues " + allValues)
        println("Je print la matrix " + matrix.toList)

        val onePositions = getOnePositions(matrix)

        val filteredValues = allValues.filter(combination => hasSamePositionsAsMatrix(combination, onePositions))

        filteredValues
    }

    def getOnePositions(matrix: List[Int]): List[Int] = {
        matrix.zipWithIndex.collect { case (value, index) if value == 1 => index }
    }

    def hasSamePositionsAsMatrix(lst: List[Int], positions: List[Int]): Boolean = {
        positions.forall(index => lst(index) == 1)
    }


    def combinationHelper(line: List[Int], size: Int): List[String] = {
        if (line.isEmpty) List("0" * size)
        else if (line.head > size) List()
        else {
            val starts: Int = size - line.head

            if (line.length == 1) {
            (0 to starts).map(i => "0" * i + "1" * line.head + "0" * (starts - i)).toList
            } else {
            (0 until size - line.head).flatMap { i =>
                combinationHelper(line.tail, starts - i - 1).map(sol => "0" * i + "1" * line.head + "0" + sol)
            }.toList
            }
        }
    }



  def main(args: Array[String]): Unit = {



val rowHints: List[Row] = List(List(2), List(5),List(5),List(1),List(1,1),List(1,1,1),List(1,2),List(4),List(2),List(5))

val colHints: List[Col] = List(List(2,3,1), List(3,1,1), List(3,2,1,1),List(2,4),List(3,5))




val numRows = rowHints.length
val numCols = colHints.length

solve(rowHints,colHints)

}
}