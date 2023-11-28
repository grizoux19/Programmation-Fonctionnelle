import scala.io.Source



sealed trait Indices
case class RowIndices(indices: List[List[Int]]) extends Indices
case class ColIndices(indices: List[List[Int]]) extends Indices

object MatrixProcessor {
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


  def hints(A: Array[Array[Boolean]]): (List[List[Int]], List[List[Int]]) = {

    val rowIndices = calculateRowIndices(arrayToList(A))


    val colIndices = calculateColIndices(arrayToList(A))

    (rowIndices, colIndices)
  }

   def arrayToList[T](matrix: Array[Array[T]]): List[List[T]] = {
  matrix.map(_.toList).toList
}


  def readMatrix(filePath: String): Array[Array[Boolean]] = {
    val fileLines = Source.fromFile(filePath).getLines().toArray
    if (fileLines.isEmpty) {
      throw new RuntimeException("Le fichier est vide \n ")
    }

    val referenceLineLength = fileLines.head.split(" ").length

    if (fileLines.exists(line => line.split(" ").length != referenceLineLength)) {
      throw new RuntimeException("Les lignes du fichier n'ont pas la même longueur \n")
    }

    fileLines.map { line =>
      line.split(" ").map {
        case "X" => false
        case "O" => true
        case _ => throw new RuntimeException("Le fichier contient des éléments qui ne sont ni X ni O \n ")
      }
    }
  }

  def hintsFromFile(filePath: String): (List[List[Int]], List[List[Int]]) = {
    val matrix = readMatrix(filePath)
    hints(matrix)
  }
}

object Main extends App {
  val filePath = "matrice.txt" 

  try {
    val resultMatrix = MatrixProcessor.readMatrix(filePath)
  resultMatrix.foreach(row => println(row.map(b => if (b) "true" else "false").mkString(" ")))
    println("\n")
    

    val (rowIndices, colIndices) = MatrixProcessor.hintsFromFile(filePath)
    println(s"Indices des lignes: $rowIndices")
    println(s"Indices des colonnes: $colIndices")
  } catch {
    case e: RuntimeException => println(s"Erreur: ${e.getMessage}")
  }
}
