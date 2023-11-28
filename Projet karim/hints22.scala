import scala.io.Source

object NonogramSolver {
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

  def hints(A: List[List[Boolean]]): (List[List[Int]], List[List[Int]]) = {
    val rowIndices = calculateRowIndices(A)
    val colIndices = calculateColIndices(A)
    
    (rowIndices, colIndices)
  }

  def readMatrix(filePath: String): List[List[Boolean]] = {
    val fileLines = Source.fromFile(filePath).getLines().toList
    if (fileLines.isEmpty) {
      throw new RuntimeException("Le fichier est vide \n ")
      sys.exit(0)

    }
  
    val referenceLineLength = fileLines.headOption.map(_.split(" ").length).getOrElse(0)
  
    if (fileLines.exists(line => line.split(" ").length != referenceLineLength)) {
      throw new RuntimeException("Les lignes du fichier n'ont pas la même longueur \n")
      sys.exit(0)

    }

    fileLines.map { line =>
      line.split(" ").map {
        case "X" => false
        case "O" => true
        case _ => {throw new RuntimeException("Le fichier contient des éléments qui ne sont ni X ni O \n ") 
        sys.exit(0)}

      }.toList
    }.toList
  }

  def main(args: Array[String]): Unit = {
    val filePath = "matrice.txt" // Assurez-vous que le fichier est dans le même répertoire que le code ou spécifiez le chemin complet
    val resultMatrix = readMatrix(filePath)
    resultMatrix.foreach(row => println(row.map(b => if (b) "true" else "false").mkString(" ")))

    val (rowIndices, colIndices) = hints(resultMatrix)
    println("\nRow Indices: " + rowIndices.mkString(", "))
    println("Column Indices: " + colIndices.mkString(", "))
  }
}
