import scala.io.Source


object NonogramSolver {
  def calculateRowIndices(matrix: Array[Array[Boolean]]): List[List[Int]] = {
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

  def calculateColIndices(matrix: Array[Array[Boolean]]): List[List[Int]] = {
    val numRows = matrix.length
    val numCols = matrix.head.length

    (0 until numCols).map { col =>
      (0 until numRows).foldLeft((List[Int](), 0)) { case ((indices, count), row) =>
        if (matrix(row)(col)) {
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
    val rowIndices = calculateRowIndices(A)
    val colIndices = calculateColIndices(A)
    
    (rowIndices, colIndices)
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

  

  def main(args: Array[String]): Unit = {
    val filePath = "matrice.txt" // Assurez-vous que le fichier est dans le même répertoire que le code ou spécifiez le chemin complet
    val resultMatrix = readMatrix(filePath)
    resultMatrix.foreach(row => println(row.map(b => if (b) "true" else "false").mkString(" ")))

    

    val (rowIndices, colIndices) = hints(resultMatrix)
    println("\nRow Indices: " + rowIndices.mkString(", "))
    println("Column Indices: " + colIndices.mkString(", "))
  }}




