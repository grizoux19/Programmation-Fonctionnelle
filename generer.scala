object NonogramHelper {

  /*def produceCombinations(line: List[Int], size: Int): List[List[Int]] = {
    val solution: List[String] = combinationHelper(line, size)

    val allValues: List[List[Int]] = solution.map { elem =>
      elem.map(num => num.toInt - 48).toList
    }

    allValues
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
  }*/

    def main(args: Array[String]): Unit = {
    // Exemple d'utilisation de la fonction avec une matrice carrée
    /*val matrix = List(
      List(true, true, true, true, false),
      List(false, true, false, true, false),
      List(true, true, false, true, true),
      List(false, true, false, true, false),
      List(true, false, true, false, true)
    )*/

    //println("Matrice : " + matrix.map(_.map(if (_) 1 else 0).mkString(" ")).mkString("\n"))
    //val (rowIndices, colIndices) = findNonogramIndices(matrix)
    //val rowIndices = calculateRowIndices(matrix)
    //val colIndices = calculateColIndices(matrix)

   //println("Indices des lignes : " + rowIndices)
    //println("Indices des colonnes : " + colIndices)


    //val colIndicess = List(List(10), List(10), List(2), List(2), List(10), List(10), List(2), List(2), List(10), List(10))
    //val rowIndicess = List(List(2,2,2), List(2,3,2), List(2,3,3), List(2,2,3), List(2,2,2), List(3,2,2), List(3,3,2), List(2,3,2), List(2,2,2), List(2,2,2))




    def produceCombinations(line: List[Int], size: Int, matrix: Array[Int]): List[List[Int]] = {
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

    def getOnePositions(matrix: Array[Int]): List[Int] = {
        matrix.zipWithIndex.collect { case (value, index) if value == 1 => index }.toList
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


def start(rowIndices: List[List[Int]], colIndices: List[List[Int]], matrix: List[List[Int]]): Boolean = {
  // ... (Votre code inchangé jusqu'à la première boucle for)

  for ((rowList, rowIndex) <- rowIndices.zipWithIndex) {
    val indicesSum = rowList.sum
    val indicesCases = indicesSum + (rowList.length - 1)
    val emptyCase = matrix.length - indicesCases

    if (emptyCase == 0) {
      var counter = 0
      var k = 0
      var skip = false
      matrix = matrix.updated(rowIndex, matrix(rowIndex).updated(0, 1))
      for (j <- 0 until matrix.length) {
        if (skip) {
          skip = false
        } else {
          matrix = matrix.updated(rowIndex, matrix(rowIndex).updated(j, 1))
          counter = counter + 1

          if (counter == rowList(k) && j + 1 != matrix.length) {
            matrix = matrix.updated(rowIndex, matrix(rowIndex).updated(j + 1, 2))
            counter = 0
            k = k + 1
            skip = true
          }
        }
      }
    } else {
      var counter = 0
      var index = 0

      if (rowList(counter) > emptyCase) {
        for (k <- counter until rowList(counter)) {
          if (k < emptyCase) {
            // matrix = matrix.updated(rowIndex, matrix(rowIndex).updated(k, 0))
            counter = counter + 1
          } else {
            matrix = matrix.updated(rowIndex, matrix(rowIndex).updated(k, 1))
            counter = counter + 1
          }
        }
      }
    }
  }

  // ... (Votre code inchangé jusqu'à la deuxième boucle for)

  for ((colList, colIndex) <- colIndices.zipWithIndex) {
    val indicesSum = colList.sum
    val indicesCases = indicesSum + (colList.length - 1)
    val emptyCase = matrix.length - indicesCases

    if (emptyCase == 0) {
      var counter = 0
      var k = 0
      var skip = false
      matrix = matrix.updated(0, matrix(0).updated(colIndex, 1))
      for (i <- 0 until matrix.length) {
        if (skip) {
          skip = false
        } else {
          matrix = matrix.updated(i, matrix(i).updated(colIndex, 1))
          counter = counter + 1

          if (counter == colList(k) && i + 1 != matrix.length) {
            matrix = matrix.updated(i + 1, matrix(i + 1).updated(colIndex, 2))
            counter = 0
            k = k + 1
            skip = true
          }
        }
      }
    } else {
      var counter = 0
      var index = 0

      if (colList(counter) > emptyCase) {
        for (k <- counter until colList(counter)) {
          if (k < emptyCase) {
            // matrix = matrix.updated(k, matrix(k).updated(colIndex, 0))
            counter = counter + 1
          } else {
            matrix = matrix.updated(k, matrix(k).updated(colIndex, 1))
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









    //val colIndicess = List(List(10,1), List(4), List(2), List(3), List(1),List(3), List(4), List(2), List(3), List(1),List(3), List(4), List(2), List(3), List(1))
    //val rowIndicess = List(List(2), List(2), List(4), List(3), List(2),List(3), List(4), List(2), List(3), List(1),List(3), List(4), List(2), List(3), List(1))
    
    val colIndicess = List(List(3), List(3), List(2), List(3), List(2))
    val rowIndicess = List(List(1), List(2), List(2,2), List(3), List(3))

    val matrix: List[List[Int]] = List.fill(5)(List.fill(5)(0))

    println("Je print la taille de rowIndices et colIndices" + rowIndicess.length + colIndicess.length)
    start(rowIndicess, colIndicess, matrix)
    //println("Solutionnnnn : " + boolean)

    /*for(i<- 0 until 5) {
        val combinations = produceCombinations(rowIndicess(i), 5, matrix(i))
        println(s"Combinations for the row $rowIndicess(i) of size 5:")
        combinations.foreach(combination => println(combination.mkString(" ")))
    }*/
    //val row = List(2)
    //val size = 5

    //val combinations = produceCombinations(row, size, matrix(1))

    /*println(s"Combinations for the row $row of size $size:")
    combinations.foreach(combination => println(combination.mkString(" ")))
    println("Je print combination " + combinations.mkString(" "))
    println("Je print combination " + combinations)*/
  }
}
