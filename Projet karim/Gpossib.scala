object Gpossib {
  
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



  def printPossibilities(possibilities: List[List[Int]]): Unit = {
    possibilities.foreach { row =>
      row.foreach(cell => print(cell + " "))
      println()
    }
  }



  def main(args: Array[String]): Unit = {
    /*println("Possibilités pour (10, List(1,1)):")
    val possibilities1 = generatePossibilities(10, List(1, 1))
    printPossibilities(possibilities1)
    
    println("\nPossibilités pour (5, List(1)):")
    val possibilities2 = generatePossibilities(5, List(1))
    printPossibilities(possibilities2)

    println("\nPossibilités pour (5, List(1,1,1)):")
    val possibilities3 = generatePossibilities(5, List(1, 1, 1))
    printPossibilities(possibilities3)

    println("\nPossibilités pour (15, List(3,1,1)):")
    val possibilities4 = generatePossibilities(15, List(3, 1, 1))
    printPossibilities(possibilities4)

    println("\nPossibilités pour (8, List(2,3)):")
    val possibilities5 = generatePossibilities(8, List(2, 3))
    printPossibilities(possibilities5)

    println("\nPossibilités pour (8, List(2,1,1)):")
    val possibilities6 = generatePossibilities(8, List(2, 1,1))
    printPossibilities(possibilities6)*/
  }
}
