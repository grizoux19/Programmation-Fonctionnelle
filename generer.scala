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




    def produceCombinations(line: List[Int], size: Int, matrix: List[Int]): List[List[Int]] = {
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




    //val colIndicess = List(List(10,1), List(4), List(2), List(3), List(1),List(3), List(4), List(2), List(3), List(1),List(3), List(4), List(2), List(3), List(1))
    //val rowIndicess = List(List(2), List(2), List(4), List(3), List(2),List(3), List(4), List(2), List(3), List(1),List(3), List(4), List(2), List(3), List(1))
    
    //val colIndicess = List(List(3), List(3), List(2), List(3), List(2))
    //val rowIndicess = List(List(1), List(2), List(2,2), List(3), List(3))

    val colIndicess = List(List(3), List(3), List(2), List(3), List(2), List(3), List(3), List(2), List(3), List(2))
    val rowIndicess = List(List(6,1), List(2), List(2,2), List(3), List(3), List(1), List(2), List(2,2), List(3), List(3))

    //val matrix: List[List[Int]] = List.fill(5)(List.fill(5)(0))
    val matrix: Array[Array[Int]] = Array.ofDim[Int](10, 10)

    println("Je print la taille de rowIndices et colIndices" + rowIndicess.length + colIndicess.length)
    start(rowIndicess, colIndicess, matrix)
    //println("Solutionnnnn : " + boolean)

    /*for(i<- 0 until 5) {
        val combinations = produceCombinations(rowIndicess(i), 5, matrix(i))
        println(s"Combinations for the row $rowIndicess(i) of size 5:")
        combinations.foreach(combination => println(combination.mkString(" ")))
    }*/
    val row = List(6,1)
    val size = 10

    val combinations = produceCombinations(row, size, matrix(1).toList)

    println(s"Combinations for the row $row of size $size:")
    combinations.foreach(combination => println(combination.mkString(" ")))
  
  }
}
