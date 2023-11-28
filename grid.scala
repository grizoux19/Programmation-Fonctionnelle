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

  def main(args: Array[String]): Unit = {
    // Exemple d'utilisation de la fonction avec une matrice carrée
    val matrix = Array(
      Array(true, true, true, true, false),
      Array(false, true, false, true, false),
      Array(true, true, false, true, true),
      Array(false, true, false, true, false),
      Array(true, false, true, false, true)
    )

    println("Matrice : " + matrix.map(_.map(if (_) 1 else 0).mkString(" ")).mkString("\n"))
    //val (rowIndices, colIndices) = findNonogramIndices(matrix)
    val rowIndices = calculateRowIndices(matrix)
    val colIndices = calculateColIndices(matrix)

    println("Indices des lignes : " + rowIndices)
    println("Indices des colonnes : " + colIndices)


    val colIndicess = List(List(10), List(10), List(2), List(2), List(10), List(10), List(2), List(2), List(10), List(10))
    val rowIndicess = List(List(2,2,2), List(2,3,2), List(2,3,3), List(2,2,3), List(2,2,2), List(3,2,2), List(3,3,2), List(2,3,2), List(2,2,2), List(2,2,2))

    println("Je print la taille de rowIndices et colIndices" + rowIndicess.length + colIndicess.length)
    val boolean = calculateSolution(rowIndicess, colIndicess)
    println("Solutionnnnn : " + boolean)

  }
}

  def calculateSolution(rowIndices : List[List[Int]], colIndices : List[List[Int]]): Boolean = {
    
    /*val matrix = Array(
      Array(0, 0, 0, 0, 0), //2 -> croix, 1 -> true, 
      Array(0, 0, 0, 0, 0),
      Array(0, 0, 0, 0, 0),
      Array(0, 0, 0, 0, 0),
      Array(0, 0, 0, 0, 0)
    )*/

    val matrix = Array.ofDim[Int](10, 10) // Matrice 10x10 initialisée à zéro

    start(rowIndices, colIndices, matrix)//Fonction pour remplir les hint plus grand que la moitié

    begin(rowIndices, colIndices, matrix)//Remplir de cases sur la première cases est coloriée

    border(rowIndices, colIndices, matrix) //Fonction pour mettre des croix la ou c'est pas possible

    thenn(rowIndices, colIndices, matrix)//Fonction pour remplir les 0
    //Fonction pour partir des bords et écrire des cases

    //Fonction pour mettre des croix la ou c'est pas possible

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
    

    def begin(rowIndices: List[List[Int]], colIndices: List[List[Int]], matrix: Array[Array[Int]]): Boolean = { //Remplir de cases sur la première cases est coloriée
        for(i <- 0 until rowIndices.length) {
            val list = rowIndices(i) //On récupère la list d'indice
            val firstIndices = list(0) //On récupère le premier indice
            val lastIndices = list(list.length - 1) //On récupère le dernier indices

            println("Je print firstIndices et lastIndices de row  et le i " + firstIndices + " " + lastIndices + " " + i)

            if(matrix(i)(0) == 1) { //Si la première case est coloriée
                for(j <- 0 until firstIndices) {
                    matrix(i)(j) = 1
                }
                matrix(i)(firstIndices) = 2 //Mettre un croix après le first indice
            }
            println("Je print le dernier de la ligne " + matrix(i)(rowIndices.length - 1))

            if(matrix(i)(rowIndices.length - 1) == 1) { //Si la dernière case est coloriée
                //println("Je passe dans le if de fin pour les lignes")
                for(j <- rowIndices.length - 1 to (rowIndices.length - lastIndices) by - 1) {
                    matrix(i)(j) = 1
                    println("Je passe dans le if de fin pour les lignes         " + i)
                    if(i.equals(3)) {
                      println("Je print lastIndices et firstIndices de row " + lastIndices + " " + firstIndices + " je print la matrice " + matrix(i)(j) + " et je print i et j " + i + " " + j)
                    }
                }
                matrix(i)(rowIndices.length - lastIndices - 1) = 2 //Mettre un croix avant le last indice
            }
        }
        for(i <- 0 until colIndices.length) {
            val list = colIndices(i) //On récupère la list d'indice
            val firstIndices = list(0) //On récupère le premier indice
            val lastIndices = list(list.length - 1) //On récupère le dernier indices

            println("Je print firstIndices et lastIndices de col " + firstIndices + " " + lastIndices)
            if(firstIndices == colIndices.length || lastIndices == colIndices.length) {
              
            } else {
              if(matrix(0)(i) == 1) { //Si la première case est coloriée
                  for(j <- 0 until firstIndices) {
                      matrix(j)(i) = 1
                  }
                  matrix(firstIndices)(i) = 2 //Mettre un croix après le first indice
              }
              if(matrix(colIndices.length - 1)(i) == 1) { //Si la dernière case est coloriée
                  for(j <- (colIndices.length - 1) until (colIndices.length - lastIndices)) {
                      matrix(j)(i) = 1
                  }
                  matrix(colIndices.length - lastIndices - 1)(i) = 2 //Mettre un croix avant le last indice
              }
            }
        }
        println("J'essaye de print la matrice :")
        for (i <- 0 until matrix.length) {
          for (j <- 0 until matrix(i).length) {
          print(matrix(i)(j) + " ")
          } 
          println()
        }

        true
    }

    def border(rowIndices: List[List[Int]], colIndices: List[List[Int]], matrix: Array[Array[Int]]): Boolean = { //Fonction pour mettre des croix la ou c'est pas possible
      //for(i <- 0 until rowIndices.length) { //Mettre des croix dans les cases vide en fonction des indices
        //val rowsWithZeros = matrix.zipWithIndex.filter { case (row, _) => row.contains(0) }
        //val indicesOfRowsWithZeros = rowsWithZeros.map { case (_, index) => index }

        //indicesOfRowsWithZeros.foreach(println)

        val rowsWithZeros = matrix.zipWithIndex.filter { case (row, _) => row.contains(0) }
        val columnsWithZeros = matrix.transpose.zipWithIndex.filter { case (column, _) => column.contains(0) }

        val indicesOfRowsWithZeros = rowsWithZeros.map { case (_, index) => index }
        val indicesOfColumnsWithZeros = columnsWithZeros.map { case (_, index) => index }

        println("Je print la ligne")
        indicesOfRowsWithZeros.foreach(println)

        println("Je print la colonne")
        indicesOfColumnsWithZeros.foreach(println)

        val numberOfRowsWithZeros = indicesOfRowsWithZeros.length
        val numberOfColumnsWithZeros = indicesOfColumnsWithZeros.length

        if (numberOfRowsWithZeros > numberOfColumnsWithZeros) {
            //println("Je rentre dans le premier if")
            //println("Je print indicesOfColumnsWithZeros(0) " + indicesOfColumnsWithZeros(0) + " indicesOfColumnsWithZeros.length " + indicesOfColumnsWithZeros.length)
            for(i <- 0  until indicesOfColumnsWithZeros.length) { //Problème ici
                val indices = indicesOfColumnsWithZeros(i)
                //println("Je rentre dans le premier for")
                //println("Je print colIndices(i).length "+ colIndices(i).length )
                if(colIndices(i).length == 1) {
                    //println("Je rentre dans le print colIndices(i).length == 1")
                    println("Je print l'indices de la colonne " + indices)
                    val numberOfone = matrix.zipWithIndex.collect { case (row, rowIndex) if row(indices) == 1 => rowIndex } //Extrait les indices des zéros !!!!
                    println("Je print numberOfone " + numberOfone.length + " colIndices(i).head " + colIndices(indices).head + " numberOfOne.length " + numberOfone.length)
                    if(numberOfone.length == colIndices(indices).head) {
                      //On fait rien peut être regarder pour mettre des 2
                    } else if (0 < numberOfone.length && numberOfone.length < colIndices(indices).head) {
                      println("Je rentre dans le else if avec le i " + i)
                      val border = colIndices(indices).head - numberOfone.length
                      println("Je print la border " + border)
                      for(j <- 0 until numberOfone(0) - border) { //De 0 à numberOfOne(0) = 2 - border
                        matrix(j)(indices) = 2
                      }
                      for(j <- (numberOfone(0) + border + 1) until rowIndices.length) //De numberOfOne(0) + border = 2 + 2 jusqu'à la fin
                        matrix(j)(indices) = 2
                    }
                }
            }

        }
  

      //}
        println("J'essaye de print la matrice :")
        for (i <- 0 until matrix.length) {
          for (j <- 0 until matrix(i).length) {
            print(matrix(i)(j) + " ")
          } 
          println()
        }

        true
    }

    def thenn(rowIndices: List[List[Int]], colIndices: List[List[Int]], matrix: Array[Array[Int]]): Boolean = { //Fonction pour remplir les 0
        val test = 0
  
        val rowsWithZeros = matrix.zipWithIndex.filter { case (row, _) => row.contains(0) }
        val columnsWithZeros = matrix.transpose.zipWithIndex.filter { case (column, _) => column.contains(0) }

        val indicesOfRowsWithZeros = rowsWithZeros.map { case (_, index) => index }
        val indicesOfColumnsWithZeros = columnsWithZeros.map { case (_, index) => index }

        println("Je print la ligne")
        indicesOfRowsWithZeros.foreach(println)

        println("Je print la colonne")
        indicesOfColumnsWithZeros.foreach(println)

        val numberOfZerosPosition = findZeros(matrix)
        println("Je print la position des zéros dans la matric " + numberOfZerosPosition)

        //Ici on va regarder quel colonne ou ligne à le moins d'indices
        /*for(i <- 0 until numberOfZerosPosition.length) { //Ici j'ai essayé de faire un truc universel pour les lignes et colonne
          if(rowIndices(numberOfZerosPosition(i)(1) ).length > colIndices(numberOfZerosPosition(i)(0) ).length ) { //Ici on traite les colonnes
            println("Je rentre dans le if du for pour i = " + i)

            val firstIndices = rowIndices(numberOfZerosPosition(i)(1) )(0)
            var counter = 0

            for(j <- 0 until rowIndices.length) {
              if(matrix(numberOfZerosPosition(i)(1)) == 1) {
                counter = counter + 1
              }

            }
          }
        }*/

        for(i <- 0 until numberOfZerosPosition.length) {
          //Ici pour chaque zéros on va regarder si le nombre de 0 est égal au nombre de cases 1 manquantes
          val sumIndiceCol = colIndices(numberOfZerosPosition(i)(1) ).sum //Sum des 1 dans la colonne
          val numberOfOneCol = countZerosInColumn(matrix, numberOfZerosPosition(i)(1), 1) //Nombre de 1 dans la colonne
          val numberOfTwoCol = countZerosInColumn(matrix, numberOfZerosPosition(i)(1), 0)

          val sumIndiceRow = rowIndices(numberOfZerosPosition(i)(0) ).sum //Sum des 1 dans la ligne
          val numberOfOneRow = countZerosInRow(matrix, numberOfZerosPosition(i)(0), 1) //Nombre de 1 dans la ligne
          val numberOfTwoRow = countZerosInRow(matrix, numberOfZerosPosition(i)(0), 0)

          println("Je print le nombre de zéros dans la colonne " + numberOfOneCol + " et le nombre de 0 dans la ligne " + numberOfOneRow)

          println("Je print sumIndiceRow " + sumIndiceRow + " numberOfOneRow " + numberOfOneRow + " numberOfTwoEow " + numberOfTwoRow)
          if(sumIndiceRow == numberOfOneRow + numberOfTwoRow) { //Boucle ici
            val ZerosPositionRow = indicesOfZeros(matrix(numberOfZerosPosition(i)(0))) 
            println("Je print ZerosPositionRow " + ZerosPositionRow)
            for(j <- 0 until ZerosPositionRow.length)
              matrix(numberOfZerosPosition(i)(0))(ZerosPositionRow(j)) = 1
          }
          if(sumIndiceRow == numberOfOneRow) {
            val ZerosPositionRow = indicesOfZeros(matrix(numberOfZerosPosition(i)(0)))
            for(j <- 0 until ZerosPositionRow.length)
              matrix(numberOfZerosPosition(i)(0))(ZerosPositionRow(j)) = 2
          }

          if(sumIndiceCol == numberOfOneCol + numberOfTwoCol) { //Boucle ici
            val ZerosPositionCol = indicesOfZeros(matrix(numberOfZerosPosition(i)(1))) 
            println("Je print ZerosPositionCol " + ZerosPositionCol)
            for(j <- 0 until ZerosPositionCol.length)
              matrix(numberOfZerosPosition(i)(1))(ZerosPositionCol(j)) = 1
          }
          if(sumIndiceCol == numberOfOneCol) {
            val ZerosPositionCol = indicesOfZeros(matrix(numberOfZerosPosition(i)(1)))
            for(j <- 0 until ZerosPositionCol.length)
              matrix(numberOfZerosPosition(i)(1))(ZerosPositionCol(j)) = 2
          }
        }

        println("J'essaye de print la matrice :")
        for (i <- 0 until matrix.length) {
          for (j <- 0 until matrix(i).length) {
            print(matrix(i)(j) + " ")
          } 
          println()
        }

        true
    }
    def indicesOfZeros(line: Array[Int]): List[Int] = {
      line.zipWithIndex.collect { case (value, index) if value == 0 => index }.toList
    }
    def countZerosInRow(matrix: Array[Array[Int]], rowIndex: Int, number: Int): Int = {
      matrix(rowIndex).count(_ == number)
    }
    def countZerosInColumn(matrix: Array[Array[Int]], columnIndex: Int, number: Int): Int = {
      matrix.count(row => row(columnIndex) == number)
    }

    def findZeros(matrix: Array[Array[Int]]): List[(Int, Int)] = {
      matrix.zipWithIndex.flatMap {
        case (row, i) =>
          row.zipWithIndex.collect {
            case (0, j) => (i, j)
        }
      }.toList
    }

    true
}
