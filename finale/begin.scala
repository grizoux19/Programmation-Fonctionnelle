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
            println("Je print le dernier de la ligne " + matrix(i)(colIndices.length - 1))

            if(matrix(i)(colIndices.length - 1) == 1) { //Si la dernière case est coloriée
                //println("Je passe dans le if de fin pour les lignes")
                for(j <- colIndices.length - 1 to (colIndices.length - lastIndices) by - 1) {
                    matrix(i)(j) = 1
                    println("Je passe dans le if de fin pour les lignes         " + i)
                    if(i.equals(3)) {
                      println("Je print lastIndices et firstIndices de row " + lastIndices + " " + firstIndices + " je print la matrice " + matrix(i)(j) + " et je print i et j " + i + " " + j)
                    }
                }
                matrix(i)(colIndices.length - lastIndices - 1) = 2 //Mettre un croix avant le last indice
            }
        }
        for(i <- 0 until colIndices.length) {
            val list = colIndices(i) //On récupère la list d'indice
            val firstIndices = list(0) //On récupère le premier indice
            val lastIndices = list(list.length - 1) //On récupère le dernier indices

            println("Je print firstIndices et lastIndices de col " + firstIndices + " " + lastIndices)
            if(firstIndices == rowIndices.length || lastIndices == rowIndices.length) {
              
            } else {
              if(matrix(0)(i) == 1) { //Si la première case est coloriée
                  for(j <- 0 until firstIndices) {
                      matrix(j)(i) = 1
                  }
                  matrix(firstIndices)(i) = 2 //Mettre un croix après le first indice
              }
              if(matrix(colIndices.length - 1)(i) == 1) { //Si la dernière case est coloriée
                  for(j <- (rowIndices.length - 1) until (rowIndices.length - lastIndices)) {
                      matrix(j)(i) = 1
                  }
                  matrix(rowIndices.length - lastIndices - 1)(i) = 2 //Mettre un croix avant le last indice
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