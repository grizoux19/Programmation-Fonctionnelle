object PossibilitiesGenerator {
  def generatePossibilities(size: Int, indices: List[Int]): List[String] = {
    def generatePossibilitiesHelper(spaceAvailable: Int, indices: List[Int]): List[String] = {
      indices match {
        case Nil => List(List.fill(size)("0").mkString)
        case head :: tail =>
          (1 to spaceAvailable - head - (tail.length - 1)).flatMap { start =>
            val consecutive = List.fill(head)("1").mkString
            val nextSpace = start + head
            generatePossibilitiesHelper(nextSpace, tail).map { rest =>
              List.fill(start)("0").mkString + consecutive + rest
            }
          }.toList
      }
    }

    val possibilities = generatePossibilitiesHelper(size, indices)

    possibilities.filter(str => !str.drop(size+1).contains("1")).map(_.slice(1, size + 1))
  }


  def main(args: Array[String]): Unit = {
    // Exemple d'utilisation
    val possibilities1 = (generatePossibilities(5, List(2, 2)))

    val sousListe = possibilities1
    println("Possibilités pour (5, List(2,2)):")
    println(possibilities1)
    
    println("\nPossibilités pour (5, List(1, 2)):")
    val possibilities2 = generatePossibilities(5, List(1, 2))
    println(possibilities2)

    println("\nPossibilités pour (5, List(1,1,1)):")
    val possibilities3 = generatePossibilities(5, List(1, 1, 1))
    println(possibilities3)


     println("\nPossibilités pour (15, List(3,1,1)):")
    val possibilities4 = generatePossibilities(15, List(3, 1, 1))
    println(possibilities4)
    //println(possibilities4(2).size)

      println("\nPossibilités pour (8, List(2,3)):")
    val possibilities5 = generatePossibilities(8, List( 2,3))
    println(possibilities5)
    println(possibilities5.size)



    
  }
}
