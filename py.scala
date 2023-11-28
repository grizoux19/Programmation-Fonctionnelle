object NonogramHelper {

  def produceCombinations(line: List[Int], size: Int): List[List[Int]] = {
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
  }

  def main(args: Array[String]): Unit = {
    // Exemple d'utilisation
    val row = List(1, 1)
    val size = 10

    val combinations = produceCombinations(row, size)

    println(s"Combinations for the row $row of size $size:")
    combinations.foreach(combination => println(combination.mkString(" ")))
  }
}
