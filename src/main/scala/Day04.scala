import Util.readFile

@main def day04(): Unit = {

  val input: IndexedSeq[IndexedSeq[Char]] = readFile("resources/day04").map(_.toIndexedSeq).toIndexedSeq

  // Part 1

  val result1 = getAllDirections(input).foldLeft(0) { case (acc, xs) => acc + countXmas(xs) }

  println(result1)

  // Part 2

  def isInBounds(row: Int, col: Int): Boolean =
    row >= 0 && row < input.length && col >= 0 && col < input.head.length

  def checkDiagonal(direction: Int, i: Int, j: Int): Boolean = {
    def checkPattern(endChar: Char): Boolean =
      isInBounds(j + 2, i + (2 * direction)) &&
        input(j + 1)(i + direction) == 'A' &&
        input(j + 2)(i + (2 * direction)) == endChar

    isInBounds(j, i) && (input(j)(i) match {
      case 'M' => checkPattern('S')
      case 'S' => checkPattern('M')
      case _   => false
    })
  }

  val result2: Int = input.indices.map { j =>
    input.head.indices.count { i =>
      checkDiagonal(1, i, j) && checkDiagonal(-1, i + 2, j)
    }
  }.sum

  println(result2)
}

def getAllDirections(grid: Seq[Seq[Char]]): Iterable[List[Char]] =
  (grid ++ grid.head.indices.map(i => grid.map(_(i))) ++ getDiagonals(grid) ++ getDiagonals(grid.reverse))
    .flatMap { x =>
      Seq(x, x.reverse)
    }
    .map(_.toList)

def getDiagonals(input: Seq[Seq[Char]]): Iterable[Seq[Char]] =
  (0 until input.length + input.head.length).map { i =>
    for {
      (line, j) <- input.zipWithIndex
      if i - j >= 0 && i - j < input.head.length
    } yield line(i - j)
  }

def countXmas(chars: List[Char]): Int = {
  val pattern = "XMAS".toList
  chars
    .sliding(pattern.length)
    .count(_ == pattern)
}
