import Util.readFile

import scala.annotation.tailrec
import scala.collection.immutable.::

@main def day04(): Unit = {

  val input: IndexedSeq[IndexedSeq[Char]] = readFile("resources/day04").map(_.toCharArray.toIndexedSeq).toIndexedSeq

  // Part 1

  val result1 = Seq(
    input,
    input.head.indices.map { i => input.map(_(i)) },
    getDiagonals(input),
    getDiagonals(input.reverse)
  ).flatMap { _.map(_.toList).map(countXmasBothDirections) }.sum

  println(result1)

  // Part 2

  def checkDiagonal(direction: Int, i: Int, j: Int): Boolean = {
    def check(lastLetter: Char): Boolean =
      (j + 2) < input.length &&
        (i + 2 * direction) > -1 &&
        (i + 2 * direction) < input.head.length &&
        input(j + 1)(i + direction) == 'A' &&
        input(j + 2)(i + 2 * direction) == lastLetter

    i > -1 && i < input.head.length && j > -1 && j < input.length && (input(j)(i) match
      case 'M' => check('S')
      case 'S' => check('M')
      case _   => false
    )
  }

  val result2: Int = input.indices.map { j =>
    input.head.indices.count { i =>
      checkDiagonal(1, i, j) && checkDiagonal(-1, i + 2, j)
    }
  }.sum

  println(result2)
}

def getDiagonals(input: Seq[IndexedSeq[Char]]): Iterable[Iterable[Char]] =
  (0 until input.length + input.head.length).map { i =>
    input.zipWithIndex.flatMap { case (line, j) =>
      if (i - j > -1 && i - j < input.head.length) Some(line(i - j)) else None
    }
  }

def countXmasBothDirections(s: List[Char]): Int =
  Seq(s, s.reverse).map(countXmas(_)).sum

@tailrec
def countXmas(s: List[Char], stack: String = "", count: Int = 0): Int = (s, stack) match
  case (Nil, _)             => count
  case ('X' :: tail, _)     => countXmas(tail, "X", count)
  case ('M' :: tail, "X")   => countXmas(tail, "XM", count)
  case ('A' :: tail, "XM")  => countXmas(tail, "XMA", count)
  case ('S' :: tail, "XMA") => countXmas(tail, "", count + 1)
  case (x :: tail, _)       => countXmas(tail, "", count)
