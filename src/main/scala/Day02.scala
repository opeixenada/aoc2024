import Util.readFile

import scala.annotation.tailrec
import scala.collection.immutable.::

@main def day02(): Unit = {

  val input = readFile("resources/day02")

  // Part 1

  val reports = input.map { line =>
    line.split("\\s+").map(_.toInt).toList
  }

  val result1 = reports.count(isSafe(_))

  println(result1)

  // Part 2

  val result2 = reports.count { report =>
    generateStates(report).exists(isSafe(_))
  }

  println(result2)
}

@tailrec
def isSafe(xs: List[Int], direction: Int = 0, acc: Boolean = true): Boolean =
  (xs, direction, acc) match
    case (_, _, false)                  => false
    case (x :: y :: ys, 0, _) if x == y => false
    case (x :: y :: ys, 0, _)           => isSafe(xs, (y - x) / (y - x).abs)
    case (x :: y :: ys, 1, _)           => isSafe(y :: ys, 1, y >= x + 1 && y <= x + 3)
    case (x :: y :: ys, -1, _)          => isSafe(y :: ys, -1, y <= x - 1 && y >= x - 3)
    case _                              => true

def generateStates(xs: List[Int]): List[List[Int]] = xs :: xs.indices.map { i =>
  xs.take(i) ++ xs.drop(i + 1)
}.toList
