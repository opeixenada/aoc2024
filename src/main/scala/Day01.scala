import Util.readFile

import scala.annotation.tailrec

@main def day01(): Unit = {

  val input = readFile("resources/day01")

  // Part 1

  val ns = input.map(_.split("\\s+").map(_.toInt).toList)
  val xs = ns.map(_.head).sorted
  val ys = ns.map(_.last).sorted
  val result1 = xs.zip(ys).map { case (a, b) => (a - b).abs }.sum

  println(result1)

  // Part 2

  val result2 = getSimilarityScore(xs, ys, 0)

  println(result2)
}

@tailrec
def getSimilarityScore(xs: List[Int], ys: List[Int], acc: Int): Int = xs match
  case Nil     => acc
  case a :: as => getSimilarityScore(as, ys.dropWhile(_ < a), acc + a * ys.dropWhile(_ < a).takeWhile(_ == a).length)
