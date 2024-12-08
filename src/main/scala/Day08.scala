import Util.readFile

import scala.annotation.tailrec

@main def day08(): Unit = {

  val input = readFile("resources/day08")

  val antennas: List[List[(Int, Int)]] = (for {
    (xs, x) <- input.zipWithIndex
    (ch, y) <- xs.zipWithIndex
    if ch != '.'
  } yield (ch, x -> y)).groupBy(_._1).map(_._2.map(_._2)).toList

  def inBounds(cs: (Int, Int)): Boolean = {
    val (x, y) = cs
    x > -1 && x < input.size && y > -1 && y < input.head.length
  }

  def countAntinodes(f: (((Int, Int), (Int, Int))) => List[(Int, Int)]) = (for {
    xs <- antennas
    ys <- xs.tails.drop(1)
    pair <- xs.zip(ys)
    node <- f(pair)
  } yield node).distinct.size

  // Part 1

  def getAntinodes(pair: ((Int, Int), (Int, Int))): List[(Int, Int)] = {
    val ((x1, y1), (x2, y2)) = pair
    val xDiff = x2 - x1
    val yDiff = y2 - y1
    List((x1 - xDiff, y1 - yDiff), (x2 + xDiff, y2 + yDiff)).filter(inBounds)
  }

  val result1 = countAntinodes(getAntinodes)

  println(result1)

  // Part 2

  @tailrec
  def traverse(point: (Int, Int), xDiff: Int, yDiff: Int, acc: List[(Int, Int)]): List[(Int, Int)] =
    (point._1 + xDiff, point._2 + yDiff) match
      case nextPoint if inBounds(nextPoint) => traverse(nextPoint, xDiff, yDiff, nextPoint :: acc)
      case _                                => acc

  def getResonantHarmonicsAntinodes(pair: ((Int, Int), (Int, Int))): List[(Int, Int)] = {
    val ((x1, y1), (x2, y2)) = pair
    val xDiff = x2 - x1
    val yDiff = y2 - y1

    traverse((x1, y1), xDiff * -1, yDiff * -1, List((x1, y1))) ++
      traverse((x2, y2), xDiff, yDiff, List((x2, y2)))
  }

  val result2 = countAntinodes(getResonantHarmonicsAntinodes)

  println(result2)
}
