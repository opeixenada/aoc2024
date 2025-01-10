import MonkeyMarket.{get2kth, getTaggedPrices, mostBananas}
import Util.readFile

@main def day22(): Unit = {
  val input = readFile("resources/day22")
  val solver = MonkeyMarket(input)
  println(solver.solvePart1())
  println(solver.solvePart2())
}

object MonkeyMarket {
  private def mix(x: BigInt)(value: BigInt): BigInt = value ^ x
  private def prune(x: BigInt): BigInt = x % BigInt(16777216)

  def get2kth(x: BigInt): BigInt = (0 until 2000).foldLeft(x) { case (a, _) => next(a) }

  def next(x: BigInt): BigInt = {
    val a = prune(mix(x)(x * 64))
    val b = prune(mix(a)(a / 32))
    prune(mix(b)(b * 2048))
  }

  private def getDiffPrice(x: BigInt): (BigInt, Int) = {
    val y = next(x)
    y -> (y % 10 - x % 10).toInt
  }

  def getPrices(n: Int, x: BigInt): List[(Int, Int)] = (0 until n)
    .foldLeft(x -> List.empty[(Int, Int)]) { case ((a, acc), _) =>
      val (y, price) = getDiffPrice(a)
      y -> acc.appended((y % 10).toInt -> price)
    }
    ._2

  def getTaggedPrices(x: BigInt): Map[String, Int] = {
    val prices = getPrices(2000, x)

    prices.tails
      .take(3)
      .foldRight(prices.tails.drop(3).toList.head.map { case (a, b) => a -> b.toString }) { case (tail, acc) =>
        acc.zip(tail).map { case ((price, accChange), (_, change)) =>
          price -> (change.toString + accChange)
        }
      }
      .groupBy(_._2)
      .map {
        case (change, x :: xs) => x.swap
        case (_, Nil)          => throw Exception("Oops")
      }
  }

  def mostBananas(xs: List[Map[String, Int]]): Int = xs
    .flatMap(_.keySet)
    .toSet
    .map { sequence =>
      xs.map(_.getOrElse(sequence, 0)).sum
    }
    .max

}

class MonkeyMarket(input: List[String]) {
  def solvePart1(): Any = input.map(BigInt.apply).map(get2kth).sum
  def solvePart2(): Any = mostBananas(input.map(BigInt.apply).map(getTaggedPrices))
}
