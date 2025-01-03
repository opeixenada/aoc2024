import ClawContraption.{Machine, offset}
import Util.readFile

@main def day13(): Unit = {
  val input = readFile("resources/day13")
  val solver = ClawContraption(input)
  println(solver.solvePart1())
  println(solver.solvePart2())
}

object ClawContraption {
  private val aPrice = 3
  private val bPrice = 1
  private val maxCount = 100
  private val offset = BigInt("10000000000000")

  case class Machine(x_a: BigInt, y_a: BigInt, x_b: BigInt, y_b: BigInt, x: BigInt, y: BigInt) {

    /** It's just two integer equations with two unknowns! Let's solve it arithmetically. */
    def getMinTokens: Option[BigInt] = {
      val a_remainder = (y * x_b - x * y_b) % (x_b * y_a - x_a * y_b)
      val b_remainder = (y * x_a - x * y_a) % (y_b * x_a - x_b * y_a)

      if (a_remainder == 0 && b_remainder == 0) {
        val a = (y * x_b - x * y_b) / (x_b * y_a - x_a * y_b)
        val b = (y * x_a - x * y_a) / (y_b * x_a - x_b * y_a)
        Some(a * 3 + b)
      } else None
    }
  }
}

class ClawContraption(input: List[String]) {

  private val machines: Iterable[Machine] = Util.splitBy(input, _.isBlank).map { m =>
    val ns = m.flatMap("(\\d)+".r.findAllMatchIn).map(_.matched.toInt)
    Machine(ns(0), ns(1), ns(2), ns(3), ns(4), ns(5))
  }

  def solvePart1(): Any = machines.flatMap(_.getMinTokens).sum

  def solvePart2(): Any = machines
    .map { m => m.copy(x = m.x + offset, y = m.y + offset) }
    .flatMap(_.getMinTokens)
    .sum
}
