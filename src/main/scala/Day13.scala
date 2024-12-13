import ClawContraption.Machine
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

  case class Machine(ax: Int, ay: Int, bx: Int, by: Int, px: Int, py: Int) {
    def getMinTokens: Option[Int] = (for {
      a <- 0 to 100
      if (px - (a * ax)) % bx == 0
      b = (px - (a * ax)) / bx
      if py == a * ay + b * by
    } yield (a * 3) + b).minOption
  }
}

class ClawContraption(input: List[String]) {

  private val machines: Iterable[Machine] = Util.splitBy(input, _.isBlank).map { m =>
    val ns = "(\\d)+".r.findAllMatchIn(m.mkString("\n")).map(_.matched.toInt).toIndexedSeq
    Machine(ns(0), ns(1), ns(2), ns(3), ns(4), ns(5))
  }

  def solvePart1(): Any = machines.flatMap(_.getMinTokens).sum
  def solvePart2(): Any = ???
}
