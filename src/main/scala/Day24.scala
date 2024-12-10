import Util.readFile

@main def day24(): Unit = {
  val input = readFile("resources/day24")
  val solver = Foo24(input)
  println(solver.solvePart1())
  println(solver.solvePart2())
}

class Foo24(input: List[String]) {
  def solvePart1(): Any = ???
  def solvePart2(): Any = ???
}
