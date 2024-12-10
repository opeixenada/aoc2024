import Util.readFile

@main def day15(): Unit = {
  val input = readFile("resources/day15")
  val solver = Foo15(input)
  println(solver.solvePart1())
  println(solver.solvePart2())
}

class Foo15(input: List[String]) {
  def solvePart1(): Any = ???
  def solvePart2(): Any = ???
}
