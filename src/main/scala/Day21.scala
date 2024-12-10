import Util.readFile

@main def day21(): Unit = {
  val input = readFile("resources/day21")
  val solver = Foo21(input)
  println(solver.solvePart1())
  println(solver.solvePart2())
}

class Foo21(input: List[String]) {
  def solvePart1(): Any = ???
  def solvePart2(): Any = ???
}
