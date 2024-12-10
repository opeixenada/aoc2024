import Util.readFile

@main def day23(): Unit = {
  val input = readFile("resources/day23")
  val solver = Foo23(input)
  println(solver.solvePart1())
  println(solver.solvePart2())
}

class Foo23(input: List[String]) {
  def solvePart1(): Any = ???
  def solvePart2(): Any = ???
}
