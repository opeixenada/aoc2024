import Util.readFile

@main def day18(): Unit = {
  val input = readFile("resources/day18")
  val solver = Foo18(input)
  println(solver.solvePart1())
  println(solver.solvePart2())
}

class Foo18(input: List[String]) {
  def solvePart1(): Any = ???
  def solvePart2(): Any = ???
}
