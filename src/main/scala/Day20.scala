import Util.readFile

@main def day20(): Unit = {
  val input = readFile("resources/day20")
  val solver = Foo20(input)
  println(solver.solvePart1())
  println(solver.solvePart2())
}

class Foo20(input: List[String]) {
  def solvePart1(): Any = ???
  def solvePart2(): Any = ???
}
