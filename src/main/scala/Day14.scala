import Util.readFile

@main def day14(): Unit = {
  val input = readFile("resources/day14")
  val solver = Foo14(input)
  println(solver.solvePart1())
  println(solver.solvePart2())
}

class Foo14(input: List[String]) {
  def solvePart1(): Any = ???
  def solvePart2(): Any = ???
}
