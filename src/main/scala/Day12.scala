import Util.readFile

@main def day12(): Unit = {
  val input = readFile("resources/day12")
  val solver = Foo12(input)
  println(solver.solvePart1())
  println(solver.solvePart2())
}

class Foo12(input: List[String]) {
  def solvePart1(): Any = ???
  def solvePart2(): Any = ???
}
