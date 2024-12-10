import Util.readFile

@main def day17(): Unit = {
  val input = readFile("resources/day17")
  val solver = Foo17(input)
  println(solver.solvePart1())
  println(solver.solvePart2())
}

class Foo17(input: List[String]) {
  def solvePart1(): Any = ???
  def solvePart2(): Any = ???
}
