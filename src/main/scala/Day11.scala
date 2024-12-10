import Util.readFile

@main def day11(): Unit = {
  val input = readFile("resources/day11")
  val solver = Foo11(input)
  println(solver.solvePart1())
  println(solver.solvePart2())
}

class Foo11(input: List[String]) {
  def solvePart1(): Any = ???
  def solvePart2(): Any = ???
}
