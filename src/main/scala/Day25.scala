import Util.readFile

@main def day25(): Unit = {
  val input = readFile("resources/day25")
  val solver = Foo25(input)
  println(solver.solvePart1())
  println(solver.solvePart2())
}

class Foo25(input: List[String]) {
  def solvePart1(): Any = ???
  def solvePart2(): Any = ???
}
