import Util.readFile

@main def day16(): Unit = {
  val input = readFile("resources/day16")
  val solver = Foo16(input)
  println(solver.solvePart1())
  println(solver.solvePart2())
}

class Foo16(input: List[String]) {
  def solvePart1(): Any = ???
  def solvePart2(): Any = ???
}
