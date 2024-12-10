import Util.readFile

@main def day13(): Unit = {
  val input = readFile("resources/day13")
  val solver = Foo13(input)
  println(solver.solvePart1())
  println(solver.solvePart2())
}

class Foo13(input: List[String]) {
  def solvePart1(): Any = ???
  def solvePart2(): Any = ???
}
