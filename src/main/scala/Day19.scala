import Util.readFile

@main def day19(): Unit = {
  val input = readFile("resources/day19")
  val solver = Foo19(input)
  println(solver.solvePart1())
  println(solver.solvePart2())
}

class Foo19(input: List[String]) {
  def solvePart1(): Any = ???
  def solvePart2(): Any = ???
}
