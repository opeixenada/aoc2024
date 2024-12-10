import Util.readFile

@main def day22(): Unit = {
  val input = readFile("resources/day22")
  val solver = Foo22(input)
  println(solver.solvePart1())
  println(solver.solvePart2())
}

class Foo22(input: List[String]) {
  def solvePart1(): Any = ???
  def solvePart2(): Any = ???
}
