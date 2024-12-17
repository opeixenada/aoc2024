import ChronospatialComputer.Computer
import org.scalatest.flatspec.AnyFlatSpec

class ChronospatialComputerTests extends AnyFlatSpec {

  "exec" should "pass test 1" in {
    val program = "2,6".split(",").map(_.toInt).toVector
    val s = Computer(c = 9, program = program)
    assert(s.exec().b == 1)
  }

  it should "pass test 2" in {
    val program = "5,0,5,1,5,4".split(",").map(_.toInt).toVector
    val s = Computer(a = 10, program = program)
    assert(s.exec().getOutput == "0,1,2")
  }

  it should "pass test 3" in {
    val program = "0,1,5,4,3,0".split(",").map(_.toInt).toVector
    val s = Computer(a = 2024, program = program)
    val result = s.exec()
    assert(result.getOutput == "4,2,5,6,7,7,7,7,3,1,0")
    assert(result.a == 0)
  }

  it should "pass test 4" in {
    val program = "1,7".split(",").map(_.toInt).toVector
    val s = Computer(b = 29, program = program)
    assert(s.exec().b == 26)
  }

  it should "pass test 5" in {
    val program = "4,0".split(",").map(_.toInt).toVector
    val s = Computer(b = 2024, c = 43690, program = program)
    assert(s.exec().b == 44354)
  }

  "solvePart1" should "pass test 1" in {
    val input =
      """|Register A: 729
         |Register B: 0
         |Register C: 0
         |
         |Program: 0,1,5,4,3,0""".stripMargin.split('\n').toList

    assert(ChronospatialComputer(input).solvePart1() == "4,6,3,5,6,3,5,2,1,0")
  }

  "solvePart2" should "pass test 1" in {
    val input =
      """Register A: 2024
        |Register B: 0
        |Register C: 0
        |
        |Program: 0,3,5,4,3,0""".stripMargin.split('\n').toList

    assert(ChronospatialComputer(input).solvePart2() == 117440)
  }
}
