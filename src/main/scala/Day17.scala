import ChronospatialComputer.{Computer, bruteforceA}
import Util.{readFile, withTimeLogging}

import scala.annotation.tailrec

@main def day17(): Unit = {
  val input = readFile("resources/day17")
  val solver = ChronospatialComputer(input)
  println(solver.solvePart1())
  println(solver.solvePart2())
}

object ChronospatialComputer {

  private type ComboOperand = Computer => BigInt

  private val comboOperands: Map[Int, ComboOperand] = Map(
    0 -> { _ => BigInt(0) },
    1 -> { _ => BigInt(1) },
    2 -> { _ => BigInt(2) },
    3 -> { _ => BigInt(3) },
    4 -> { _.a },
    5 -> { _.b },
    6 -> { _.c }
  )

  private type Instruction = (Computer, Int) => Computer

  private def adv(s: Computer, operand: Int): Computer =
    s.copy(a = s.a >> comboOperands(operand)(s).toInt, pointer = s.pointer + 2)

  private def bxl(s: Computer, operand: Int): Computer =
    s.copy(b = s.b ^ operand, pointer = s.pointer + 2)

  private def bst(s: Computer, operand: Int): Computer =
    s.copy(b = comboOperands(operand)(s) & 0x7, pointer = s.pointer + 2)

  private def jnz(s: Computer, operand: Int): Computer =
    if (s.a == 0) s.copy(pointer = s.pointer + 2)
    else s.copy(pointer = operand)

  private def bxc(s: Computer, operand: Int): Computer =
    s.copy(b = s.b ^ s.c, pointer = s.pointer + 2)

  private def out(s: Computer, operand: Int): Computer =
    s.copy(pointer = s.pointer + 2, output = s.output.appended((comboOperands(operand)(s) & 0x7).toInt))

  private def bdv(s: Computer, operand: Int): Computer =
    s.copy(b = s.a >> comboOperands(operand)(s).toInt, pointer = s.pointer + 2)

  private def cdv(s: Computer, operand: Int): Computer =
    s.copy(c = s.a >> comboOperands(operand)(s).toInt, pointer = s.pointer + 2)

  private val instructions: Map[Int, Instruction] = Map(
    0 -> adv,
    1 -> bxl,
    2 -> bst,
    3 -> jnz,
    4 -> bxc,
    5 -> out,
    6 -> bdv,
    7 -> cdv
  )

  @tailrec
  def bruteforceA(c: Computer, a: BigInt): BigInt = {
    if (c.a % 10000000 == 0) println(s"A: ${c.a}")
    val newComputer = c.copy(a = a)
    if (newComputer.outputsProgram()) a
    else bruteforceA(newComputer, a + 1)
  }

  case class Computer(
      program: Vector[Int],
      a: BigInt = 0,
      b: BigInt = 0,
      c: BigInt = 0,
      pointer: Int = 0,
      output: Vector[Int] = Vector.empty
  ) {
    def startA: BigInt = (BigInt(1) << (3 * (program.length - 1))) - 1

    def getOutput: String = output.mkString(",")

    @tailrec
    final def exec(): Computer =
      if (pointer > (program.length - 2)) this
      else instructions(program(pointer))(this, program(pointer + 1)).exec()

    @tailrec
    final def outputsProgram(): Boolean =
      if (!program.startsWith(output)) false
      else if (pointer > (program.length - 2)) program == output
      else instructions(program(pointer))(this, program(pointer + 1)).outputsProgram()
  }
}

class ChronospatialComputer(input: List[String]) {

  private val a = BigInt(input.head.drop("Register A: ".length))
  private val b = BigInt(input.tail.head.drop("Register B: ".length))
  private val c = BigInt(input.drop(2).head.drop("Register C: ".length))
  private val program = input.drop(4).head.drop("Program: ".length).split(",").map(_.toInt).toVector

  private val state0: Computer = Computer(program, a, b, c)

  def solvePart1(): Any = state0.exec().getOutput
  def solvePart2(): Any = withTimeLogging(bruteforceA(state0, state0.startA))
}
