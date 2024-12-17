import ChronospatialComputer.{Computer, findQuineA}
import Util.readFile

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

  case class Computer(
      program: Vector[Int],
      a: BigInt = 0,
      b: BigInt = 0,
      c: BigInt = 0,
      pointer: Int = 0,
      output: Vector[Int] = Vector.empty
  ) {
    def getOutput: String = output.mkString(",")

    @tailrec
    final def exec(): Computer =
      if (pointer > (program.length - 2)) this
      else instructions(program(pointer))(this, program(pointer + 1)).exec()
  }

  case class State(a: BigInt, n: Int, values: List[Int])

  /** This function finds a value for register A that will make the computer output its own program. In other words, it
    * finds input that creates a quine. See: [[https://en.wikipedia.org/wiki/Quine_(computing)]].
    *
    * It uses backtracking search to build up the value of A one 3-bit digit at a time. It works because each
    * instruction in the output depends on exactly 3 bits of A (because there's `a = a/8` instruction in every cycle).
    */
  @tailrec
  def findQuineA(
      program: Vector[Int],
      state: State = State(0, 1, (0 to 7).toList),
      stack: List[State] = Nil
  ): Option[BigInt] = {
    // If we've matched all n elements of the program, we've found our answer
    if (state.n > program.length) Some(state.a)
    else
      state.values match
        case Nil =>
          // If we run out of values to try at this level, backtrack to previous state
          stack match
            case Nil     => None // If no more states to try, we failed
            case s :: ss => findQuineA(program, s, ss)

        case i :: rest =>
          // Build the next value of A by:
          // 1. Shifting existing A left by 3 bits to make room for new digit
          // 2. OR-ing with i to add the new 3-bit digit
          // This is like building a base-8 number digit by digit from left to right
          val nextA = (state.a << 3) | i

          // Create a test computer with this A value
          val computer = Computer(program = program, a = nextA)

          // Run the computer and get its output
          val output = computer.exec().output

          // We're trying to match the rightmost n elements of the program
          // This is because we're building the solution from left to right,
          // but the program outputs instructions from right to left
          val target = program.takeRight(state.n)

          if (output == target)
            // If this value of A produces the correct output for the rightmost n elements,
            // try matching n+1 elements with new digits
            // Save current state for backtracking
            val nextState = State(nextA, state.n + 1, (0 to 7).toList)
            val stackState = State(state.a, state.n, rest)
            findQuineA(program, nextState, stackState :: stack)
          else
            // If this digit doesn't work, try the next value
            findQuineA(program, State(state.a, state.n, rest), stack)
  }
}

class ChronospatialComputer(input: List[String]) {

  private val a = BigInt(input.head.drop("Register A: ".length))
  private val b = BigInt(input.tail.head.drop("Register B: ".length))
  private val c = BigInt(input.drop(2).head.drop("Register C: ".length))
  private val program = input.drop(4).head.drop("Program: ".length).split(",").map(_.toInt).toVector

  private val state0: Computer = Computer(program, a, b, c)

  def solvePart1(): Any = state0.exec().getOutput
  def solvePart2(): Any = findQuineA(program).get
}
