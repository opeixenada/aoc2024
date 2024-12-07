import Util.readFile

import scala.annotation.tailrec
import scala.math.BigInt

@main def day07(): Unit = {

  val input: Iterable[(BigInt, List[BigInt])] = readFile("resources/day07").map { s =>
    val arr = s.split(": ")
    BigInt(arr.head) -> arr.last.split(' ').map(BigInt(_)).toList
  }

  case class State(x: BigInt, rest: List[BigInt])

  type Operation = (BigInt, BigInt) => BigInt

  val sum: Operation = (x, y) => x + y
  val prod: Operation = (x, y) => x * y
  val concat: Operation = (x, y) => BigInt(x.toString + y.toString)

  @tailrec
  def explore(operations: List[Operation])(target: BigInt, states: List[State]): Boolean = states match
    case Nil => false
    case s :: tail =>
      s.rest match
        case Nil if s.x == target => true
        case Nil                  => explore(operations)(target, tail)
        case _ if s.x > target    => explore(operations)(target, tail)
        case a :: as =>
          explore(operations)(
            target,
            operations.map { operation =>
              State(operation(s.x, a), as)
            } ::: tail
          )

  def isGood(operations: List[Operation])(result: BigInt, xs: List[BigInt]): Boolean =
    explore(operations)(result, List(State(xs.head, xs.tail)))

  // Part 1

  val result1 = input.filter(isGood(List(sum, prod))).map(_._1).sum

  println(result1)

  // Part 2

  val result2 = input.filter(isGood(List(sum, prod, concat))).map(_._1).sum

  println(result2)
}
