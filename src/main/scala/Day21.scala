import KeypadConundrum.{findMySequenceLength, numericPart}
import Util.readFile

import scala.annotation.tailrec
import scala.collection.mutable

@main def day21(): Unit = {
  val input = readFile("resources/day21")
  val solver = KeypadConundrum(input)
  println(solver.solvePart1())
  println(solver.solvePart2())
}

object KeypadConundrum {

  private type Keypad = Map[Char, List[(Char, Char)]]

  private val numericKeypad: Map[Char, List[(Char, Char)]] = Map(
    '7' -> List('>' -> '8', 'v' -> '4'),
    '8' -> List('<' -> '7', '>' -> '9', 'v' -> '5'),
    '9' -> List('<' -> '8', 'v' -> '6'),
    '4' -> List('^' -> '7', 'v' -> '1', '>' -> '5'),
    '5' -> List('^' -> '8', 'v' -> '2', '>' -> '6', '<' -> '4'),
    '6' -> List('^' -> '9', 'v' -> '3', '<' -> '5'),
    '1' -> List('^' -> '4', '>' -> '2'),
    '2' -> List('^' -> '5', '>' -> '3', '<' -> '1', 'v' -> '0'),
    '3' -> List('^' -> '6', '<' -> '2', 'v' -> 'A'),
    '0' -> List('^' -> '2', '>' -> 'A'),
    'A' -> List('<' -> '0', '^' -> '3')
  )

  private val directionalKeypad: Map[Char, List[(Char, Char)]] = Map(
    '^' -> List('>' -> 'A', 'v' -> 'v'),
    'A' -> List('<' -> '^', 'v' -> '>'),
    '<' -> List('>' -> 'v'),
    'v' -> List('<' -> '<', '>' -> '>', '^' -> '^'),
    '>' -> List('^' -> 'A', '<' -> 'v')
  )

  private val shortestPathsDirectional: Map[(Char, Char), List[String]] = {
    val xs = for {
      a <- directionalKeypad.keys
      b <- directionalKeypad.keys
    } yield (a -> b) -> findShortestPaths(directionalKeypad)(a, b)

    xs.toMap
  }

  private def findShortestPaths(keypad: Keypad)(a: Char, b: Char): List[String] = {

    @tailrec
    def findShortestPathsRec(acc: Map[(Char, Char), List[String]]): List[String] = acc.get(a -> b) match
      case Some(paths) => paths
      case _ =>
        val nextIteration = for {
          ((key1, key2), paths) <- acc.toList
          path <- paths
          (nextChar, nextKey) <- keypad(key2)
        } yield (key1 -> nextKey) -> path.appended(nextChar)

        findShortestPathsRec(nextIteration.groupBy(_._1).view.mapValues(_.map(_._2)).toMap)

    val initMap = for {
      (key, xs) <- keypad.filter(_._1 == a)
      (char, otherKey) <- xs
    } yield (key -> otherKey) -> List(s"$char")

    if (a == b) List("")
    else findShortestPathsRec(initMap)
  }

  def findSequencesForNumericKeypad(s: String): List[String] = s
    .foldLeft(('A', List(""))) { case ((currentButton, acc), nextButton) =>
      val newPathSegments = for {
        segment <- findShortestPaths(numericKeypad)(currentButton, nextButton)
        path <- acc
      } yield path + segment.appended('A')

      nextButton -> newPathSegments
    }
    ._2
    .map(_.mkString)

  def findMySequenceLength(directionalKeypadsThatRobotsAreUsing: Int)(s: String): BigInt = (for {
    numericSequence <- findSequencesForNumericKeypad(s)
    ys = expand(s = numericSequence, depth = directionalKeypadsThatRobotsAreUsing)
  } yield ys).min

  private case class Fragment(a: Char, b: Char, d: Int)

  def expand(s: String, depth: Int): BigInt = {
    val initialFragments = ("A" + s).zip(s).toList.map { case (a, b) => Fragment(a, b, depth) }
    processFragments(initialFragments)
    initialFragments.map(memory2.apply).sum
  }

  private val memory1: mutable.Map[Fragment, List[List[Fragment]]] = mutable.Map.empty
  private val memory2: mutable.Map[Fragment, BigInt] = mutable.Map.empty

  private def expandMemory(f: Fragment, fs: List[List[Fragment]]): Unit = {
    if (fs.forall(_.forall(memory2.contains))) reduceMemory(f, fs.map(_.map(memory2.apply).sum).min)
    else memory1.addOne(f -> fs)
  }

  private def reduceMemory(f: Fragment, v: BigInt): Unit = reduceMemoryRec(List(f -> v))

  @tailrec
  private def reduceMemoryRec(xs: List[(Fragment, BigInt)]): Unit = xs match
    case Nil => ()
    case (f, v) :: tail =>
      memory2.addOne(f -> v)
      memory1.filter(_._2.flatten.contains(f)).find { case (f2, v2) => v2.forall(_.forall(memory2.contains)) } match
        case None => reduceMemoryRec(tail)
        case Some((f2, v2)) =>
          memory1.remove(f2)
          reduceMemoryRec((f2 -> v2.map(_.map(memory2.apply).sum).min) :: tail)

  @tailrec
  private def processFragments(fragments: List[Fragment]): Unit = fragments match
    case Nil => ()
    case f :: fs if f.d == 0 =>
      reduceMemory(f, 1)
      processFragments(fs)
    case f :: fs =>
      memory2.get(f) match
        case Some(hit) =>
          processFragments(fs)
        case _ =>
          val paths = shortestPathsDirectional(f.a, f.b).map(_ + "A")
          val newFragments: List[List[Fragment]] = paths.map { path =>
            ("A" + path).zip(path).map { case (a, b) => Fragment(a, b, f.d - 1) }.toList
          }
          expandMemory(f, newFragments)
          processFragments(newFragments.flatten ::: fs)

  def numericPart(s: String): Int = s.takeWhile(_.isDigit).toInt
}

class KeypadConundrum(input: List[String]) {

  def solvePart1(): Any = input.map { code =>
    findMySequenceLength(2)(code) * numericPart(code)
  }.sum

  def solvePart2(): Any = input.map { code =>
    findMySequenceLength(25)(code) * numericPart(code)
  }.sum
}
