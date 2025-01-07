import Util.readFile

import scala.annotation.tailrec
import scala.collection.mutable

@main def day21(): Unit = {
  val input = readFile("resources/day21")
  val solver = KeypadConundrum(input)
  println(solver.solvePart1())
  println(solver.solvePart2())
}

class KeypadConundrum(input: List[String]) {
  import KeypadConundrum._

  def solvePart1(): Any = solve(2)
  def solvePart2(): Any = solve(25)

  private def solve(depth: Int): BigInt = input.map { code =>
    findHumanSequenceLength(depth)(code) * numericPart(code)
  }.sum
}

object KeypadConundrum {
  private type Keypad = Map[Char, List[(Char, Char)]]

  private val memoryExpanded: mutable.Map[Fragment, List[List[Fragment]]] = mutable.Map.empty
  private val memoryReduced: mutable.Map[Fragment, BigInt] = mutable.Map.empty

  private case class Fragment(a: Char, b: Char, d: Int)

  private val numericKeypad: Keypad = Map(
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

  private val directionalKeypad: Keypad = Map(
    '^' -> List('>' -> 'A', 'v' -> 'v'),
    'A' -> List('<' -> '^', 'v' -> '>'),
    '<' -> List('>' -> 'v'),
    'v' -> List('<' -> '<', '>' -> '>', '^' -> '^'),
    '>' -> List('^' -> 'A', '<' -> 'v')
  )

  /** Pre-computed shortest paths for the directional keypad
    */
  private val shortestPathsDirectional: Map[(Char, Char), List[String]] = (for {
    a <- directionalKeypad.keys
    b <- directionalKeypad.keys
  } yield (a -> b) -> findShortestPaths(directionalKeypad)(a, b)).toMap

  /** Finds all shortest paths between two points on a keypad
    */
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

    if (a == b) List("")
    else {
      val initMap = for {
        (key, xs) <- keypad.filter(_._1 == a)
        (char, otherKey) <- xs
      } yield (key -> otherKey) -> List(s"$char")

      findShortestPathsRec(initMap)
    }
  }

  def expand(s: String, depth: Int): BigInt = {
    val initialFragments = ("A" + s).zip(s).toList.map { case (a, b) => Fragment(a, b, depth) }
    processFragments(initialFragments)
    initialFragments.map(memoryReduced.apply).sum
  }

  private def expandMemory(f: Fragment, fs: List[List[Fragment]]): Unit =
    if (fs.forall(_.forall(memoryReduced.contains))) reduceMemory(f, fs.map(_.map(memoryReduced.apply).sum).min)
    else memoryExpanded.addOne(f -> fs)

  private def reduceMemory(f: Fragment, v: BigInt): Unit = reduceMemoryRec(List(f -> v))

  @tailrec
  private def reduceMemoryRec(xs: List[(Fragment, BigInt)]): Unit = xs match
    case Nil => ()
    case (f, v) :: tail =>
      memoryReduced.addOne(f -> v)
      memoryExpanded.filter(_._2.flatten.contains(f)).find { case (f2, v2) =>
        v2.forall(_.forall(memoryReduced.contains))
      } match
        case None => reduceMemoryRec(tail)
        case Some((f2, v2)) =>
          memoryExpanded.remove(f2)
          reduceMemoryRec((f2 -> v2.map(_.map(memoryReduced.apply).sum).min) :: tail)

  @tailrec
  private def processFragments(fs: List[Fragment]): Unit = fs match
    case Nil => ()
    case f :: fs if f.d == 0 =>
      reduceMemory(f, 1)
      processFragments(fs)
    case f :: fs =>
      memoryReduced.get(f) match {
        case Some(_) => processFragments(fs)
        case _ =>
          val paths = shortestPathsDirectional(f.a, f.b).map(_ + "A")
          val newFragments = paths.map { path =>
            ("A" + path).zip(path).map { case (a, b) => Fragment(a, b, f.d - 1) }.toList
          }
          expandMemory(f, newFragments)
          processFragments(newFragments.flatten ::: fs)
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

  def findHumanSequenceLength(depth: Int)(s: String): BigInt = (for {
    numericSequence <- findSequencesForNumericKeypad(s)
    result = expand(s = numericSequence, depth = depth)
  } yield result).min

  private def numericPart(s: String): Int = s.takeWhile(_.isDigit).toInt
}
