import Util.readFile
import scala.collection.mutable
import scala.annotation.tailrec

@main def day05(): Unit = {

  val input = readFile("resources/day05")

  val orderingRules: Map[Int, Set[Int]] = input
    .takeWhile(!_.isBlank)
    .map { s =>
      val Array(from, to) = s.split('|')
      from.trim.toInt -> to.trim.toInt
    }
    .groupMap(_._1)(_._2)
    .view
    .mapValues(_.toSet)
    .toMap
    .withDefaultValue(Set.empty)

  val updates: List[List[Int]] = input
    .dropWhile(!_.isBlank)
    .drop(1)
    .map(_.split(',').map(_.toInt).toList)

  val isLeftCache = mutable.Map[(Int, Int), Boolean]()
  def isLeft(x: Int, y: Int): Boolean = {
    isLeftCache.getOrElseUpdate((x, y), !orderingRules(y).contains(x))
  }

  def isValid(update: List[Int]): Boolean = update.tails.toList.tail.forall(update.zip(_).forall(isLeft))

  def getMiddle(update: List[Int]): Int = update.drop(update.length / 2).head

  // Part 1

  val result1 = updates.filter(isValid).map(getMiddle).sum

  println(result1)

  // Part 2

  case class State(placed: List[Int], unplaced: Set[Int])

  def order(xs: List[Int]): List[Int] = explore(List(State(Nil, xs.toSet)))

  @tailrec
  def explore(states: List[State]): List[Int] = states match
    case Nil                             => Nil
    case s :: tail if s.unplaced.isEmpty => s.placed
    case s :: tail =>
      val nextStates = s.unplaced
        .filter(a => s.placed.forall(b => isLeft(b, a)))
        .map(a => State(a :: s.placed, s.unplaced - a))
      explore(nextStates.toList ::: tail)

  val result2 = updates.filterNot(isValid).map(order).map(getMiddle).sum

  println(result2)
}
