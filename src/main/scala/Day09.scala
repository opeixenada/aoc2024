import Util.readFile

import scala.annotation.tailrec

@main def day09(): Unit = {

  val input = readFile("resources/day09").head.map(_ - '0')

  val initState: List[(Option[Int], Int)] = input.zipWithIndex.map { case (x, i) =>
    if (i % 2 == 0) Some(i / 2) -> x
    else Option.empty[Int] -> x
  }.toList

  def getChecksum(xs: List[Int]) = xs.map(BigInt.apply).zipWithIndex.map(_ * _).sum

  // Part 1

  @tailrec
  def move(xs: List[(Option[Int], Int)], left: List[Int] = Nil, window: Int = 0): List[Int] = xs match
    case Nil                                 => left
    case (Some(x), i) :: tail if window == 0 => move(xs.tail, left ++ List.fill(i)(x))
    case (None, i) :: tail                   => move(xs.tail, left, window + i)
    case _ =>
      xs.last match
        case (None, i) =>
          move(xs.dropRight(1), left, window)

        case (Some(x), i) if i > window =>
          move(xs.dropRight(1) :+ (Some(x) -> (i - window)), left ++ List.fill(window)(x))

        case (Some(x), i) =>
          move(xs.dropRight(1), left ++ List.fill(i)(x), window - i)

  val result1 = getChecksum(move(initState))

  println(result1)

  // Part 2

  @tailrec
  def moveWhole(
      xs: List[(Option[Int], Int)],
      left: List[Int] = Nil,
      right: List[Int] = Nil
  ): List[Int] = xs match
    case Nil                  => left ++ right
    case (Some(x), i) :: tail => moveWhole(xs.tail, left ++ List.fill(i)(x), right)
    case _ =>
      xs.last match
        case (None, i) => moveWhole(xs.dropRight(1), left, List.fill(i)(0) ++ right)
        case (Some(x), i) =>
          val prefix = xs.takeWhile { block => block._1.isDefined || block._2 < i }
          xs.drop(prefix.length) match
            case Nil => moveWhole(xs.dropRight(1), left, List.fill(i)(x) ++ right)
            case (_, j) :: ys if j == i =>
              moveWhole(
                prefix ++ (xs.last :: xs.drop(prefix.length + 1).dropRight(1)),
                left,
                List.fill(i)(0) ++ right
              )
            case (_, j) :: ys =>
              moveWhole(
                prefix ++ (xs.last :: (None -> (j - i)) :: xs.drop(prefix.length + 1).dropRight(1)),
                left,
                List.fill(i)(0) ++ right
              )

  val result2 = getChecksum(moveWhole(initState))

  println(result2)
}
