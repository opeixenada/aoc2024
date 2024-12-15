import Util.readFile

import java.io.PrintWriter
import scala.annotation.tailrec
import scala.util.Using

@main def day14(): Unit = {
  val input = readFile("resources/day14")
  val solver = RestroomRedoubt(input)
  println(solver.solvePart1())
  println(solver.solvePart2())
}

class RestroomRedoubt(input: List[String]) {

  private val w = 101
  private val h = 103
  private val seconds = 100

  case class Robot(x: Int, y: Int, vx: Int, vy: Int) {
    def move(i: Int): Robot = this.copy(x = ((x + (i * vx)) % w + w) % w, y = ((y + (i * vy)) % h + h) % h)

    def getQuadrant: Option[Int] = this match
      case _ if x < (w / 2) && y < (h / 2) => Some(1)
      case _ if x < (w / 2) && y > (h / 2) => Some(2)
      case _ if x > (w / 2) && y < (h / 2) => Some(3)
      case _ if x > (w / 2) && y > (h / 2) => Some(4)
      case _                               => None
  }

  private val robots: List[Robot] = input.map { s =>
    val ns = "(-?\\d)+".r.findAllMatchIn(s).map(_.matched.toInt).toIndexedSeq
    Robot(ns(0), ns(1), ns(2), ns(3))
  }

  private def paint(writer: PrintWriter)(row: List[(Int, List[Robot])]): Unit = {

    val strings = row.map { case (i, rs) =>
      (s"[$i]" ::
        (0 until h).map { j =>
          (0 until w)
            .map { i =>
              if (rs.exists(r => r.x == i && r.y == j)) '█' else ' '
            }
            .mkString("")
        }.toList).map(_.padTo(w, ' '))
    }

    @tailrec
    def printLines(ss: List[List[String]]): Unit = ss.head match
      case Nil => ()
      case _ =>
        val s = ss.map(_.head).mkString("", "", "\n")
        writer.write(s)
        printLines(ss.map(_.tail))

    printLines(strings)
  }

  @tailrec
  private def paintAndMove(
      writer: PrintWriter
  )(rs: List[Robot], i: Int, acc: List[(Int, List[Robot])]): Unit = {
    val n = 10

    if (acc.size == n) {
      paint(writer)(acc)
      paintAndMove(writer)(rs, i, Nil)
    } else {
      val moved = rs.map(_.move(1))
      if (moved == robots) paint(writer)(acc)
      else paintAndMove(writer)(moved, i + 1, acc.appended(i + 1 -> moved))
    }
  }

  def solvePart1(): Any =
    robots.map(_.move(seconds)).groupBy(_.getQuadrant).filterNot(_._1.isEmpty).values.map(_.size).product

  def solvePart2(): Any = {
    Using(new PrintWriter("RestroomRedoubt.txt")) { writer =>
      val moved = robots.map(_.move(1))
      paintAndMove(writer)(moved, 1, List(0 -> robots, 1 -> moved))
    }

    "Go look for the tree in the file now!"
  }
}
