import GardenGroups.*
import Util.readFile

@main def day12(): Unit = {
  val input = readFile("resources/day12")
  val solver = GardenGroups(input)
  println(solver.solvePart1())
  println(solver.solvePart2())
}

object GardenGroups {
  private type Plot = (Char, (Int, Int))
  private type Region = (Char, List[(Int, Int)])

  private def isBordering(cs1: (Int, Int))(cs2: (Int, Int)): Boolean = {
    val (x1, y1) = cs1
    val (x2, y2) = cs2
    (x1 == x2 && (y1 - y2).abs == 1) || (y1 == y2 && (x1 - x2).abs == 1)
  }

  private val directions = List((1, 0), (-1, 0), (0, 1), (0, -1))

  private def isClear(cs: (Int, Int), css: List[(Int, Int)])(direction: (Int, Int)): Boolean =
    !css.contains((cs._1 + direction._1) -> (cs._2 + direction._2))

  private def expandMap(map: List[Region], plot: Plot): List[(Char, List[(Int, Int)])] = {
    val (ch, cs) = plot
    (ch -> List(cs)) :: map.map {
      case (ch1, coordinates) if ch == ch1 && coordinates.exists(isBordering(cs)) => ch -> (cs :: coordinates)
      case other                                                                  => other
    }
  }

  private def containsPlot(plot: Plot)(region: Region) = plot._1 == region._1 && region._2.contains(plot._2)

  private def mergePlots(regions: List[Region]): Region = regions.head._1 -> regions.flatMap(_._2).distinct

  private def reduceMap(map: List[Region], plot: Plot): List[Region] =
    mergePlots(map.filter(containsPlot(plot))) :: map.filterNot(containsPlot(plot))

  val getPerimeter: List[(Int, Int)] => Int = { coordinates =>
    coordinates.map { cs =>
      directions.count(isClear(cs, coordinates))
    }.sum
  }

  val getSides: List[(Int, Int)] => Int = { plotsCoordinates =>

    def getOuterEdges(xs: List[(Int, Int, Int)]) = xs
      .groupBy { case (a, b, _) => (a, b) }
      .filter(_._2.size == 1)
      .values
      .flatMap(_.headOption)
      .toList

    def countDirectedSides(xs: List[(Int, Int, Int)]): Int = xs
      .groupBy { case (a, b, direction) => (b, direction) }
      .map { (k, v) =>
        val ys = v.map(_._1).sorted

        val count = ys
          .zip(ys.tail)
          .count { (x, y) => (y - x) != 1 } + 1

        count
      }
      .sum

    def countSides(f: ((Int, Int)) => List[(Int, Int, Int)]): Int = countDirectedSides(
      getOuterEdges(plotsCoordinates.flatMap(f))
    )

    countSides({ case (x, y) =>
      List((x, y, 1), (x, y + 1, 0))
    }) + countSides { case (x, y) =>
      List((y, x, 1), (y, x + 1, 0))
    }
  }

  private def getCost(f: List[(Int, Int)] => Int)(map: List[Region]): Int = map.map { case (_, coordinates) =>
    coordinates.size * f(coordinates)
  }.sum
}

class GardenGroups(input: List[String]) {

  val regions: List[Region] = (for {
    (s, x) <- input.zipWithIndex
    (ch, y) <- s.zipWithIndex
  } yield (ch, (x, y))).foldLeft(List.empty[Region]) { case (map, plot) =>
    reduceMap(expandMap(map, plot), plot)
  }

  def solvePart1(): Any = getCost(getPerimeter)(regions)
  def solvePart2(): Any = getCost(getSides)(regions)
}
