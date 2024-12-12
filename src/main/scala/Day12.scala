import GardenGroups.*
import Util.readFile

@main def day12(): Unit = {
  val input = readFile("resources/day12")
  val solver = GardenGroups(input)
  println(solver.solvePart1())
  println(solver.solvePart2())
}

object GardenGroups {

  /** Single plot with its type (character) and coordinates. */
  private type Plot = (Char, (Int, Int))

  /** Region of connected plots with the same type. */
  private type Region = (Char, List[(Int, Int)])

  /** Coordinate in 2D space. */
  private type Coordinate = (Int, Int)

  /** List of cardinal directions for adjacent plot checking. */
  private val directions = List((1, 0), (-1, 0), (0, 1), (0, -1))

  /** Determines if two coordinates are bordering each other. */
  private def isBordering(p1: Coordinate)(p2: Coordinate): Boolean = {
    val dx = (p1._1 - p2._1).abs
    val dy = (p1._2 - p2._2).abs
    (dx == 1 && dy == 0) || (dx == 0 && dy == 1)
  }

  /** Expands the map by adding a new plot and merging it with any adjacent matching regions. */
  private def expand(map: List[Region], plot: Plot): List[Region] = {
    val (char, coord) = plot

    (char -> List(coord)) :: map.map {
      case (ch, coords) if ch == char && coords.exists(isBordering(coord)) => ch -> (coord :: coords)
      case other                                                           => other
    }
  }

  /** Reduces the map by merging overlapping regions of the same type. */
  private def reduce(map: List[Region], plot: Plot): List[Region] = {
    val matching = map.filter(r => r._1 == plot._1 && r._2.contains(plot._2))
    val merged = matching.head._1 -> matching.flatMap(_._2).distinct
    merged :: map.filterNot(r => r._1 == plot._1 && r._2.contains(plot._2))
  }

  /** Calculates the perimeter of a list of coordinates. */
  val getPerimeter: List[Coordinate] => Int = coords =>
    coords.map { coord =>
      directions.count { case (dx, dy) =>
        !coords.contains((coord._1 + dx, coord._2 + dy))
      }
    }.sum

  /** Calculates the number of distinct sides in a list of coordinates.
    *
    * This function analyzes both horizontal and vertical edges to determine the total number of distinct sides in the
    * shape formed by the plots with the given coordinates.
    */
  val getSides: List[Coordinate] => Int = coords => {

    /** Helper function to calculate edges in a given direction. */
    def getEdges(toEdges: Coordinate => List[(Int, Int, Int)]) = {
      val allEdges = coords.flatMap(toEdges)
      val groupedEdges = allEdges.groupBy { case (a, b, _) => (a, b) }
      val singleEdges = groupedEdges.filter(_._2.size == 1).values.flatten.toList

      singleEdges
        .groupBy { case (_, b, dir) => (b, dir) }
        .map { case (_, edges) =>
          val xs = edges.map(_._1).sorted
          xs.zip(xs.tail).count { case (x, y) => y - x != 1 } + 1
        }
        .sum
    }

    val horizontal = getEdges(coord => List((coord._1, coord._2, 1), (coord._1, coord._2 + 1, 0)))
    val vertical = getEdges(coord => List((coord._2, coord._1, 1), (coord._2, coord._1 + 1, 0)))

    horizontal + vertical
  }

  /** Calculates the total cost for all regions using the provided cost function. */
  private def getCost(f: List[Coordinate] => Int)(map: List[Region]): Int =
    map.map { case (_, coords) => coords.size * f(coords) }.sum
}

class GardenGroups(input: List[String]) {

  /** Regions parsed from the input. */
  val regions: List[Region] = {
    val plots = for {
      (row, x) <- input.zipWithIndex
      (char, y) <- row.zipWithIndex
    } yield (char, (x, y))

    plots.foldLeft(List.empty[Region]) { (map, plot) =>
      reduce(expand(map, plot), plot)
    }
  }

  def solvePart1(): Any = getCost(getPerimeter)(regions)
  def solvePart2(): Any = getCost(getSides)(regions)
}
