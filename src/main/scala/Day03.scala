import Util.readFile

@main def day03(): Unit = {

  val input = readFile("resources/day03")

  // Part 1

  val result1 = input.flatMap { line =>
    """mul\(\d+,\d+\)""".r
      .findAllIn(line)
      .toList
      .map { exp =>
        """\d+""".r.findAllIn(exp).map(_.toInt).product
      }
  }.sum

  println(result1)

  // Part 2

  val result2 = input
    .flatMap { line =>
      """mul\(\d+,\d+\)|do\(\)|don't\(\)""".r
        .findAllIn(line)
        .toList
    }
    .foldLeft((List.empty[String], true)) { case ((xs, flag), s) =>
      s match
        case "don't()" => (xs, false)
        case "do()"    => (xs, true)
        case _ if flag => (s :: xs, flag)
        case _         => (xs, flag)
    }
    ._1
    .map { exp =>
      """\d+""".r.findAllIn(exp).map(_.toInt).product
    }
    .sum

  println(result2)
}
