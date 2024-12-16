import WarehouseWoes.*
import org.scalatest.flatspec.AnyFlatSpec

class WarehouseWoesTests extends AnyFlatSpec {

  "parseExpandedInput" should "expand the map" in {
    val input =
      """
        |##########
        |#..O..O.O#
        |#......O.#
        |#.OO..O.O#
        |#..O@..O.#
        |#O#..O...#
        |#O..O..O.#
        |#.OO.O.OO#
        |#....O...#
        |##########
        |""".stripMargin.split('\n').toList.filterNot(_.isBlank)

    val expected =
      """
        |####################
        |##....[]....[]..[]##
        |##............[]..##
        |##..[][]....[]..[]##
        |##....[]@.....[]..##
        |##[]##....[]......##
        |##[]....[]....[]..##
        |##..[][]..[]..[][]##
        |##........[]......##
        |####################
        |""".stripMargin.split('\n').toList.filterNot(_.isBlank)

    assert(parseExpandedInput(input)._2 == parseInput(expected)._2)
  }

  "move" should "process > for small boxes" in {
    val input =
      """
        |########
        |#..O.O.#
        |##@.O..#
        |#...O..#
        |#.#.O..#
        |#...O..#
        |#......#
        |########
        |""".stripMargin.split('\n').toList.filterNot(_.isBlank)

    val expectedMap =
      """
        |########
        |#..O.O.#
        |##.@O..#
        |#...O..#
        |#.#.O..#
        |#...O..#
        |#......#
        |########
        |""".stripMargin.split('\n').toList.filterNot(_.isBlank)

    val (map, _) = parseInput(input)
    val result = move()(map, '>')

    assert(result.map(_.mkString) == expectedMap)
  }

  it should "process < for small boxes" in {
    val input =
      """
        |########
        |#..O.O.#
        |##.@O..#
        |#...O..#
        |#.#.O..#
        |#...O..#
        |#......#
        |########
        |""".stripMargin.split('\n').toList.filterNot(_.isBlank)

    val expectedMap =
      """
        |########
        |#..O.O.#
        |##@.O..#
        |#...O..#
        |#.#.O..#
        |#...O..#
        |#......#
        |########
        |""".stripMargin.split('\n').toList.filterNot(_.isBlank)

    val (map, _) = parseInput(input)
    val result = move()(map, '<')

    assert(result.map(_.mkString) == expectedMap)
  }

  it should "process ^ for small boxes" in {
    val input =
      """
        |########
        |#..O.O.#
        |##..O..#
        |#..@O..#
        |#.#.O..#
        |#...O..#
        |#......#
        |########
        |""".stripMargin.split('\n').toList.filterNot(_.isBlank)

    val expectedMap =
      """
        |########
        |#..O.O.#
        |##.@O..#
        |#...O..#
        |#.#.O..#
        |#...O..#
        |#......#
        |########
        |""".stripMargin.split('\n').toList.filterNot(_.isBlank)

    val (map, _) = parseInput(input)
    val result = move()(map, '^')

    assert(result.map(_.mkString) == expectedMap)
  }

  it should "process v for small boxes" in {
    val input =
      """
        |########
        |#..O.O.#
        |##.@O..#
        |#...O..#
        |#.#.O..#
        |#...O..#
        |#......#
        |########
        |""".stripMargin.split('\n').toList.filterNot(_.isBlank)

    val expectedMap =
      """
        |########
        |#..O.O.#
        |##..O..#
        |#..@O..#
        |#.#.O..#
        |#...O..#
        |#......#
        |########
        |""".stripMargin.split('\n').toList.filterNot(_.isBlank)

    val (map, _) = parseInput(input)
    val result = move()(map, 'v')

    assert(result.map(_.mkString) == expectedMap)
  }

  it should "process < for big boxes" in {
    val input =
      """
        |##############
        |##......##..##
        |##..........##
        |##....[][]@.##
        |##....[]....##
        |##..........##
        |##############
        |""".stripMargin.split('\n').toList.filterNot(_.isBlank)

    val expectedMap =
      """
        |##############
        |##......##..##
        |##..........##
        |##...[][]@..##
        |##....[]....##
        |##..........##
        |##############
        |""".stripMargin.split('\n').toList.filterNot(_.isBlank)

    val (map, _) = parseInput(input)
    val result = move(processLineForBigBoxes)(map, '<')

    assert(result.map(_.mkString) == expectedMap)
  }

  it should "process ^ for big boxes" in {
    val input =
      """
        |##############
        |##......##..##
        |##...[][]...##
        |##....[]....##
        |##.....@....##
        |##..........##
        |##############
        |""".stripMargin.split('\n').toList.filterNot(_.isBlank)

    val expectedMap =
      """
        |##############
        |##......##..##
        |##...[][]...##
        |##....[]....##
        |##.....@....##
        |##..........##
        |##############
        |""".stripMargin.split('\n').toList.filterNot(_.isBlank)

    val (map, _) = parseInput(input)
    val result = move(processLineForBigBoxes)(map, '^')

    assert(result.map(_.mkString) == expectedMap)
  }

  it should "process > for big boxes" in {
    val input =
      """
        |####################
        |##[]..[]....[]..[]##
        |##[]..........[]..##
        |##.@[][]....[]..[]##
        |##...[]....[].[]..##
        |##..##....[]......##
        |##...[].......[]..##
        |##.....[]..[].[][]##
        |##........[]......##
        |####################
        |""".stripMargin.split('\n').toList.filterNot(_.isBlank)

    val expectedMap =
      """
        |####################
        |##[]..[]....[]..[]##
        |##[]..........[]..##
        |##..@[][]...[]..[]##
        |##...[]....[].[]..##
        |##..##....[]......##
        |##...[].......[]..##
        |##.....[]..[].[][]##
        |##........[]......##
        |####################
        |""".stripMargin.split('\n').toList.filterNot(_.isBlank)

    val (map, _) = parseInput(input)
    val result = move(processLineForBigBoxes)(map, '>')

    assert(result.map(_.mkString) == expectedMap)
  }

  "process" should "apply all instructions for small boxes: case 1" in {
    val input =
      """|########
         |#..O.O.#
         |##@.O..#
         |#...O..#
         |#.#.O..#
         |#...O..#
         |#......#
         |########
         |
         |<^^>>>vv<v>>v<<
         |""".stripMargin.split('\n').toList

    val expected =
      """
        |########
        |#....OO#
        |##.....#
        |#.....O#
        |#.#O@..#
        |#...O..#
        |#...O..#
        |########
        |""".stripMargin.split('\n').toList.filterNot(_.isBlank)

    val result = process(parseInput(input))

    assert(result == parseInput(expected)._1)
  }

  it should "apply all instructions for small boxes: case 2" in {
    val input =
      """|##########
         |#..O..O.O#
         |#......O.#
         |#.OO..O.O#
         |#..O@..O.#
         |#O#..O...#
         |#O..O..O.#
         |#.OO.O.OO#
         |#....O...#
         |##########
         |
         |<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
         |vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
         |><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
         |<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
         |^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
         |^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
         |>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
         |<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
         |^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
         |v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^
         |""".stripMargin.split('\n').toList

    val expected =
      """
        |##########
        |#.O.O.OOO#
        |#........#
        |#OO......#
        |#OO@.....#
        |#O#.....O#
        |#O.....OO#
        |#O.....OO#
        |#OO....OO#
        |##########
        |""".stripMargin.split('\n').toList.filterNot(_.isBlank)

    val result = process(parseInput(input))

    assert(result == parseInput(expected)._1)
  }

  it should "apply all instructions for big boxes: case 1" in {
    val input =
      """|#######
         |#...#.#
         |#.....#
         |#..OO@#
         |#..O..#
         |#.....#
         |#######
         |
         |<vv<<^^<<^^
         |""".stripMargin.split('\n').toList

    val expected =
      """
        |##############
        |##...[].##..##
        |##...@.[]...##
        |##....[]....##
        |##..........##
        |##..........##
        |##############
        |""".stripMargin.split('\n').toList.filterNot(_.isBlank)

    val result = process(parseExpandedInput(input), processLineForBigBoxes)

    assert(result == parseInput(expected)._1)
  }

  it should "apply all instructions for big boxes: case 2" in {
    val input =
      """|##########
         |#..O..O.O#
         |#......O.#
         |#.OO..O.O#
         |#..O@..O.#
         |#O#..O...#
         |#O..O..O.#
         |#.OO.O.OO#
         |#....O...#
         |##########
         |
         |<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
         |vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
         |><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
         |<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
         |^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
         |^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
         |>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
         |<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
         |^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
         |v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^
         |""".stripMargin.split('\n').toList

    val expected =
      """
        |####################
        |##[].......[].[][]##
        |##[]...........[].##
        |##[]........[][][]##
        |##[]......[]....[]##
        |##..##......[]....##
        |##..[]............##
        |##..@......[].[][]##
        |##......[][]..[]..##
        |####################
        |""".stripMargin.split('\n').toList.filterNot(_.isBlank)

    val result = process(parseExpandedInput(input), processLineForBigBoxes)

    assert(result.map(_.mkString) == parseInput(expected)._1.map(_.mkString))
  }

}
