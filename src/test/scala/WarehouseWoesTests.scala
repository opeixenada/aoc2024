import org.scalatest.flatspec.AnyFlatSpec

class WarehouseWoesTests extends AnyFlatSpec {

  "rearrange" should "move the robot" in {
    val input = "#..@....#"
    val result = WarehouseWoes.rearrange(input)
    val expected = "#...@...#"

    assert(result == expected)
  }

  it should "stop before a wall" in {
    val input = "#..@#...#"
    val result = WarehouseWoes.rearrange(input)
    val expected = "#..@#...#"

    assert(result == expected)
  }

  it should "move a single box" in {
    val input = "#..@O...#"
    val result = WarehouseWoes.rearrange(input)
    val expected = "#...@O..#"

    assert(result == expected)
  }

  it should "move many boxes" in {
    val input = "#..@OOO...#"
    val result = WarehouseWoes.rearrange(input)
    val expected = "#...@OOO..#"

    assert(result == expected)
  }

  it should "not move boxed into a wall" in {
    val input = "#..@OOO#...#"
    val result = WarehouseWoes.rearrange(input)
    val expected = "#..@OOO#...#"

    assert(result == expected)
  }

  "move" should "process >" in {
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

    val solver = WarehouseWoes(input)

    val result = WarehouseWoes.move(solver.robot -> solver.map, '>')

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

    assert(result._1 == (3, 2))
    assert(result._2 == WarehouseWoes(expectedMap).map)
  }

  it should "process <" in {
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

    val solver = WarehouseWoes(input)

    val result = WarehouseWoes.move(solver.robot -> solver.map, '<')

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

    assert(result._1 == (2, 2))
    assert(result._2 == WarehouseWoes(expectedMap).map)
  }

  it should "process v" in {
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

    val solver = WarehouseWoes(input)

    val result = WarehouseWoes.move(solver.robot -> solver.map, 'v')

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

    assert(result._1 == (3, 3))
    assert(result._2 == WarehouseWoes(expectedMap).map)
  }

  it should "process ^" in {
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

    val solver = WarehouseWoes(input)

    val result = WarehouseWoes.move(solver.robot -> solver.map, '^')

    val expected =
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

    assert(result._1 == (3, 2))
    assert(result._2 == WarehouseWoes(expected).map)
  }

  "transpose" should "turn verticals to horizontals" in {
    val input =
      """
        |@BCD
        |EFGH
        |""".stripMargin.split('\n').toList.filterNot(_.isBlank)

    val solver = WarehouseWoes(input)

    val result = WarehouseWoes.transpose(solver.map)

    val expected =
      """
        |@E
        |BF
        |CG
        |DH
        |""".stripMargin.split('\n').toList.filterNot(_.isBlank)

    assert(result == WarehouseWoes(expected).map)
  }

  "turnBack" should "restore warehouse" in {
    val input =
      """
        |@BCD
        |EFGH
        |""".stripMargin.split('\n').toList.filterNot(_.isBlank)

    val solver = WarehouseWoes(input)
    val result = WarehouseWoes.turnBack(WarehouseWoes.transpose(solver.map))

    assert(result == solver.map)
  }

  "process" should "apply all instructions in case 1" in {
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

    val solver = WarehouseWoes(input)
    val result = WarehouseWoes.process(solver.robot, solver.map, solver.instructions)

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

    assert(result._2 == WarehouseWoes(expected).map)
  }

  it should "apply all instructions in case 2" in {
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

    val solver = WarehouseWoes(input)
    val result = WarehouseWoes.process(solver.robot, solver.map, solver.instructions)

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

    assert(result._2 == WarehouseWoes(expected).map)
  }
}
