import org.scalatest.flatspec.AnyFlatSpec

class HoofItTests extends AnyFlatSpec {

  "solvePart1" should "pass test 1" in {
    val input =
      """
        |0123
        |1234
        |8765
        |9876
        |""".stripMargin.split('\n').toList.filterNot(_.isBlank)

    assert(HoofIt(input).solvePart1() == 1)
  }

  it should "pass test 2" in {
    val input =
      """
        |...0...
        |...1...
        |...2...
        |6543456
        |7.....7
        |8.....8
        |9.....9
        |""".stripMargin.split('\n').toList.filterNot(_.isBlank)

    assert(HoofIt(input).solvePart1() == 2)
  }

  it should "pass test 3" in {
    val input =
      """
        |..90..9
        |...1.98
        |...2..7
        |6543456
        |765.987
        |876....
        |987....
        |""".stripMargin.split('\n').toList.filterNot(_.isBlank)

    assert(HoofIt(input).solvePart1() == 4)
  }

  it should "pass test 4" in {
    val input =
      """
        |10..9..
        |2...8..
        |3...7..
        |4567654
        |...8..3
        |...9..2
        |.....01
        |""".stripMargin.split('\n').toList.filterNot(_.isBlank)

    assert(HoofIt(input).solvePart1() == 3)
  }

  it should "pass test 5" in {
    val input =
      """
        |89010123
        |78121874
        |87430965
        |96549874
        |45678903
        |32019012
        |01329801
        |10456732
        |""".stripMargin.split('\n').toList.filterNot(_.isBlank)

    assert(HoofIt(input).solvePart1() == 36)
  }

  "solvePart2" should "pass test 1" in {
    val input =
      """
        |.....0.
        |..4321.
        |..5..2.
        |..6543.
        |..7..4.
        |..8765.
        |..9....
        |""".stripMargin.split('\n').toList.filterNot(_.isBlank)

    assert(HoofIt(input).solvePart2() == 3)
  }

  it should "pass test 2" in {
    val input =
      """
        |..90..9
        |...1.98
        |...2..7
        |6543456
        |765.987
        |876....
        |987....
        |""".stripMargin.split('\n').toList.filterNot(_.isBlank)

    assert(HoofIt(input).solvePart2() == 13)
  }

  it should "pass test 3" in {
    val input =
      """
        |012345
        |123456
        |234567
        |345678
        |4.6789
        |56789.
        |""".stripMargin.split('\n').toList.filterNot(_.isBlank)

    assert(HoofIt(input).solvePart2() == 227)
  }

  it should "pass test 4" in {
    val input =
      """
        |89010123
        |78121874
        |87430965
        |96549874
        |45678903
        |32019012
        |01329801
        |10456732
        |""".stripMargin.split('\n').toList.filterNot(_.isBlank)

    assert(HoofIt(input).solvePart2() == 81)
  }
}
