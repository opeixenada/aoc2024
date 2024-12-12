import org.scalatest.flatspec.AnyFlatSpec

class GardenGroupsTests extends AnyFlatSpec {

  "regions" should "be constructed for one type" in {
    val input =
      """
        |AAAA
        |""".stripMargin.split('\n').toList.filterNot(_.isBlank)

    val regions = GardenGroups(input).regions

    assert(regions.size == 1)
    assert(regions.find(_._1 == 'A').get._2.size == 4)
  }

  it should "be constructed for 4 types" in {
    val input =
      """
        |AAAA
        |BBCD
        |BBCC
        |EEEC
        |""".stripMargin.split('\n').toList.filterNot(_.isBlank)

    val regions = GardenGroups(input).regions

    assert(regions.size == 5)
    assert(regions.find(_._1 == 'A').get._2.size == 4)
    assert(regions.find(_._1 == 'B').get._2.size == 4)
    assert(regions.find(_._1 == 'C').get._2.size == 4)
    assert(regions.find(_._1 == 'D').get._2.size == 1)
    assert(regions.find(_._1 == 'E').get._2.size == 3)
  }

  "getPerimeter" should "handle a row" in {
    val input =
      """
        |AAAA
        |""".stripMargin.split('\n').toList.filterNot(_.isBlank)

    val regions = GardenGroups(input).regions

    assert(GardenGroups.getPerimeter(regions.head._2) == 10)
  }

  "solvePart1" should "pass test 1" in {
    val input =
      """
        |AAAA
        |BBCD
        |BBCC
        |EEEC
        |""".stripMargin.split('\n').toList.filterNot(_.isBlank)

    assert(GardenGroups(input).solvePart1() == 140)
  }

  it should "pass test 2" in {
    val input =
      """
        |OOOOO
        |OXOXO
        |OOOOO
        |OXOXO
        |OOOOO
        |""".stripMargin.split('\n').toList.filterNot(_.isBlank)

    assert(GardenGroups(input).solvePart1() == 772)
  }

  it should "pass test 3" in {
    val input =
      """
        |RRRRIICCFF
        |RRRRIICCCF
        |VVRRRCCFFF
        |VVRCCCJFFF
        |VVVVCJJCFE
        |VVIVCCJJEE
        |VVIIICJJEE
        |MIIIIIJJEE
        |MIIISIJEEE
        |MMMISSJEEE
        |""".stripMargin.split('\n').toList.filterNot(_.isBlank)

    assert(GardenGroups(input).solvePart1() == 1930)
  }

  "getSides" should "handle a row" in {
    val input =
      """
        |AAAA
        |""".stripMargin.split('\n').toList.filterNot(_.isBlank)

    val regions = GardenGroups(input).regions

    assert(GardenGroups.getSides(regions.head._2) == 4)
  }

  it should "handle a complex region" in {
    val input =
      """
        |AAAA
        |BBCD
        |BBCC
        |EEEC
        |""".stripMargin.split('\n').toList.filterNot(_.isBlank)

    val regions = GardenGroups(input).regions

    assert(GardenGroups.getSides(regions.find(_._1 == 'C').get._2) == 8)
  }

  it should "handle a complex region 2" in {
    val input =
      """
        |EEEEE
        |EXXXX
        |EEEEE
        |EXXXX
        |EEEEE
        |""".stripMargin.split('\n').toList.filterNot(_.isBlank)

    val regions = GardenGroups(input).regions

    assert(GardenGroups.getSides(regions.find(_._1 == 'E').get._2) == 12)
  }

  it should "handle a complex region 3" in {
    val input =
      """
        |AAAAAA
        |AAABBA
        |AAABBA
        |ABBAAA
        |ABBAAA
        |AAAAAA
        |""".stripMargin.split('\n').toList.filterNot(_.isBlank)

    val regions = GardenGroups(input).regions

    assert(regions.size == 3)
    assert(GardenGroups.getSides(regions.find(_._1 == 'A').get._2) == 12)
  }

  "solvePart2" should "pass test 1" in {
    val input =
      """
        |AAAA
        |BBCD
        |BBCC
        |EEEC
        |""".stripMargin.split('\n').toList.filterNot(_.isBlank)

    assert(GardenGroups(input).solvePart2() == 80)
  }

  it should "pass test 2" in {
    val input =
      """
        |EEEEE
        |EXXXX
        |EEEEE
        |EXXXX
        |EEEEE
        |""".stripMargin.split('\n').toList.filterNot(_.isBlank)

    assert(GardenGroups(input).solvePart2() == 236)
  }

  it should "pass test 3" in {
    val input =
      """
        |AAAAAA
        |AAABBA
        |AAABBA
        |ABBAAA
        |ABBAAA
        |AAAAAA
        |""".stripMargin.split('\n').toList.filterNot(_.isBlank)

    assert(GardenGroups(input).solvePart2() == 368)
  }

  it should "pass test 4" in {
    val input =
      """
        |RRRRIICCFF
        |RRRRIICCCF
        |VVRRRCCFFF
        |VVRCCCJFFF
        |VVVVCJJCFE
        |VVIVCCJJEE
        |VVIIICJJEE
        |MIIIIIJJEE
        |MIIISIJEEE
        |MMMISSJEEE
        |""".stripMargin.split('\n').toList.filterNot(_.isBlank)

    assert(GardenGroups(input).solvePart2() == 1206)
  }

}
