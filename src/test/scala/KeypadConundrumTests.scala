import Util.withTimeLogging
import org.scalatest.flatspec.AnyFlatSpec

class KeypadConundrumTests extends AnyFlatSpec {

  "getSequencesForNumeric" should "pass test 1" in {
    assert(
      KeypadConundrum.findSequencesForNumericKeypad("029A").toSet == Set(
        "<A^A>^^AvvvA",
        "<A^A^>^AvvvA",
        "<A^A^^>AvvvA"
      )
    )
  }

  "mySequence" should "pass test 1" in {
    assert(
      KeypadConundrum.findMySequenceLength(2)(
        "029A"
      ) == "<vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A".length
    )
  }

  it should "pass test 2" in {
    assert(
      KeypadConundrum.findMySequenceLength(2)("980A")
        == "<v<A>>^AAAvA^A<vA<AA>>^AvAA<^A>A<v<A>A>^AAAvA<^A>A<vA>^A<A>A".length
    )
  }

  it should "pass test 3" in {
    assert(
      KeypadConundrum.findMySequenceLength(2)("179A")
        == "<v<A>>^A<vA<A>>^AAvAA<^A>A<v<A>>^AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A".length
    )
  }

  it should "pass test 4" in {
    assert(
      KeypadConundrum.findMySequenceLength(2)("456A")
        == "<v<A>>^AA<vA<A>>^AAvAA<^A>A<vA>^A<A>A<vA>^A<A>A<v<A>A>^AAvA<^A>A".length
    )
  }

  it should "pass test 5" in {
    assert(
      KeypadConundrum.findMySequenceLength(2)("379A")
        == "<v<A>>^AvA^A<vA<AA>>^AAvA<^A>AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A".length
    )
  }

  "solvePart1" should "pass test 1" in {
    val input =
      """
        |029A
        |980A
        |179A
        |456A
        |379A
        |""".stripMargin.split('\n').toList.filterNot(_.isBlank)

    assert(KeypadConundrum(input).solvePart1() == 126384)
  }

  it should "pass test 2" in {
    val input =
      """
        |805A
        |983A
        |149A
        |413A
        |582A
        |""".stripMargin.split('\n').toList.filterNot(_.isBlank)

    assert(KeypadConundrum(input).solvePart1() == 202648)
  }

  "State.expand" should "pass test with times=1" in {
    val result = KeypadConundrum.expand(s = "<A^A>^^AvvvA", depth = 1)
    assert(result == "v<<A>>^A<A>AvA<^AA>A<vAAA>^A".length)
  }

  it should "pass test with times=2" in {
    val result = KeypadConundrum.expand(s = "<A^A>^^AvvvA", depth = 2)
    assert(result == "<vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A".length)
  }

  it should "terminate with times=25" in {
    for {
      n <- 1 to 25
    } yield {
      println(s"n = $n")
      val result = withTimeLogging { KeypadConundrum.expand(s = "<A^A>^^AvvvA", depth = n) }
      assert(result > 0)
    }
  }
}
