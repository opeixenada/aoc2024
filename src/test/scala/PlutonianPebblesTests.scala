import org.scalatest.flatspec.AnyFlatSpec

class PlutonianPebblesTests extends AnyFlatSpec {

  "process" should "blink once" in {
    val blinks = 1
    val init = List(125, 17).map(x => PlutonianPebbles.Pebble(BigInt.apply(x), blinks))
    assert(PlutonianPebbles.process(init) == 3)
  }

  it should "blink twice" in {
    val blinks = 2
    val init = List(125, 17).map(x => PlutonianPebbles.Pebble(BigInt.apply(x), blinks))
    assert(PlutonianPebbles.process(init) == 4)
  }

  it should "blink 6 times" in {
    val blinks = 6
    val init = List(125, 17).map(x => PlutonianPebbles.Pebble(BigInt.apply(x), blinks))
    assert(PlutonianPebbles.process(init) == 22)
  }

  it should "blink 25 times" in {
    val blinks = 25
    val init = List(125, 17).map(x => PlutonianPebbles.Pebble(BigInt.apply(x), blinks))
    assert(PlutonianPebbles.process(init) == 55312)
  }

}
