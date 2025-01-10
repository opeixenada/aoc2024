import org.scalatest.flatspec.AnyFlatSpec

class MonkeyMarketTests extends AnyFlatSpec {

  "next" should "calculate secret numbers" in {
    val xs = List(15887950, 16495136, 527345, 704524, 1553684, 12683156, 11100544, 12249484, 7753432, 5908254).map(
      BigInt.apply
    )

    xs.zip(xs.tail).foreach { case (a, b) =>
      assert(MonkeyMarket.next(a) == b)
    }
  }

  it should "calculate 2000th secret numbers" in {
    val cases = Map(
      1 -> 8685429,
      10 -> 4700978,
      100 -> 15273692,
      2024 -> 8667524
    ).map { case (a, b) => BigInt(a) -> BigInt(b) }

    cases.foreach { case (a, b) =>
      assert(MonkeyMarket.get2kth(a) == b)
    }
  }

  "getPrices" should "foo" in {
    val x = BigInt(123)
    val expected = List(0 -> -3, 6 -> 6, 5 -> -1, 4 -> -1, 4 -> 0, 6 -> 2, 4 -> -2, 4 -> 0, 2 -> -2)
    assert(MonkeyMarket.getPrices(9, x) == expected)
  }

}
