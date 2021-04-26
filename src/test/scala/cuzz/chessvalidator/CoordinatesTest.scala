package cuzz.chessvalidator

import org.scalatest.{FlatSpec, Matchers}

class CoordinatesTest extends FlatSpec with Matchers {
  "Coordinates" should "be correctly represented as a string" in {
    val coordinates: Coordinates = Coordinates(1, 5)
    coordinates.toString shouldBe "[1, 5]"
  }
}
