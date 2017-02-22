package com.springer.model

import org.scalatest._
import scala.util.{Success, Try}


class CommandParserTest extends FunSuite with Matchers {

  test("It parses input and returns successful commands") {
    CommandParser.parseInput("C 20 20") shouldBe Some(Canvas(20, 20))
    CommandParser.parseInput("L 1 1 4 1") shouldBe Some(Line(1, 1, 4, 1))
    CommandParser.parseInput("R 1 1 4 4") shouldBe Some(Rectangle(1, 1, 4, 4))
    CommandParser.parseInput("B 1 1 K") shouldBe Some(BucketFill(1, 1, 'K'))
    CommandParser.parseInput("Q") shouldBe Some(Quit())
  }

  test("Parser should return none for illegal input") {
    CommandParser.parseInput("C gsg20 20").get shouldBe EmptyCommand()
    CommandParser.parseInput("T 1 1 4 1 2 2").get shouldBe EmptyCommand()
    CommandParser.parseInput("abasd234").get shouldBe EmptyCommand()
    CommandParser.parseInput("R").get shouldBe EmptyCommand()
    CommandParser.parseInput("L").get shouldBe EmptyCommand()
  }

}
