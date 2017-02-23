package com.springer.model

import org.scalatest._
import scala.util.{Success, Try}


class CommandParserTest extends FunSuite with Matchers {

  test("It parses input and returns successful commands") {
    CommandParser.parseInput("C 20 20") shouldBe Canvas(20, 20)
    CommandParser.parseInput("L 1 1 4 1") shouldBe Line(1, 1, 4, 1)
    CommandParser.parseInput("R 1 1 4 4") shouldBe Rectangle(1, 1, 4, 4)
    CommandParser.parseInput("B 1 1 K") shouldBe BucketFill(1, 1, 'K')
    CommandParser.parseInput("Q") shouldBe Quit()
  }

  test("Parser should return none for illegal input") {
    CommandParser.parseInput("C gsg20 20") shouldBe EmptyCommand()
    CommandParser.parseInput("T 1 1 4 1 2 2") shouldBe EmptyCommand()
    CommandParser.parseInput("abasd234") shouldBe EmptyCommand()
    CommandParser.parseInput("R") shouldBe EmptyCommand()
    CommandParser.parseInput("L") shouldBe EmptyCommand()
  }

}
