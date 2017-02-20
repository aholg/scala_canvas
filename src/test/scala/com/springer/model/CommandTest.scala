package com.springer.model

import com.springer.factories.Converter
import com.springer.model.Paint.Matrix
import org.scalatest._
import org.scalatest.matchers.{MatchResult, Matcher}

import scala.collection.mutable.ArrayBuffer
import scala.util.{Failure, Success, Try}

class CommandTest extends FunSuite with Matchers with CustomMatchers with BeforeAndAfter with TryValues{
  var matrix: Paint.Matrix  = _

  before {
    matrix = Array.fill[Char](4, 20)(' ')
  }

  test("Canvas command returns a new canvas.") {
    val canvas = Canvas(4, 20)
    canvas.execute(null) should haveSameDimensionsAs(matrix)
    canvas.execute(null).success.value shouldBe matrix
  }

  test("Line command can add lines to a canvas.") {
    val testCases = Seq(
      (
        "Horizontal line",
        Line(0, 2, 5, 2), Seq(
          "                    ",
          "                    ",
          "xxxxxx              ",
          "                    ")),
      (
        "Vertical line",
        Line(0, 0, 0, 3), Seq(
          "x                   ",
          "x                   ",
          "x                   ",
          "x                   "))
    )

    testCases.foreach { testCase =>
      matrix = Array.fill[Char](4, 20)(' ')
      testCommand(testCase)
    }
  }

  test("Line command can add lines across other lines. ") {
    val testCases = Seq(
      (
        "Horizontal line",
        Line(0, 2, 5, 2), Seq(
        "                    ",
        "                    ",
        "xxxxxx              ",
        "                    ")),
      (
        "Vertical line",
        Line(0, 0, 0, 3), Seq(
        "x                   ",
        "x                   ",
        "xxxxxx              ",
        "x                   "))
    )
    testCases.foreach { testCase =>
      testCommand(testCase)
    }
  }

  test("Line command does not allow diagonal lines.") {
    val exception = {
      Line(1, 2, 3, 5).execute(null)
    }
    exception.failure.exception shouldBe an[IllegalArgumentException]
  }

  test("Commands do not allow out of canvas coordinates.") {
    val testCases = Seq(
      Line(21, 3, 24, 3),
      Line(19, 5, 19, 3),
      Rectangle(21, 3, 24, 3)
    )
    testCases.foreach { testCase =>
      testCase.execute(matrix).failure.exception shouldBe an[IllegalArgumentException]
    }
  }

  test("Rectangle command can add rectangle to canvas.") {
    val testCases = Seq(
      (
        "Correct Rectangle",
          Rectangle(1, 1, 5, 3), Seq(
              "                    ",
              " xxxxx              ",
              " x   x              ",
              " xxxxx              "))
    )
    testCases.foreach { testCase =>
     testCommand(testCase)
    }
  }

  private def testCommand(testCase: (String, Command, Seq[String])): Unit = {
    val command = testCase._2
    val expected = Converter.StringToMatrix(testCase._3.mkString("\n"))
    withClue(s"Test case: ${testCase._1}") {
      command.execute(matrix) should haveSameDimensionsAs(expected)
      command.execute(matrix) should haveSameContentAs(expected)
    }
  }
}

trait CustomMatchers {
  def checkDimensions(lhs: Paint.Matrix, rhs: Paint.Matrix): (Int, Int) = {
    val leftRows = lhs.size
    if (leftRows != rhs.size) {
      return (leftRows, lhs(0).size)
    }
    for (i <- 0 until lhs.size) {
      if (lhs(0).size != rhs(i).size) {
        return (leftRows, lhs(i).size)
      }
    }
    return (leftRows, lhs(0).size)
  }

  def haveSameDimensionsAs(expected: Paint.Matrix) = new Matcher[Try[Paint.Matrix]] {
    def apply(actual: Try[Paint.Matrix]) = {
      val (lhsRows, lhsCols) = checkDimensions(actual.get, expected)
      val expectedRows = expected.size
      val expectedCols = expected(0).size
      val result = MatchResult(
        lhsRows == expectedRows && lhsCols == expectedCols,
        s"""Matrix did not have the same dimensions, had: ($lhsRows, $lhsCols), expected: ($expectedRows, $expectedCols)"""",
        s"""Matrix had the same dimensions"""
      )
      result
    }
  }

  def checkContent(actual: Matrix, expected: Matrix): String = {
    val errors = ArrayBuffer.empty[String]
    for {
      x <- actual(0).indices
      y <- actual.indices
    } if (actual(y)(x) != expected(y)(x)) errors.append(s"Error in ($x,$y): want ${expected(y)(x)}, but got: ${actual(y)(x)}")
    if (errors.nonEmpty) errors.mkString("\n") else ""
  }

  def haveSameContentAs(expected: Paint.Matrix) = new Matcher[Try[Paint.Matrix]] {
    def apply(actual: Try[Paint.Matrix]) = {
      val errors = checkContent(actual.get, expected)
      MatchResult(
        errors.isEmpty,
        s"Errors:\n${errors}",
        "OK"
      )
    }
  }
}
