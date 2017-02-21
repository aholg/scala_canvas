package com.springer.model

import com.springer.factories.Converter
import com.springer.helpers.CustomMatchers
import org.scalatest._

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
    val expected = Converter.stringToMatrix(testCase._3.mkString("\n"))
    withClue(s"Test case: ${testCase._1}") {
      command.execute(matrix) should haveSameDimensionsAs(expected)
      command.execute(matrix) should haveSameContentAs(expected)
    }
  }
}


