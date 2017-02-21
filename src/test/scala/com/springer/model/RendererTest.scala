package com.springer.model

import com.springer.helpers.CustomMatchers
import org.scalatest.{BeforeAndAfter, FunSuite, Matchers}

class RendererTest extends FunSuite with Matchers with BeforeAndAfter with CustomMatchers{

  test("Creates a string representation from a canvas board. ") {
    val canvas = Array.fill[Char](4, 20)(' ')
    val canvasWithLine: Paint.Matrix = Line(0, 1, 1, 1).execute(Array.fill[Char](4, 20)(' ')).get
    val testCases = Seq(
        ("Empty canvas",
          Seq(
            "----------------------",
            "|                    |",
            "|                    |",
            "|                    |",
            "|                    |",
            "----------------------"),
          canvas),
        ("Canvas with lines",
          Seq(
            "----------------------",
            "|                    |",
            "|xx                  |",
            "|                    |",
            "|                    |",
            "----------------------"),
          canvasWithLine))

        testCases.foreach{ testCase =>
          val expected = testCase._2.mkString("\n")
          val result: String= Renderer.createCanvasBoardString(testCase._3)
          result should equal (expected)
        }
  }
}
