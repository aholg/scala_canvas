package com.springer.helpers

import com.springer.model.Paint
import com.springer.model.Paint._
import org.scalatest.matchers.{MatchResult, Matcher}

import scala.collection.mutable.ArrayBuffer
import scala.util.Try

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

