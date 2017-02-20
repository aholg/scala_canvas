package com.springer.model

object CommandParser {
  val canvasPattern = "C ([1-9][0-9]*) ([1-9][0-9]*)".r
  val linePattern = "L ([1-9][0-9]*) ([1-9][0-9]*) ([1-9][0-9]*) ([1-9][0-9]*)".r
  val rectanglePattern = "R ([1-9][0-9]*) ([1-9][0-9]*) ([1-9][0-9]*) ([1-9][0-9]*)".r
  val bucketPattern = "B ([1-9][0-9]*) ([1-9][0-9]*) (.)".r

  def parseInput(input: String):Option[Command] = {
    implicit def stringToInt(s: String) = s.toInt

    input match {
      case canvasPattern(x, y) => Some(Canvas(x, y))
      case linePattern(x1, y1, x2, y2) => Some(Line(x1, y1, x2, y2))
      case rectanglePattern(x1, y1, x2, y2) => Some(Rectangle(x1, y1, x2, y2))
      case bucketPattern(x, y, c) => Some(BucketFill(x, y, c.charAt(0)))
      case "Q" => Some(Quit())
      case _ => None
    }
  }
}
