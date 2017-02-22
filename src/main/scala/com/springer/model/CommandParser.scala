package com.springer.model

object CommandParser {
  val canvasPattern = "C (\\d*) (\\d*)".r
  val linePattern = "L (\\d*) (\\d*) (\\d*) (\\d*)".r
  val rectanglePattern = "R (\\d*) (\\d*) (\\d*) (\\d*)".r
  val bucketPattern = "B (\\d*) (\\d*) (.)".r

  def parseInput(input: String):Option[Command] = {
    implicit def stringToInt(s: String) = s.toInt

    input match {
      case canvasPattern(x, y) => Some(Canvas(x, y))
      case linePattern(x1, y1, x2, y2) => Some(Line(x1, y1, x2, y2))
      case rectanglePattern(x1, y1, x2, y2) => Some(Rectangle(x1, y1, x2, y2))
      case bucketPattern(x, y, c) => Some(BucketFill(x, y, c.charAt(0)))
      case "Q" => Some(Quit())
      case _ => Some(EmptyCommand())
    }
  }
}
