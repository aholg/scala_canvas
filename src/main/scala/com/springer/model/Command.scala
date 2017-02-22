package com.springer.model

import com.springer.model.Paint.Matrix

import scala.util.{Failure, Success, Try}

case class IllegalArgumentException(msg: String) extends RuntimeException(msg)
case class NoCommandFoundError(msg: String) extends RuntimeException(msg)
abstract class Command extends {
  def execute(board: Paint.Matrix): Try[Paint.Matrix]
  def isWithinBoard(board: Paint.Matrix, x: Int, y: Int): Boolean = {
    Try(board(y)(x)) match {
      case Success(_) => true
      case Failure(_) => false
    }
  }
}

case class Canvas(rows: Int, cols: Int) extends Command {
  override def execute(board: Paint.Matrix): Try[Paint.Matrix] = {
    Success(Array.fill[Char](rows, cols)(' '))
  }
}

case class EmptyCommand() extends Command {
  override def execute(board: Paint.Matrix): Try[Paint.Matrix] = {
    Failure(NoCommandFoundError("No command found"))
  }
}

object Paint {
  type Matrix = Array[Array[Char]]
}

case class Line(x1: Int, y1: Int, x2: Int, y2: Int) extends Command {
  override def execute(board: Paint.Matrix): Try[Paint.Matrix] = {
    if(!isWithinBoard(board, x1, y1) || !isWithinBoard(board, x2, y2)) {
      Failure(IllegalArgumentException("Coordinates are outside canvas."))
    }
    else if(x1 != x2 && y1 != y2) {
      Failure(IllegalArgumentException("Only accept horizontal or vertical lines."))
    }
    else if (y1 == y2) {
      Success(drawHorizontalLine(board, y1, x1, x2))
    } else {
      Success(drawVerticalLine(board, x1, y1, y2))
    }
  }

  def drawHorizontalLine(board: Paint.Matrix, row: Int, colStart: Int, colEnd: Int): Paint.Matrix = {
    for(col <- colStart to colEnd)
      board(row)(col) = 'x'
    board
  }

  def drawVerticalLine(board: Paint.Matrix, col: Int, rowStart: Int, rowEnd: Int): Paint.Matrix = {
    for(row <- rowStart to rowEnd)
      board(row)(col) = 'x'
    board
  }
}

case class Rectangle(x1: Int, y1: Int, x2: Int, y2: Int) extends Command {
  override def execute(board: Paint.Matrix): Try[Paint.Matrix] = {
    Line(x1, y1, x2, y1).execute(board)
    Line(x1, y2, x2, y2).execute(board)
    Line(x1, y1, x1, y2).execute(board)
    Line(x2, y1, x2, y2).execute(board)
  }
}

case class BucketFill(x: Int, y: Int, color: Char) extends Command {
  override def execute(board: Paint.Matrix): Try[Paint.Matrix] = ???
}

case class Quit() extends Command {
  override def execute(board: Paint.Matrix): Try[Paint.Matrix] = {
    Success(board)
  }
}
