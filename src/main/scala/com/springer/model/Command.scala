package com.springer.model

abstract class Command {
  def execute(board: Paint.Matrix): Paint.Matrix
}

case class Canvas(rows: Int, cols: Int) extends Command {
  override def execute(board: Paint.Matrix): Paint.Matrix = {
    Array.fill[Char](rows, cols)(' ')
  }
}

object Paint {
  type Matrix = Array[Array[Char]]
}

case class Line(x1: Int, y1: Int, x2: Int, y2: Int) extends Command {
  override def execute(board: Paint.Matrix): Paint.Matrix = {
    if (y1 == y2) {
      drawHorizontalLine(board, y1, x1, x2)
    } else {
      drawVerticalLine(board, x1, y1, y2)
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
  override def execute(board: Paint.Matrix): Paint.Matrix = ???
}

case class BucketFill(x: Int, y: Int, color: Char) extends Command {
  override def execute(board: Paint.Matrix): Paint.Matrix = ???
}

case class Quit() extends Command {
  override def execute(board: Paint.Matrix): Paint.Matrix = ???
}
