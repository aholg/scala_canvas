package com.springer.model

import com.springer.model.Paint.Matrix

object Renderer {
  def createCanvasBoardString(board: Paint.Matrix): String = {
    val canvas = new StringBuilder
    if (board.size > 0) {
      canvas ++= createBorder(board(0).size + 1) + "\n"
      board.foreach { row =>
        canvas += '|'
        row.foreach { col =>
          canvas += col
        }
        canvas ++= "|\n"
      }
      canvas ++= createBorder(board(0).size + 1)
    }
    return canvas.toString()
  }

  private def createBorder(amount: Int): String = {
    val border = for (x <- 0 to amount) yield "-"
    border.mkString("")
  }
}
