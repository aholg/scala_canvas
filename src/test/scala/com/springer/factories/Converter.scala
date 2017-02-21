package com.springer.factories

import com.springer.model.Paint

object Converter {
  def stringToMatrix(content: String): Paint.Matrix = {
    content.split("\n").map { s =>
      s.toCharArray
    }
  }
}
