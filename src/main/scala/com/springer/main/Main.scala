package com.springer.main

import com.springer.model._

import scala.util.{Failure, Success}

object Main extends App {
  var canvas: Paint.Matrix = Array.fill[Char](0, 0)(' ')

  def run {
    print("enter command: ")
    for (input <- io.Source.stdin.getLines()) {
      val cmd: Command = CommandParser.parseInput(input)

      if (cmd.isInstanceOf[Quit]) {
        println("Goodbye")
        System.exit(1)
      } else {
        cmd.execute(canvas) match {
          case Success(board) => canvas = board
          case Failure(exception) => println(exception.getMessage)
        }
      }
      println(Renderer.createCanvasBoardString(canvas))
      print("enter command: ")
    }
  }

  run
}
