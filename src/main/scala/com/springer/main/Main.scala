package com.springer.main

import com.springer.model._

import scala.util.{Failure, Success}

object Main extends App {
  var canvas: Paint.Matrix = _

  def run {
    print("enter command: ")
    for (input <- io.Source.stdin.getLines()) {
      val cmd: Command = CommandParser.parseInput(input) match {
        case Some(command) if command.isInstanceOf[Canvas] => {
          canvas = command.execute(canvas).get
          command
        }
        case Some(command) => command
      }

      if (cmd.isInstanceOf[Canvas]) {
        println(Renderer.createCanvasBoardString(canvas))
      } else if (cmd.isInstanceOf[Quit]) {
        println("Goodbye")
        System.exit(1)
      } else {
        cmd.execute(canvas) match {
          case Success(board) => println(Renderer.createCanvasBoardString(board))
          case Failure(exception) => println(exception.getMessage)
        }
      }
      print("enter command: ")
    }
  }

  run
}
