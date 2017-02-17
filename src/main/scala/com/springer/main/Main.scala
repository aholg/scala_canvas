package com.springer.main

import com.springer.model.CommandParser

object Main extends App{

  for (input <- io.Source.stdin.getLines()) {
    val cmd = CommandParser.parseInput(input)
    CommandParser.parseInput(input)
    println(cmd)
//    cmd.apply
  }
}
