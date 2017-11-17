package com.github.kright

import com.github.kright.Helpers.{nedoMd5, nedoReduce}


object SimpleExample extends App {

  val table = new RainbowTable(nedoMd5, nedoReduce(3), chainLength = 3)
  table.generate(Array("aaa"))
  println(table.chainByEnd.values.head.head.show)
  println(table(nedoMd5("aaa")))
  println(table(nedoMd5("rix")))
}
