package com.github.kright

import com.github.kright.Helpers.{englishAlphabet, nedoMd5, nedoReduce}

import scala.util.Random


object BiggerExample extends App {
  val table = new RainbowTable(nedoMd5, nedoReduce(3), chainLength = 32)

  val initials = englishAlphabet.flatMap(c => englishAlphabet.flatMap(c2 => List(s"a$c$c2", s"b$c$c2", s"c$c$c2")))
  table.generate(initials)

  val maySolve = table.passwordsMaySolve()
  val totalPasswordsCount = math.pow(englishAlphabet.size, 3)
  println(s"accurate solution probability = ${maySolve.size.toDouble / totalPasswordsCount}")

  val rnd = new Random()

  def randChar = englishAlphabet(rnd.nextInt(englishAlphabet.size))

  val checksCount = 1000
  val approximateProbability = (0 until checksCount).count { i =>
    val pass = s"$randChar$randChar$randChar"
    table(nedoMd5(pass)).contains(pass)
  }.toDouble / checksCount

  println(s"approximate solution probability = $approximateProbability")
  println(s"chains stored count ${initials.size}")
  println(s"solution complexity = ${table.gettingPasswordComplexity}")
  println(s"generating complexity = ${table.generatingTableComplexity(initials.size)}")
}
