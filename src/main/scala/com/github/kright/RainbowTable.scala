package com.github.kright

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class RainbowTable[Password, Hash](val hash: Password => Hash, val reduction: (Hash, Int) => Password, val chainLength: Int) {

  def chainStream(pass: Password, pos: Int): Stream[(Password, Hash)] = {
    val h = hash(pass)
    (pass, h) #:: chainStream(reduction(h, pos), pos + 1)
  }

  case class Chain(start: Password, end: Hash, startPos: Int = 0) {
    def elements: Seq[(Password, Hash)] = chainStream(start, startPos).take(chainLength - startPos)

    def show: String = elements.map { case (pass, h) => s"$pass => $h" }.mkString(" => ")

    def foundPassword(h: Hash): Option[Password] = elements.find { case (pass, h2) => h2 == h }.map(_._1)
  }

  def step(p: Password, i: Int) = reduction(hash(p), i)

  def makeChain(start: Password, startPos: Int = 0, end: Int = chainLength): Chain = {
    val (endPass, endHash) = chainStream(start, startPos).drop(end - startPos).head
    Chain(start, endHash, startPos)
  }


  val chainByEnd = new mutable.HashMap[Hash, ArrayBuffer[Chain]]()

  def generate(initial: Seq[Password]): Unit = {
    for (p <- initial) {
      val c = makeChain(p)
      val arr = chainByEnd.getOrElseUpdate(c.end, new ArrayBuffer[Chain]())
      arr += c
    }
  }

  def apply(h: Hash): Option[Password] = {
    require(chainByEnd.nonEmpty, "table was't generated")

    for (pos <- 0 until chainLength) {
      val c = makeChain(reduction(h, pos), pos + 1)

      val result = chainByEnd.getOrElse(c.end, List.empty).flatMap(_.foundPassword(h)).headOption

      if (result.isDefined) {
        return result
      }
    }
    None
  }

  /* for better understanding */

  def passwordsMaySolve(): mutable.Set[Password] = {
    val maySolve = mutable.HashSet[Password]()

    chainByEnd.values.foreach { arr =>
      arr.foreach { chain =>
        chain.elements.foreach { case (pass, _) =>
          maySolve.add(pass)
        }
      }
    }

    maySolve
  }

  // in worst case. Average value will be less by constant multiplier
  def gettingPasswordComplexity: Int = chainLength * chainLength

  def generatingTableComplexity(initialsCount: Int): Int = chainLength * initialsCount
}
