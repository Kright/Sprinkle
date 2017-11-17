package com.github.kright

import java.security.MessageDigest

object Helpers {
  def englishAlphabet: Array[Char] = (0 until 26).map(i => (i + 'a').toChar).toArray

  def nedoHashFunc(dg: MessageDigest)(s: String): String = {
    dg.reset()
    val h = dg.digest(s.map(_.toByte).toArray)
    bytesToString(h)
  }

  def nedoMd5: String => String = nedoHashFunc(MessageDigest.getInstance("MD5"))

  def nedoReduce(passwordLength: Int)(hash: String, shift: Int): String =
    stringToBytes(hash).take(passwordLength).map(b => ((shift ^ (b.toInt + 128)) % 26 + 'a').toChar).mkString("")

  def bytesToString(buf: Array[Byte]): String = buf.map("%02X".format(_)).mkString

  def stringToBytes(s: String): Array[Byte] = s.sliding(2, 2).map(Integer.parseInt(_, 16).toByte).toArray
}
