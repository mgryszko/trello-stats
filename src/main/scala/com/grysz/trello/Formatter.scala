package com.grysz.trello

import java.time.Duration

import scalaz.Show
import scalaz.Show.showFromToString

object Formatter {
  implicit val showString: Show[String] = new Show[String] {
    override def shows(f: String): String = f.stripPrefix("\"").stripSuffix("\"")
  }

  implicit val showInt: Show[Int] = showFromToString[Int]

  implicit val showDuration: Show[Duration] = new Show[Duration] {
    override def shows(f: Duration): String = {
      val h = f.toHours
      val m = f.minusHours(h).toMinutes
      val s = f.minusHours(h).minusMinutes(m).getSeconds
      s"${h}h ${m}m ${s}s"
    }
  }

  implicit def showMap[K: Show, V: Show]: Show[Map[K, V]] = {
    new Show[Map[K, V]] {
      override def shows(f: Map[K, V]): String =
        f.map { case (k, v) => showsEntry(k, v) }.mkString(System.lineSeparator)

      private def showsEntry(k: K, v: V) = implicitly[Show[K]].shows(k) + ": " + implicitly[Show[V]].shows(v)
    }
  }
}
