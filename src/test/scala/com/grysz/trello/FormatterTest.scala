package com.grysz.trello

import java.time.Duration

import org.scalatest.{FlatSpec, Matchers}

class FormatterTest extends FlatSpec with Matchers {

  import com.grysz.trello.Formatter._
  import scalaz.syntax.show._

  "Formatter" should "format a string" in {
    "abc".shows should equal("abc")
    "\"abc\"".shows should equal("abc")
  }

  it should "format an integer" in {
    123.shows should equal("123")
  }

  it should "format a duration" in {
    Duration.ofSeconds(0).shows should equal("0h 0m 0s")
    Duration.ofMillis(500).shows should equal("0h 0m 0s")
    Duration.ofMillis(999).shows should equal("0h 0m 0s")
    Duration.ofHours(1).plusMinutes(61).shows should equal("2h 1m 0s")
    Duration.ofDays(1).plusHours(2).plusMinutes(3).plusSeconds(4).shows should equal("26h 3m 4s")
  }

  it should "format a map" in {
    Map[String, String]().shows should equal("")
    Map("key1" -> "value1").shows should equal("key1: value1")
    Map("key1" -> "value1", "key2" -> "value2").shows should equal(List("key1: value1", System.lineSeparator, "key2: value2").mkString)
  }
}
