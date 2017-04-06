package com.grysz.trello

import java.time.Duration

import org.scalatest.{FlatSpec, Matchers}

class AvgDurationTest extends FlatSpec with Matchers {
  import com.grysz.trello.DurationSyntax._

  val timeSpent = Map(
    "list1" -> List(Duration.parse("PT64H37M21.316S"), Duration.parse("PT33H3M48.724S"), Duration.parse("PT4828H14M30.461S")),
    "list2" -> List(Duration.parse("PT309H38M33.444S"), Duration.parse("PT59H4M41.737S")),
    "list3" -> List(Duration.parse("PT23H52M56.719S")),
    "list4" -> List(Duration.parse("PT102H36M12.341S"), Duration.parse("PT870H4M27.923S"), Duration.parse("PT1201H22M3.175S")),
    "list5" -> List(Duration.parse("PT318H22M24.418S"), Duration.parse("PT21H34M7.931S")),
    "list6" -> List(Duration.parse("PT134H30M32.215S")),
    "list7" -> List(Duration.parse("PT645H11M56.453S"))
  )

  val expectedAvgTimeSpent = Map(
    "list1" -> Duration.parse("PT1641H58M33S"),
    "list2" -> Duration.parse("PT184H21M37S"),
    "list3" -> Duration.parse("PT23H52M56S"),
    "list4" -> Duration.parse("PT724H40M54S"),
    "list5" -> Duration.parse("PT169H58M16S"),
    "list6" -> Duration.parse("PT134H30M32S"),
    "list7" -> Duration.parse("PT645H11M56S")
  )

  it should "calculate average duration" in {
    AvgDuration.avg(timeSpent).mapValues(_.stripMillis) should equal(expectedAvgTimeSpent)
  }
}
