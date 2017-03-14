package com.grysz.trello

import java.time.Duration

object AvgDuration {
  def avg[A](m: Map[A, List[Duration]]): Map[A, Duration] = m mapValues avg

  private def avg(xs: List[Duration]): Duration = {
    val total = xs.fold(Duration.ZERO)(_ plus _)
    total dividedBy xs.size
  }
}

object DurationSyntax {
  implicit class DurationOps(duration: Duration) {
    def stripMillis: Duration = Duration.ofSeconds(duration.getSeconds)
  }
}
