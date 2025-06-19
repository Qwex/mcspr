import ru.qwex.mcspr.utils.RegexUtils

/**
 *
 * @author Aleksander Marenkov <a.marenkov at itgrp.ru>
 */
object Test {

  def main(args: Array[String]): Unit = {
    for (
      i <- 1000 to 1100
    ) println(s"${i} ${declinationScores(i)}")
  }

  def declinationScores(scores: Int): String =
  {
    val rem1 = scores % 100
    val rem2 = rem1 % 10
    if ((rem1 >= 5 && rem1 <= 20) || (rem2 == 0 || (rem1 > 20 && rem2 >= 5))) {
      "баллов"
    } else if (rem2 == 1) {
      "балл"
    } else {
      "балла"
    }
  }

}

object Test2 {

  def main(args: Array[String]): Unit = {
    val p = "   Сергеевич-владимирович  "
    val regex = s"[${RegexUtils.cyrillicSymbols}]+-*[${RegexUtils.cyrillicSymbols}]+".r

    println(regex.matches(p))
  }

}

object Test3 {

  def main(args: Array[String]): Unit = {
    val r = "\\d\\d\\d\\d".r
    println(r.findFirstIn("1 января 2025 г."))
  }

}
