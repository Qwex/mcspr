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
