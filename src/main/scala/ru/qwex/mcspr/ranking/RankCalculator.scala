package ru.qwex.mcspr.ranking

import java.time.LocalTime
import java.time.format.DateTimeFormatter

import com.typesafe.config.Config
import ru.qwex.mcspr.Boot
import ru.qwex.mcspr.model.{ConstantRanking, ProtocolItem, Ranking}

/**
 *
 * @author Aleksander Marenkov <a.marenkov at itgrp.ru>
 */
object RankCalculator {
  private val rankingPath = "ranking"

  private val formatter = DateTimeFormatter.ofPattern("HH:mm:ss")

  private def rankingCalculatorConfig = RankCalculatorConfig(Boot.config, rankingPath)

  private def calculateScore(protocolItems: List[ProtocolItem]): Int = {
    Option
      .when(protocolItems.length >= rankingCalculatorConfig.startLimit) {
        protocolItems
          .flatMap(protocolItem =>
            for {
              _ <- protocolItem.maybeResultMs
              qualificationName = protocolItem.sportsCategory.getOrElse("")
            } yield qualificationToScore(qualificationName)
          )
          .sorted
          .reverse
      }
      .filter(_.length >= rankingCalculatorConfig.finishLimit)
      .map(_.take(rankingCalculatorConfig.sumCount).sum)
      .getOrElse(-1)
  }

  def calculate(qualificationNames: List[String], protocolItems: List[ProtocolItem]): Ranking = {
    val maybeLeaderResultMs = protocolItems.dropWhile(_.maybeResultMs.isEmpty).headOption.flatMap(_.maybeResultMs)
    maybeLeaderResultMs.flatMap { leaderResultMs =>
      val score = calculateScore(protocolItems)
      val rankLines = qualificationNames.sortBy(qualificationNameScoreMap.getOrElse(_, 0)).reverse.flatMap { qualificationName =>
        RankingTable
          .getPercent(score, qualificationName)
          .map { percent =>
            val ms = Math.round(leaderResultMs * percent.toDouble / 100)
            val time = LocalTime
              .ofNanoOfDay(ms * 1000000)
              .plusNanos(500000000)
              .withNano(0)
              .format(formatter)
            s"${qualificationName.padTo(maxQualificationNameLength, ' ')} - ${percent}% - ${time}"
          }
      }
      Option.when(rankLines.nonEmpty) {
        ConstantRanking(List(
          s"Квалификационный уровень - ${score} ${declinationScores(score)}"
        ) ++ rankLines)
      }
    }.getOrElse(Ranking.notRanking)
  }

  private val qualificationNameScoreMap: Map[String, Int] = Map(
    "IIIю" -> 1,
    "IIю" -> 2,
    "Iю" -> 3,
    "III" -> 6,
    "II" -> 25,
    "I" -> 50,
    "КМС" -> 80,
    "МС" -> 100,
    "МСМК" -> 100,
  )

  private val maxQualificationNameLength = qualificationNameScoreMap.keys.map(_.length).max

  private def qualificationToScore(qualificationName: String): Int = {
    qualificationNameScoreMap.getOrElse(qualificationName, 0)
  }

  private def declinationScores(scores: Int): String = {
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

object RankCalculatorConfig {
  private val startLimitPath = "start-limit"
  private val finishLimitPath = "finish-limit"
  private val sumCountPath = "sum-count"

  def apply(config: Config, basePath: String): RankCalculatorConfig = {
    val baseConfig = config.getConfig(basePath)
    RankCalculatorConfig(
      startLimit = baseConfig.getInt(startLimitPath),
      finishLimit = baseConfig.getInt(finishLimitPath),
      sumCount = baseConfig.getInt(sumCountPath),
    )
  }

}

case class RankCalculatorConfig(
                                 startLimit: Int,
                                 finishLimit: Int,
                                 sumCount: Int,
                               )

