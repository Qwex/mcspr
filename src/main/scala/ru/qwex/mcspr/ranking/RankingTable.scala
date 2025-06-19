package ru.qwex.mcspr.ranking

import java.io.File

import scala.io.Source
import scala.util.Try

/**
 *
 * @author Aleksander Marenkov <a.marenkov at itgrp.ru>
 */
object RankingTable {

  private val source = Source.fromFile(new File(System.getProperty("user.dir"), "ranking.txt"))
  private val table = source.getLines().toList.flatMap { line =>
    val parts = line
      .split(";")
      .map(_.trim)
      .toList
    parts match {
      case rawScore :: rawPercents =>
        parseInt(rawScore).map(_ ->
          List("I", "II", "III", "Iю", "IIю")
            .zip(rawPercents.map(parseInt))
            .flatMap{case (qualificationName, maybePercent) => maybePercent.map(qualificationName -> _)}
            .toMap
        )
    }
  }.sortBy(_._1)
  source.close()

  def getPercent(score: Int, qualificationName: String): Option[Int] = {
    def loop(table: List[(Int, Map[String, Int])]): Map[String, Int] = table match {
      case (qualificationScore1, percents1) :: (qualificationScore2, _) :: _
        if score >= qualificationScore1 && score < qualificationScore2 => percents1
      case (qualificationScore, percents) :: Nil if score >= qualificationScore => percents
      case (qualificationScore, _) :: _ if score < qualificationScore => Map.empty
      case _ :: restTable => loop(restTable)
      case _ => Map.empty
    }
    loop(table).get(qualificationName)
  }

  private def parseInt(str: String): Option[Int] = Try{str.toInt}.toOption


}
