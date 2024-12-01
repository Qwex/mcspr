package ru.qwex.mcspr.model

import ru.qwex.mcspr.data.Application
import ru.qwex.mcspr.utils.RegexUtils

import scala.annotation.tailrec
import scala.util.Try
import scala.util.matching.Regex

/**
 *
 * @author Aleksander Marenkov <a.marenkov at itgrp.ru>
 */
case class ProtocolData(
                         header: ProtocolHeader,
                         distance: Distance,
                         table: List[ProtocolItem],
                         footer: ProtocolFooter,
                       ) {

  val hasPatronymic: Boolean = table.exists(_.hasPatronymic)
  val hasBirthday: Boolean = table.exists(_.hasPatronymic)

  val descriptionDatePlace: List[String] = {
    List(
      List(
        header.discipline,
        header.disciplineCode.map(disciplineCode => s"код дисциплины: $disciplineCode"),
        header.registry.map(registry => s"реестр: $registry")
      ).flatten.mkString(", "),
      List(
        header.date, header.place
      ).flatten.mkString(", ")
    )
  }

  private def buildPageDescription(pageNum: Int, pagesCount: Int): String = {
    s"лист $pageNum, всего листов $pagesCount"
  }

  def additionalPageHeader(pageNum: Int, pagesCount: Int):String = {

    val competition = header.competition
    val date = header.date
    val group = distance.name.toLowerCase().replaceAll("\\(|\\)", "")
    val pageDescription = buildPageDescription(pageNum, pagesCount)
    List(
      Some(competition),
      date,
      Some(group),
      Some(pageDescription),
    ).flatten.mkString(", ")
  }

  private def removeSuffix(str: String, suffix: String): String = {
    if (str.endsWith(suffix)) {
      removeSuffix(str.stripSuffix(suffix), suffix)
    } else {
      str
    }
  }

  val tableTitle = List(
    Some(distance.name),
    if (distance.kp > 0) Some(s"${distance.kp} КП") else None,
    {
      if (distance.length.nonEmpty) {
        if (distance.length.contains(".")) {
          Some(s"${removeSuffix(distance.length, "0")} км")
        } else {
          Some(s"${distance.length} м")
        }
      } else {
        None
      }
    },
    distance.controlTime.flatMap(controlTime => Try(controlTime.trim.toInt).toOption).map { iControlTime =>
      s"Контрольное время $iControlTime ${printMinutes(iControlTime % 100)}"
    }
  ).flatten.mkString(", ")

  private def printMinutes(iControlTime: Int): String = {
    if (iControlTime > 10 && iControlTime < 20) {
      "минут"
    } else {
      val sControlTimeLast = iControlTime.toString.last.toString.toInt
      if (Set(0, 5, 6, 7, 8, 9).contains(sControlTimeLast)) {
        "минут"
      } else if (Set(1).contains(sControlTimeLast)) {
        "минута"
      } else {
        "минуты"
      }
    }
  }


  def filter(filters: Seq[ProtocolItemFilter]): ProtocolData = {
    @tailrec
    def loop(
              items: List[ProtocolItem],
              reversed: List[ProtocolItem] = List.empty,
            ): List[ProtocolItem] = {
      items match {
        case item :: restItems =>
          val failFilters = filters.filterNot(_.filter(item))
          if (failFilters.nonEmpty) {
            loop(restItems, reversed)
          } else {
            loop(restItems, item :: reversed)
          }
        case Nil => reversed.reverse
      }
    }

    copy(table = loop(table))
  }

}

case class ProtocolItemFilter(name: String, filter: ProtocolItem => Boolean)

object ProtocolItemFilter {

  private val outOfCompetitionFilter: ProtocolItemFilter = ProtocolItemFilter(
    name = "Out of competition filter",
    filter = protocolItem => !protocolItem.outOfCompetition,
  )

  private def patronymicAndBirthdayFilter(protocolData: ProtocolData): Option[ProtocolItemFilter] = {
    if (protocolData.hasPatronymic || protocolData.hasBirthday) {
      Some(ProtocolItemFilter(
        name = "Patronymic and birthday filter",
        filter = protocolItem => protocolItem.hasPatronymic && protocolItem.hasBirthday,
      ))
    } else {
      None
    }
  }

  private val cyrillicRegexp: String = s"^([${RegexUtils.cyrillicSymbols}]|\\s|-)+$$"

  private val latinNameFilter: ProtocolItemFilter = ProtocolItemFilter(
    name = "Latin name filter",
    filter = { protocolItem => protocolItem.fullName.matches(cyrillicRegexp) }
  )

  def filter(protocolData: ProtocolData): ProtocolData = {
    protocolData.filter(
      List(
        patronymicAndBirthdayFilter(protocolData),
        Some(outOfCompetitionFilter),
        Some(latinNameFilter)
      ).flatten
    )
  }

}

case class ProtocolHeader(
                           conductingOrganizations: Seq[String],
                           competition: String,
                           date: Option[String],
                           discipline: Option[String],
                           disciplineCode: Option[String],
                           registry: Option[String],
                           place: Option[String],
                         ) {

}

case class ProtocolFooter(
                           ranking: Option[Ranking],
                           judges: List[Judge],
                         )

case class ProtocolItem(
                         fullName: String,
                         team: String,
                         sportsCategory: Option[String],
                         number: String,
                         birthdate: String,
                         result: String,
                         place: Option[Int],
                         comment: Option[String],
                         hasPatronymic: Boolean,
                         hasBirthday: Boolean,
                         outOfCompetition: Boolean,
                         application: Option[Application] = None,
                       )

object ProtocolItem {

  val personallyTeamName: String = "лично"

  private val commentRegExp: Regex = s"(\\d\\d\\.\\d\\d\\.*)\\s([${RegexUtils.cyrillicSymbols}]+)".r

  private def parseComment(comment: Option[String]): (Option[String], Option[String]) = comment.map(_.trim).flatMap {
    case commentRegExp(birthday, patronymic) =>
      Some((Some(birthday.stripSuffix(".")), Some(patronymic)))
    case _ => None
  }.getOrElse((None, None))

  def from(
            name: String,
            team: String,
            sportsCategory: Option[String],
            number: String,
            yearOfBirth: String,
            result: String,
            place: String,
            comment: Option[String],
            application: Option[Application],
          ): ProtocolItem = {


    val (maybeBirthday, maybePatronymic) = parseComment(comment)

    val birthdate: String = maybeBirthday.map(birthday => s"$birthday.$yearOfBirth").getOrElse(yearOfBirth)
    val fullName: String = maybePatronymic.map(patronymic => s"$name $patronymic").getOrElse(name)

    val hasPatronymic = maybePatronymic.nonEmpty
    val hasBirthday = maybeBirthday.nonEmpty

    val parsedPlace: Option[Int] = {
      val _place = place.replaceAll("[^0-9]", "")
      if (_place.isEmpty) {
        None
      } else {
        Some(_place.toInt)
      }
    }

    ProtocolItem(
      fullName = fullName,
      team = team,
      sportsCategory = sportsCategory,
      number = number,
      birthdate = birthdate,
      result = result,
      place = parsedPlace,
      comment = comment,
      hasPatronymic = hasPatronymic,
      hasBirthday = hasBirthday,
      outOfCompetition = outOfCompetitionPLaces.contains(place.toLowerCase),
      application = application,
    )
  }

  private val outOfCompetitionPLaces: Set[String] = Set("в/к")

  def isOutOfCompetitionPLaces(place: String): Boolean = {
    outOfCompetitionPLaces.contains(place.toLowerCase)
  }

}

case class Ranking(lines: List[String])

case class Judge(
                  name: String,
                  position: String,
                  qualification: String,
                )

case class Distance(
                     name: String,
                     length: String,
                     kp: Int,
                     controlTime: Option[String],
                     isOpen: Boolean = false,
                     isJunior: Boolean = false,
                   )

case class Group(
                  name: String,
                  isOpen: Boolean = false,
                  isJunior: Boolean = false,
                )
