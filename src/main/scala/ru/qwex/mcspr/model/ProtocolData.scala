package ru.qwex.mcspr.model

import ru.qwex.mcspr.data.{Application, Competition}
import ru.qwex.mcspr.utils.{DateNormalizer, RegexUtils}

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

  def calculateRanking(): ProtocolData = {
    copy(footer = footer.copy(ranking = footer.ranking.map(_.calculate(table))))
  }

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

  def additionalPageHeader(pageNum: Int, pagesCount: Int): String = {

    val competition = header.competition
    val date = header.date
    val group = distance.name.toLowerCase() //.replaceAll("\\(|\\)", "")
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
    distance.controlTime.flatMap(controlTime => Try(controlTime.trim.toInt).toOption.filter(_ > 0)).map { iControlTime =>
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

  private def yearOfBirthFilter(protocolData: ProtocolData): Option[ProtocolItemFilter] = {
    protocolData
      .distance
      .maybeMaxAge
      .map(maxAge =>
        ProtocolItemFilter(
          name = "Year of birth filter",
          filter = { protocolItem =>
            protocolItem
              .maybeYearOfBirth
              .flatMap(yearOfBirth => protocolData.header.maybeYear.map(_ - yearOfBirth))
              .exists(_ <= maxAge)
          },
        )
      )
  }

  def filter(protocolData: ProtocolData): ProtocolData = {
    protocolData.filter(
      List(
        yearOfBirthFilter(protocolData),
        patronymicAndBirthdayFilter(protocolData),
        Some(outOfCompetitionFilter),
        Some(latinNameFilter)
      ).flatten
    )
  }

}

case class ProtocolHeader(
                           conductingOrganizations: Seq[String],
                           competitionParts: List[String],
                           date: Option[String],
                           discipline: Option[String],
                           disciplineCode: Option[String],
                           registry: Option[String],
                           place: Option[String],
                           maybeYear: Option[Int] = None,
                         ) {

  val competition: String = competitionParts.mkString(" ")

}

object ProtocolHeader {

  def from(competition: Competition): ProtocolHeader = {
    ProtocolHeader(
      conductingOrganizations = competition.conductingOrganizations,
      competitionParts = competition.name.split("\\\\n").toList.map(_.trim),
      date = Some(competition.date).filter(_.nonEmpty),
      discipline = Some(competition.discipline).filter(_.nonEmpty),
      disciplineCode = Some(competition.disciplineCode).filter(_.nonEmpty),
      registry = Some(competition.registry).filter(_.nonEmpty),
      place = Some(competition.place).filter(_.nonEmpty),
      maybeYear = competition.maybeYear,
    )
  }

}

case class ProtocolFooter(
                           ranking: Option[Ranking],
                           judges: List[Judge],
                         )

case class ProtocolItem(
                         fullName: String,
                         name: String,
                         team: String,
                         sportsCategory: Option[String],
                         number: String,
                         birthdate: String,
                         yearOfBirth: String,
                         result: String,
                         place: Option[Int],
                         comment: Option[String],
                         hasPatronymic: Boolean,
                         hasBirthday: Boolean,
                         outOfCompetition: Boolean,
                         application: Option[Application] = None,
                         maybeYearOfBirth: Option[Int] = None,
                         maybeResultMs: Option[Int] = None,
                       )

object ProtocolItem {

  val personallyTeamName: String = "лично"

  private val patronymicFilterRegex: Regex = s"[${RegexUtils.cyrillicSymbols}]+(-[${RegexUtils.cyrillicSymbols}]+)?".r

  def patronymicFilter(patronymic: String): Boolean = {
    patronymicFilterRegex.matches(patronymic) && patronymic.split("-").forall(_.length >= 3)
  }

  //  private val commentRegExp: Regex = s"(\\d\\d\\.\\d\\d\\.*)\\s([${RegexUtils.cyrillicSymbols}]+)".r
  private val commentRegExp: Regex = s"(\\d\\d\\.\\d\\d\\.*)(.*)".r

  private def parseComment(comment: Option[String]): (Option[String], Option[String]) = comment.map(_.trim).flatMap {
    case commentRegExp(birthday, patronymic) =>
      Some((Some(birthday.stripSuffix(".")), Some(patronymic).map(_.trim).filter(patronymicFilter)))
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
            maybeResultMs: Option[Int] = None,
          ): ProtocolItem = {


    val (maybeCommentBirthday, maybeCommentPatronymic) = parseComment(comment)

    val maybeBirthdate = application.flatMap(_.birthdate).flatMap(DateNormalizer.maybeNormalize)
    val birthdate: String = maybeCommentBirthday
      .map(birthday => s"$birthday.$yearOfBirth")
      .orElse(maybeBirthdate)
      .getOrElse(yearOfBirth)
    val maybePatronymic = maybeCommentPatronymic.orElse(application.flatMap(_.patronymic))
    val fullName: String = maybePatronymic.map(patronymic => s"$name $patronymic").getOrElse(name)

    val hasPatronymic = maybePatronymic.nonEmpty
    val hasBirthday = maybeCommentBirthday.nonEmpty || maybeBirthdate.nonEmpty

    val parsedPlace: Option[Int] = {
      val _place = place.replaceAll("[^0-9]", "")
      if (_place.isEmpty) {
        None
      } else {
        Some(_place.toInt)
      }
    }

    val maybeYearOfBirth = Try {
      yearOfBirth.toInt
    }.toOption

    ProtocolItem(
      fullName = fullName,
      name = name,
      team = team,
      sportsCategory = sportsCategory,
      number = number,
      birthdate = birthdate,
      yearOfBirth = yearOfBirth,
      result = result,
      place = parsedPlace,
      comment = comment,
      hasPatronymic = hasPatronymic,
      hasBirthday = hasBirthday,
      outOfCompetition = outOfCompetitionPLaces.contains(place.toLowerCase),
      application = application,
      maybeYearOfBirth = maybeYearOfBirth,
      maybeResultMs = maybeResultMs,
    )
  }

  private val outOfCompetitionPLaces: Set[String] = Set("в/к")

  def isOutOfCompetitionPLaces(place: String): Boolean = {
    outOfCompetitionPLaces.contains(place.toLowerCase)
  }

}

trait Ranking {
  def lines: List[String]

  def calculate(protocolItems: List[ProtocolItem]): Ranking
}

case class ConstantRanking(lines: List[String]) extends Ranking {

  override def calculate(protocolItems: List[ProtocolItem]): Ranking = this

}

case class ComputableRanking(calculator: List[ProtocolItem] => Ranking) extends Ranking {

  override val lines: List[String] = Ranking.notRanking.lines

  override def calculate(protocolItems: List[ProtocolItem]): Ranking = calculator(protocolItems)

}

object Ranking {

  def apply(lines: List[String]): Ranking = ConstantRanking(lines)

  val notRanking: Ranking = Ranking(List("Ранг не определялся"))

}

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
                     maybeMaxAge: Option[Int] = None,
                   ) {

  lazy val controlTimeMs = controlTime.flatMap(ct => Try {
    ct.toInt
  }.toOption.map(_ * 60 * 1000))

}

case class Group(
                  name: String,
                  isOpen: Boolean = false,
                  isJunior: Boolean = false,
                  maybeAge: Option[Int] = None,
                ) {

  val maybeMaxAge: Option[Int] = {
    Some(39)
      .filter(_ => maybeAge.contains(21) && !isOpen)
      .orElse(maybeAge)
  }

}
