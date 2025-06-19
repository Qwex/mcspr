package ru.qwex.mcspr.parsers

import java.io.File

import org.jsoup.Jsoup
import org.jsoup.nodes.{Document, Element}
import ru.qwex.mcspr.data.{Application, Applications, Competition, CompetitionSettings}
import ru.qwex.mcspr.model._

import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 *
 * @author Aleksander Marenkov <a.marenkov at itgrp.ru>
 */
class WinOrientTableParser() extends Parser {

  import WinOrientTableParser._

  private def readHtml(file: File): Document = {
    Jsoup.parse(file)
  }

  def parse(competition: Competition, sources: ParserSource): Seq[ProtocolData] = {
    val document = readHtml(sources.sourceFile)

    val tables = document.select("body>table").asScala.toList.drop(1)

    //    val protocolHeader = ProtocolHeader(Seq.empty, "")
    val tableGroups = tables.grouped(3).toList


    val judgesTable = tableGroups.filter(_.length < 3).flatten.headOption
    //val judges = List(Judge("Лукин А.С.", "Главный судья", "СС2К"))
    val judges = judgesTable.map(parseJudges).getOrElse(List.empty) //  List(Judge("Лукин А.С.", "Главный судья", "СС2К"))


    val protocolHeader = ProtocolHeader.from(competition)

    val protocolDatas = tableGroups.filter(_.length == 3).map {
      case List(distanceNameTable, distanceAttributesTable, protocolTable) =>
        val distance = parseDistance(distanceNameTable, distanceAttributesTable)
        val rawDistanceName = parseRawDistanceName(distanceNameTable)
        val (protocolItems, ranking) = parseProtocolTable(
          protocolTable = protocolTable,
          distance = distance,
          rawDistanceName = rawDistanceName,
          applications = sources.applications,
          settings = competition.settings,
        )

        ProtocolData(
          header = protocolHeader,
          distance = distance,
          table = protocolItems,
          footer = ProtocolFooter(
            ranking = Some(ranking),
            judges = competition.judges,
          ),
        )
    }

    protocolDatas
  }

  private def parseJudges(judgesTable: Element): List[Judge] = {
    val trs = judgesTable.select("tr").asScala.toList
    trs.flatMap { tr =>
      val tds = tr.select("td").asScala.toList
      tds match {
        case td1 :: td2 :: _ =>
          val (name, qualification) = td2.text().split(",").map(_.trim).toList match {
            case name :: qualification :: _ => (name, qualification)
            case name :: _ => (name, "")
            case _ => ("", "")
          }
          val position = td1.text().trim
          Some(Judge(name, position, qualification))
            .filter(_.name.nonEmpty)
            .filter(_.position.nonEmpty)
        case Nil => Option.empty[Judge]
      }
    }
  }

  private def parseRawDistanceName(distanceNameTable: Element): String = {
    distanceNameTable.text().trim
  }

  private def parseDistance(distanceNameTable: Element, distanceAttributesTable: Element): Distance = {
    val distanceName = parseRawDistanceName(distanceNameTable)
    val group = parseGroup(distanceName)
    val distanceAttributes: Map[String, String] = distanceAttributesTable
      .select(">tbody>tr:eq(1)>td:eq(1) table tr")
      .asScala
      .map(_.select("td").asScala)
      .collect {
        case tds if tds.length > 1 => tds.head.text().trim.toLowerCase -> tds.tail.head.text().trim
      }
      .toMap

    Distance(
      name = group.name,
      length = distanceAttributes.getOrElse("длина", ""),
      kp = distanceAttributes.getOrElse("кп", "0").toInt,
      controlTime = distanceAttributes.get("контрольное время"),
      isOpen = group.isOpen,
      isJunior = group.isJunior,
      maybeMaxAge = group.maybeMaxAge
    )
  }

  private def parseProtocolTable(
                                  protocolTable: Element,
                                  distance: Distance,
                                  rawDistanceName: String,
                                  applications: Applications,
                                  settings: CompetitionSettings,
                                ) = {
    val columnsNames = protocolTable.select("th").asScala.toList.map(_.text())
    val typedColumns = columnsNames.map(TypedColumn.findTypedColumn) :+ TypedColumn.comment
    val rawRows = protocolTable
      .select("tr")
      .asScala.drop(1)
      .map(_.select("td").asScala.map(_.text().trim).toList)
      .toList
    val (rawResultRows, rawRankingRows) = rawRows.partition(_.length > 3)
    val protocolItems = rawResultRows.map {
      tds =>
        val parsedRow = parseResultRow(tds, typedColumns)

        val name = parsedRow.getOrElse(TypedColumn.fullName.typeName, "")

        val teamShort = parsedRow.get(TypedColumn.team.typeName)

        val application = findApplication(applications, rawDistanceName, name, teamShort)

        val team = findTeam(application, teamShort, settings.maybeMandatoryTeam)

        ProtocolItem.from(
          name = parsedRow.getOrElse(TypedColumn.fullName.typeName, ""),
          team = team,
          sportsCategory = parsedRow
            .get(TypedColumn.sportsCategory.typeName)
            .filter(_.nonEmpty)
            .filterNot(_.toLowerCase.contains("ю") && !distance.isJunior),
          number = parsedRow.getOrElse(TypedColumn.number.typeName, ""),
          yearOfBirth = parsedRow.getOrElse(TypedColumn.yearOfBirth.typeName, ""),
          result = parsedRow
            .get(TypedColumn.result.typeName)
            .map(TypedColumn.result.transform)
            .getOrElse(""),
          place = parsedRow.getOrElse(TypedColumn.place.typeName, ""),
          comment = parsedRow.get(TypedColumn.comment.typeName),
          application = application,
        )
    }
    val ranking = Ranking(rawRankingRows.map(_.mkString("")))
    (protocolItems, ranking)
  }

  private def parseResultRow(
                              tds: List[String],
                              typedColumns: List[TypedColumn],
                              result: Map[String, String] = Map.empty
                            ): Map[String, String] = (tds, typedColumns) match {
    case (td :: restTds, typedColumn :: restTypedColumns) =>
      if (typedColumn.acceptFilter(td)) {
        parseResultRow(restTds, restTypedColumns, result.updated(typedColumn.typeName, td))
      } else {
        parseResultRow(tds, restTypedColumns, result)
      }
    case _ => result
  }

  //  private def parseGroup(name: String): Group = {
  //    val regex1 = "^([ЖМ])(\\d+|Э)(.*)$".r
  //
  //    def parse(name: String, originalName: String): Group = {
  //      val maybe1 = try {
  //        regex1.findAllIn(name).subgroups match {
  //          case genderLabel :: "Э" :: _ =>
  //            Genders.find(genderLabel).flatMap(_.buildAgeCategory(21)).map((_, false))
  //          case genderLabel :: Age(age) :: postfix :: _ if List("Э", "А", "").contains(postfix) =>
  //            Genders.find(genderLabel).flatMap(_.buildAgeCategory(age)).map((_, age < 16))
  //          case _ => None
  //        }
  //      } catch {
  //        case ex: IllegalStateException =>
  //          None
  //      }
  //      maybe1
  //        .orElse(Genders.find(name).flatMap(_.buildAgeCategory(21)).map((_, false)))
  //        .map { case (category, isJunior) => Group(category, isJunior = isJunior) }
  //        .getOrElse(Group(originalName, isOpen = true))
  //    }
  //
  //    parse(name.trim.toUpperCase, name)
  //  }

}

object WinOrientTableParser {

  def findApplication(
                       applications: Applications,
                       rawDistanceName: String,
                       name: String,
                       teamShort: Option[String],
                     ) = {
    applications
      .findApplications(rawDistanceName, name)
      .filter { application => true
        //        teamShort
        //          .map(_.toLowerCase)
        //          .exists(application.team.toLowerCase.startsWith)
      } match {
      case application :: Nil => Some(application)
      case applications =>
        if (applications.map(_.team.toLowerCase).distinct.length == 1) {
          applications.headOption
        } else {
          None
        }
    }
  }

  private val maxWinOrientTeamLength = 20

  def findTeam(application: Option[Application], teamShort: Option[String], maybeMandatoryTeam: Option[String]): String = {
    maybeMandatoryTeam
      .orElse(
        application
          .map(_.team)
          .orElse(teamShort.filter(_.length < maxWinOrientTeamLength))
          .filterNot(_.matches(".*[a-zA-Z].*"))
          .map(team => if (team.contains(ProtocolItem.personallyTeamName)) ProtocolItem.personallyTeamName else team)
      )
      .getOrElse(ProtocolItem.personallyTeamName)
  }

  def competition2ProtocolHeader(competition: Competition): ProtocolHeader = {
    ProtocolHeader(
      conductingOrganizations = competition.conductingOrganizations,
      competitionParts = competition.name.split("\\\\n").toList.map(_.trim),
      date = Some(competition.date).filter(_.nonEmpty),
      discipline = Some(competition.discipline).filter(_.nonEmpty),
      disciplineCode = Some(competition.disciplineCode).filter(_.nonEmpty),
      registry = Some(competition.registry).filter(_.nonEmpty),
      place = Some(competition.place).filter(_.nonEmpty),
    )
  }

  object Genders {
    trait Gender {
      def mainAgeCategory: (Int, String)

      def minAge: Int

      def juniorCategories: List[(Int, String)]

      private def buildJuniorCategory(age: Int): Option[String] = {
        if (age < minAge) None else {
          juniorCategories.find {
            case (ageBound, _) => age < ageBound
          }.map {
            case (_, ageCategory) => s"$ageCategory (${buildUpToYears(age + 1)})"
          }
        }
      }

      private def buildUpToYears(age: Int): String = {
        val normalizedAge = age % 100
        if ((normalizedAge > 1 && normalizedAge < 20) || (normalizedAge % 10 > 1)) {
          s"до $age лет"
        } else {
          s"до $age года"
        }
      }

      private def buildMainCategory(age: Int): Option[String] = {
        val (ageBound, ageCategory) = mainAgeCategory
        Option().filter(_ => age == ageBound).map(_ => ageCategory)
      }


      def buildAgeCategory(age: Int): Option[String] = {
        buildJuniorCategory(age)
          .orElse(buildMainCategory(age))
      }
    }


    object Man extends Gender {
      override def mainAgeCategory: (Int, String) = (21, "Мужчины")

      override def minAge = 10

      override def juniorCategories: List[(Int, String)] = List(
        14 -> "Мальчики",
        19 -> "Юноши",
        21 -> "Юниоры"
      )

    }

    object Woman extends Gender {
      override def mainAgeCategory: (Int, String) = (21, "Женщины")

      override def minAge = 10

      override def juniorCategories: List[(Int, String)] = List(
        14 -> "Девочки",
        19 -> "Девушки",
        21 -> "Юниорки"
      )
    }

    //    case class Unknown(label: String) extends Gender {
    //      override def mainAgeCategory: (Int, String) = (0, label)
    //      override def juniorCategories: List[(Int, String)] = Nil
    //
    //      override def buildAgeCategory(age: Int): Option[String] = {
    //        Some(s"$label$age")
    //      }
    //    }

    private val labelToGender: Map[String, Gender] = Map(
      "Ж" -> Woman,
      "М" -> Man,
      "ЖЕНЩИНЫ" -> Woman,
      "МУЖЧИНЫ" -> Man,
    )

    def find(label: String): Option[Gender] = {
      labelToGender.get(label) //.getOrElse(label, Unknown(label))
    }
  }

  private object Age {

    def unapply(age: String): Option[Int] = {
      Try {
        age.toInt
      }.toOption
    }

  }

  def parseGroup(name: String): Group = {
    val regex1 = "^([ЖМ])(\\d+|Э)(.*)$".r

    def parse(name: String, originalName: String): Group = {
      val maybe1 = try {
        regex1.findAllIn(name).subgroups match {
          case genderLabel :: "Э" :: _ =>
            Genders.find(genderLabel).flatMap(_.buildAgeCategory(21)).map((_, false, 21))
          case genderLabel :: Age(age) :: postfix :: _ if List("Э", "А", "").contains(postfix) =>
            Genders.find(genderLabel).flatMap(_.buildAgeCategory(age)).map((_, age < 16, age))
          case _ => None
        }
      } catch {
        case ex: IllegalStateException =>
          None
      }
      maybe1
        .orElse(Genders.find(name).flatMap(_.buildAgeCategory(21)).map((_, false, 21)))
        .map { case (category, isJunior, age) => Group(category, isJunior = isJunior, maybeAge = Some(age)) }
        .getOrElse(Group(originalName, isOpen = true))
    }

    parse(name.trim.toUpperCase, name)
  }

}

case class WinOrientTableParserConfig(
                                       withOutOfCompetition: Boolean = false,
                                     )

case class ColumnsNormalization(
                                 remove: Set[String],
                                 append: Seq[String],
                               ) {

  def normalize(columnNames: Seq[String]): Seq[String] = {
    columnNames.filterNot(remove.contains) ++ append
  }

}

