package ru.qwex.mcspr.renders

import java.io.File

import ru.qwex.mcspr.data.Csv
import ru.qwex.mcspr.model.ProtocolItem.personallyTeamName
import ru.qwex.mcspr.model.{Judge, ProtocolData, ProtocolItem, ProtocolItemFilter}
import ru.qwex.mcspr.parsers.WinOrientTableParser
import ru.qwex.mcspr.renders.model.PageRowRender
import ru.qwex.mcspr.utils.ZipUtils

import scala.io.Source

/**
 *
 * @author Aleksander Marenkov <a.marenkov at itgrp.ru>
 */
object DocxRender {
  private val output = "templates/document/word/document.xml"

  private val documentTemplatePath = "templates/documentTemplate.xml"
  private val footerTemplatePath = "templates/footerTemplate.xml"
  private val judgeTemplatePath = "templates/judgeTemplate.xml"
  private val protocolItemTemplatePath = "templates/protocolItemTemplate.xml"
  private val rankingRowTemplatePath = "templates/rankingRowTemplate.xml"
  private val tableHeaderTemplatePath = "templates/tableHeaderTemplate.xml"
  private val conductingOrganizationTemplatePath = "templates/conductingOrganizationTemplate.xml"
  private val competitionTemplatePath = "templates/competitionTemplate.xml"
  private val descriptionDatePlaceTemplatePath = "templates/descriptionDatePlaceTemplate.xml"
  private val titleTemplatePath = "templates/titleTemplate.xml"
  private val tableTitleTemplatePath = "templates/tableTitleTemplate.xml"
  private val pageSeparatorTemplatePath = "templates/pageSeparatorTemplate.xml"
  private val additionalPageHeaderPath = "templates/additionalPageHeader.xml"

  private val maxWinOrientTeamLength = 20


  object renders {
    private val documentTemplate = DocxTemplate.load(documentTemplatePath)
    private val footerTemplate = DocxTemplate.load(footerTemplatePath)
    private val judgeTemplate = DocxTemplate.load(judgeTemplatePath)
    private val protocolItemTemplate = DocxTemplate.load(protocolItemTemplatePath)
    private val rankingRowTemplate = DocxTemplate.load(rankingRowTemplatePath)
    private val tableHeaderTemplate = DocxTemplate.load(tableHeaderTemplatePath)

    def protocolItemRow(row: String) = PageRowRender(row, protocolItemTemplate.substitute(_))

    def tableHeader(row: String) = PageRowRender(row, tableHeaderTemplate.substitute(_))
    //    def

  }

  //  private def proteoclItemRow(row: String) =


  private def loadTemplate(path: String): String = {
    val source = Source.fromFile(path)
    val template = source.getLines().mkString("\n")
    source.close()
    template
  }

  private def loadTemplate2(path: String): DocxTemplate = {
    val source = Source.fromFile(path)
    val template = source.getLines().mkString("\n")
    source.close()
    DocxTemplate(template, "")
  }

  private def addPadding(string: String, padding: Int): String = {
    if (padding != 0) {
      String.format(s"%${padding}s", string)
    } else {
      string
    }
  }

  //  private def align(string: String, align: Int, width: Int): String = {
  //    val absWidth = Math.abs(width)
  //    if (absWidth !=0 ) {
  //      val spaces = absWidth - string.length
  //    } else {
  //
  //    }
  //  }

  private def addPaddings(string: String, leftPad: Int, rightPad: Int): String = {
    addPadding(addPadding(string, -Math.abs(leftPad)), Math.abs(rightPad))
  }

  private val sequenceNumberColumnName = "№п/п"
  private val fullNameColumnName1 = "Фамилия, имя"
  private val fullNameColumnName2 = "Фамилия, имя, отчество"
  private val teamColumnName = "Коллектив"
  private val sportCategoryColumnName = "Квал"
  private val numberColumnName = "Номер"
  private val birthYearColumnName = "  ГР "
  private val birthDayColumnName = "    ДР  "
  private val resultColumnName = "Результат"
  private val placeColumnName = "Место"
  private val pageWidth = 96
  private val columnsCount = 8
  private val columnGaps = columnsCount - 1
  private val effectivePageWidth = pageWidth - columnGaps


  def render(protocols: Seq[ProtocolData]): Unit = {
    val documentTemplate = loadTemplate(documentTemplatePath)

    val renderedProtocols = protocols.map(render)

    printToFile(new File(output)) { p =>
      p.println(documentTemplate.replace("${document}",
        List(renderedProtocols).mkString("\n")
      ))
    }
  }

  def render(protocol: ProtocolData): String = {
    val protocolItemTemplate = loadTemplate(protocolItemTemplatePath)
    val tableHeaderTemplate = loadTemplate(tableHeaderTemplatePath)
    val additionalPageHeaderTemplate = loadTemplate(additionalPageHeaderPath)


    val widths = calculateWidths(protocol)
    val layoutTable = calculateLayoutTable(widths)

    val tableHeader = layoutTable.header
    val tableHeaderRender = tableHeaderTemplate.replace("${header}", tableHeader)

    val layout = LayoutCalculator.calculate(protocol)

    def renderPages(protocolData: ProtocolData): String = {
      val renderedHeader = renderHeader(protocol, layout)
      val renderedJudges = renderJudges(protocolData, 2)
      val renderedFooter = renderFooter(protocol, renderedJudges)

      def renderPage(
                      protocolItems: Seq[ProtocolItem],
                      pageNum: Int,
                      startI: Int = 0,
                      prevProtocolPlace: Int = 1,
                      prevPlace: Option[Int] = Option.empty,
                    ) = {
        val (renderedProtocolItems, nextPrevPlace, nextPrevProtocolPlace) = protocolItemsRender1(
          protocolItems.toList, widths, protocolItemTemplate, startI, prevProtocolPlace, prevPlace)

        (List(
          if (pageNum == 1) {
            List(renderedHeader, tableHeaderRender)
          } else {
            val renderedAdditionalPageHeader = additionalPageHeaderTemplate.replace(
              "${additionalPageHeader}", protocol.additionalPageHeader(pageNum, layout.pagesCount)
            )
            List(renderedAdditionalPageHeader)
          },
          List(renderedProtocolItems, if (
            layout.pagesCount == 1 || pageNum == layout.pagesCount)  renderedFooter else renderedJudges
          ),
        ).flatten.mkString("\n"), nextPrevPlace, nextPrevProtocolPlace)
      }

      def loop(
                protocolItems: Seq[ProtocolItem],
                pageNum: Int,
                itemsCounts: Seq[Int],
                pages: Seq[String],
                startI: Int = 0,
                prevProtocolPlace: Int = 1,
                prevPlace: Option[Int] = Option.empty,
              ): Seq[String] = {
        itemsCounts match {
          case itemsCount :: restItemsCount if protocolItems.nonEmpty =>
            val (renderedPage, nextPrevPlace, nextPrevProtocolPlace) = renderPage(
              protocolItems.take(itemsCount),
              pageNum,
              startI,
              prevProtocolPlace,
              prevPlace,
            )
            loop(
              protocolItems = protocolItems.drop(itemsCount),
              pageNum = pageNum + 1,
              itemsCounts = restItemsCount,
              pages = pages :+ renderedPage,
              startI = startI + itemsCount,
              prevPlace = nextPrevPlace,
              prevProtocolPlace = nextPrevProtocolPlace,
            )
          case _ => pages
        }
      }

      val pages = loop(protocolData.table, 1, itemsCounts = layout.tableRowOnPage, Seq.empty)
      pages.mkString(s"\n")
    }

    val renderedPages = renderPages(protocol)

    List(renderedPages).mkString("\n")


  }


  private def buildPageDescription(i: Int, layout: Layout): String = {
    s"лист $i, всего листов ${layout.tableRowOnPage.length}"
  }

  def renderHeader(protocolData: ProtocolData, layout: Layout) = {
    val conductingOrganizationTemplate = loadTemplate(conductingOrganizationTemplatePath)
    val competitionTemplate = loadTemplate(competitionTemplatePath)
    val descriptionDatePlaceTemplate = loadTemplate(descriptionDatePlaceTemplatePath)
    val titleTemplate = loadTemplate(titleTemplatePath)
    val tableTitleTemplate = loadTemplate(tableTitleTemplatePath)

    val renderedConductingOrganizations = protocolData.header.conductingOrganizations.map(conductingOrganization =>
      conductingOrganizationTemplate.replace("${conductingOrganization}", conductingOrganization)
    ).mkString("\n")
    val renderCompetition = competitionTemplate.replace(
      "${competition}", {
        val competition = protocolData.header.competition

        val r = "(?i)^(ЧиП|Чемпионат и первенство)\\s+((г|г.|гор.|города)\\s+Москвы)\\s+.*".r


        competition
      }
    )

    val descriptionDatePlace = protocolData.descriptionDatePlace

    //    val descriptionDatePlace = List("Лыжная гонка – спринт, код дисциплины: 0830133811Я, реестр: 2612",
    //      "20 января 2024 г., г.о. Балашиха (Московская область)")

    val renderedDescriptionDatePlaceTemplate = descriptionDatePlace.map(p =>
      descriptionDatePlaceTemplate.replace(
        "${descriptionDatePlace}", p
      )
    ).mkString("\n")

    val tableTitle = protocolData.tableTitle // "Девочки (до 13 лет) , 9 КП, 1,5 км Контрольное время 60 минут"
    val pageDescription = if (layout.pagesCount > 2) Some(buildPageDescription(1, layout)) else None
    val renderedTableTitle = tableTitleTemplate.replace(
      "${tableTitle}", List(Some(tableTitle), pageDescription).flatten.mkString(", ")
    )


    List(
      renderedConductingOrganizations,
      renderCompetition,
      renderedDescriptionDatePlaceTemplate,
      titleTemplate,
      renderedTableTitle,
    ).mkString("\n")
  }

  def renderJudges(protocolData: ProtocolData, judgePadding: Int = 2): String = {
    val pageWidth = 86
    val judgeRowTemplate = loadTemplate(judgeTemplatePath)
    val pageSeparatorTemplate = loadTemplate(pageSeparatorTemplatePath)

    val emptyRow = judgeRowTemplate.replace("${judge}", "")
    val renderedJudgePadding = List.fill(judgePadding)(emptyRow).mkString("\n")

    def buildNameQualification(judge: Judge) = s"${judge.name}, ${judge.qualification}"

    val widths = protocolData.footer.judges.foldLeft((0, 0)) {
      case (p@(_, nameQualificationWidth), judge) =>
        val nameQualification = buildNameQualification(judge)
        if (nameQualificationWidth < nameQualification.length) {
          (pageWidth - nameQualification.length, nameQualification.length)
        } else p
    }

    def buildJudgeString(judge: Judge): String = {
      val (positionWidth, nameQualificationWidth) = widths
      String.format(s"%${-positionWidth}s%${-nameQualificationWidth}s", judge.position, buildNameQualification(judge))
    }

    val judges: List[String] = {
      val judges = protocolData.footer.judges.map(buildJudgeString)
      if (judges.nonEmpty) {
        val lastJudge = judges.last
        judges.updated(judges.length - 1, s"${lastJudge}$pageSeparatorTemplate")
      } else {
        judges
      }
    }


    val renderedJudges = judges.flatMap(judge =>
      List(judgeRowTemplate.replace(
        "${judge}", judge
      ), emptyRow)
    ).dropRight(1).mkString("\n")

    List(
      renderedJudgePadding,
      renderedJudges
    ).mkString("\n")
  }

  def renderFooter(protocolData: ProtocolData, renderedJudges: String): String = {
    val judgeRowTemplate = loadTemplate(judgeTemplatePath)
    val rankingRowTemplate = loadTemplate(rankingRowTemplatePath)

    val renderedRanking = protocolData.footer.ranking.map(
      _.lines.map(line =>
        rankingRowTemplate.replace("${rankingRow}", line)
      ).mkString("\n")
    ).getOrElse("")

    List(
      renderedRanking,
      renderedJudges
    ).mkString("\n")
  }


  def calculateLayoutTable(widths: Seq[(String, Int, Int, Int)]): LayoutTable = {
    LayoutTable(widths.map {
      case (header, _, width, _) => LayoutColumn(header, width, false)
    }.toList, pageWidth)
  }

  def calculateWidths(protocol: ProtocolData): Seq[(String, Int, Int, Int)] = {
    val fullNameColumnName = if (protocol.hasPatronymic) {
      fullNameColumnName2
    } else {
      fullNameColumnName1
    }
    val (maxFullNameWidth, maxTeamWidth) =
      protocol.table.foldLeft(
        (fullNameColumnName.length, Math.max(teamColumnName.length, personallyTeamName.length))
      ) {
        case ((maxFullNameWidth, maxTeamWidth), protocolItem) =>
          (
            Math.max(maxFullNameWidth, protocolItem.fullName.length),
            Math.max(maxTeamWidth, protocolItem.team.length),
          )
      }

    val sequenceNumberColumnWidth = 4
    val sportCategoryColumnWidth = 4
    val numberColumnWidth = 4
    val birthdateColumnWidth = protocol.table.foldLeft(6) {
      case (maxBirthdateWidth, protocolItem) => Math.max(maxBirthdateWidth, protocolItem.birthdate.length)
    }
    val resultColumnWidth = 9
    val placeColumnWidth = 5

    val maxFullNameAndTeamWidth = {
      effectivePageWidth -
        sequenceNumberColumnWidth -
        sportCategoryColumnWidth -
        numberColumnWidth -
        birthdateColumnWidth -
        resultColumnWidth -
        placeColumnWidth
    }

    val teamColumnWidth = Math.min(maxTeamWidth, maxFullNameAndTeamWidth - maxFullNameWidth)
    val fullNameColumnWidth = maxFullNameAndTeamWidth - teamColumnWidth
    val birthDateColumnName = if (protocol.table.map(_.birthdate.length).max > 4) {
      birthDayColumnName
    } else {
      birthYearColumnName
    }

    val widths = Seq(
      (sequenceNumberColumnName, 0, sequenceNumberColumnWidth, 1),
      (fullNameColumnName, 1, fullNameColumnWidth, -1),
      (teamColumnName, 1, teamColumnWidth, -1),
      (sportCategoryColumnName, 1, sportCategoryColumnWidth, -1),
      (numberColumnName, 1, numberColumnWidth, 1),
      (birthDateColumnName, 1, birthdateColumnWidth, 1),
      (resultColumnName, 1, resultColumnWidth, -1),
      (placeColumnName, 1, placeColumnWidth, 1),
    )

    widths
  }

  private val maxTeamShortWidth: Int = 20

  private def hasTeamShortened(protocolItem: ProtocolItem) = {
    protocolItem.application.isEmpty
  }

//  private def chooseTeam(protocolItem: ProtocolItem, maxTeamWidth: Int) = {
//    if (protocolItem.team.length > maxTeamWidth ||
//      protocolItem.team.contains(personallyTeamName) ||
//      protocolItem.team.isEmpty
//    ) {
//      personallyTeamName
//    } else {
//      protocolItem.team
//    }
//  }

  def protocolItemsRender1(
                            table: List[ProtocolItem],
                            widths: Seq[(String, Int, Int, Int)],
                            protocolItemTemplate: String,
                            startI: Int = 0,
                            prevProtocolPlace: Int = 1,
                            prevPlace: Option[Int] = Option.empty
                          ): (String, Option[Int], Int) = {
    val a = table.zipWithIndex.map{case (item, i) => (item, i + startI)}.foldLeft(
      (List.empty[String], prevPlace, prevProtocolPlace)
    ) { case ((rows, prevPlace, prevProtocolPlace), (protocolItem, i)) =>
      val sequenceNumber = i + 1
      val repeatedPlace = isRepeatedPlace(protocolItem.place, prevPlace)
      val place = if (repeatedPlace) prevProtocolPlace else sequenceNumber
      val renderedPlace = renderPlace(protocolItem.place, place, repeatedPlace)
      val teamWidth = widths(2)._3
      val values = Seq(
        sequenceNumber.toString,
        protocolItem.fullName,
        protocolItem.team,
//        chooseTeam(protocolItem, teamWidth),
//        if (
//          protocolItem.team.length > teamWidth ||
//            protocolItem.team.contains(personallyTeamName) ||
//            protocolItem.team.isEmpty
//        ) {
//          personallyTeamName
//        } else {
//          protocolItem.team
//        },
        protocolItem.sportsCategory.getOrElse("б/р"),
        protocolItem.number,
        protocolItem.birthdate,
        protocolItem.result,
        renderedPlace,
      )

      val protocolItemStr = values.zip(widths).map {
        case (value, (_, spacesWidth, width, sing)) =>
          val spaces = if (spacesWidth > 0) String.format(s"%${spacesWidth}s", " ") else ""
          val str = if (width != 0) String.format(s"%${width * sing}s", value) else value
          spaces + str
      }.mkString("")

      (
        protocolItemTemplate.replace("${protocol_item}", protocolItemStr) :: rows,
        protocolItem.place,
        place
      )
    } //._1.reverse.mkString("\n")
    (a._1.reverse.mkString("\n"), a._2, a._3)
  }

  private def isRepeatedPlace(maybePlace: Option[Int], prevPlace: Option[Int]): Boolean = {
    maybePlace.exists(_ => maybePlace == prevPlace)
  }

  private def renderPlace(maybePlace: Option[Int], prevProtocolPlace: Int, repeated: Boolean): String = {
    maybePlace.map(_ =>
      if (repeated) {
        s"=$prevProtocolPlace"
      } else {
        prevProtocolPlace.toString
      }
    ).getOrElse("")
  }

  def main(args: Array[String]): Unit = {
    val csvPath = "competitions.csv"
    val competitions = Csv.load(csvPath)
    //
    val parser = new WinOrientTableParser()
    //
    //    println(parser.parse(competitions.head))

    val protocols = parser.parse(competitions.head)

    val protocol = protocols.head
    //    val csvPath = "./competitions.csv"

    //    val sourcePath = "./source_table2.html"

    //    val parser = new WinOrientTableParser(sourcePath)

    //    val protocol = parser.parse().head




    render(
      protocols.filterNot(_.distance.isOpen)
        .map(ProtocolItemFilter.filter)
        .map(protocol => protocol.copy(
          table = protocol.table ++ protocol.table ++ protocol.table ++ protocol.table ++ protocol.table
        ))
    )
//    ZipUtils.zipIt(new File(inputDocx), new File(outputDocx))

  }

  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(f)
    try {
      op(p)
    } finally {
      p.close()
    }
  }
}
