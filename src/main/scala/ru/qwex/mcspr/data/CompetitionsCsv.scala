package ru.qwex.mcspr.data

import java.io.File

import com.github.tototoshi.csv._
import ru.qwex.mcspr.data.format.ExcelFormat
import ru.qwex.mcspr.model.Judge

import scala.annotation.tailrec

/**
 *
 * @author Aleksander Marenkov <a.marenkov at itgrp.ru>
 */
object Csv {

  def main(args: Array[String]): Unit = {
    println("вaв".matches(".*[a-zA-Z].*"))

    //    val B = SectionLine(1)
    //
    //    Seq.empty[Seq[String]] match {
    //      case B(a) => println(a)
    //    }

    //    Seq.empty[String] match {
    //      case EmptyLine(true) => println("Hi!")
    //      case _ => println("No:(")
    //    }


    //        val lines = load("competitions.csv")

//    val encoding = "cp1251"
    //
    //        val reader = CSVReader.open(new File("competitions.csv"), encoding)
    //
    //    val lines = reader.all().filterNot(_.mkString("").trim.isEmpty)
    //    reader.close()
//    println(load("competitions.csv"))

  }

  def load(path: String): List[Competition] = {
    val encoding = "cp1251"
    val reader = CSVReader.open(new File(path), encoding)(ExcelFormat)
    val lines = reader.all().filterNot(_.mkString("").trim.isEmpty)
    reader.close()
    parse(lines)
  }

  private val conductingOrganizationsSectionName = "проводящие организации"
  private val judgeSectionName = "судьи"
  private val competitionSectionName = "соревнования"

  private object CompetitionColumnKey {
    val competition: String = "название соревнования"
    val date: String = "дата"
    val discipline: String = "дисциплина"
    val disciplineCode: String = "код дисциплины"
    val registry: String = "реестр"
    val place: String = "место"
    val file: String = "файл"
    val shortName: String = "сокр. название"
    val saveAs: String = "сохранить как"

    val list = List(competition, date, discipline, disciplineCode, registry, place, file, shortName, saveAs)

    val all = list.toSet
  }

  private object JudgeColumnKey {
    val name: String = "имя"
    val position: String = "должность"
    val qualification: String = "квалификация"
    val needSign: String = "нужна подпись"

    val list = List(name, position, qualification, needSign)

    val all = list.toSet

    val required = Set(name, position, qualification)
  }

  type Lines = List[List[String]]

  def isEmptyLine(line: Seq[String]): Boolean = {
    line.forall(_.trim.isEmpty)
  }

  private object EmptyLine {

    def unapply(line: Lines): Option[Lines] = {
      line match {
        case line :: restLines if isEmptyLine(line) =>
          Some(restLines)
        case _ =>
          None
      }
    }

  }

  private object ConductingOrganizationsLine {

    def unapply(lines: Lines): Option[(List[String], Lines)] = {
      lines match {
        case ConductingOrganizations(conductingOrganizations) :: restLines =>
          Some((conductingOrganizations, restLines))
        case _ =>
          None
      }
    }

  }

  private object ConductingOrganizations {

    def unapply(conductingOrganizationsLine: List[String]): Option[List[String]] = {
      conductingOrganizationsLine match {
        case head :: rest if isSectionStart(head) =>
          Some(rest.filterNot(_.trim.isEmpty))
        case _ => None
      }
    }

  }

  def isSectionStart(field: String): Boolean = {
    isSection(conductingOrganizationsSectionName, Seq(field))
  }

  def normalizeLine(line: Seq[String]): Seq[String] = {
    line.map(_.toLowerCase.trim).filterNot(_.isEmpty)
  }

  private abstract class Section(
                                  sectionName: Option[String],
                                  uniqueColumns: Set[String],
                                  defaultColumns: List[String],
                                  requiredColumns: Set[String] = Set.empty,
                                ) {
    def unapply(lines: Lines): Option[(List[String], Lines)] = {
      lines match {
        case line :: restLines =>
          val normalizedLine = normalizeLine(line)
          sectionName
            .filter(sectionName => normalizedLine.headOption.exists(_.startsWith(sectionName)))
            .flatMap { _ =>
              parseColumns(restLines).orElse(Some((defaultColumns, restLines)))
            }
            .orElse(parseColumns(lines))
        case _ =>
          None
      }
    }

    private def parseColumns(lines: Lines): Option[(List[String], Lines)] = {
      lines match {
        case line :: restLines =>
          val normalizedLine = normalizeLine(line)
          if (uniqueColumns.intersect(normalizedLine.toSet).nonEmpty && requiredColumns.subsetOf(normalizedLine.toSet)) {
            Some(normalizedLine.toList, restLines)
          } else {
            None
          }
      }
    }

    //    private def parseColumns(lines: Lines): Option[(List[String], Lines)] = {
    //      lines match {
    //        case head :: restLines
    //          if uniqueColumns.intersect(head.toSet).nonEmpty && requiredColumns.subsetOf(head.toSet) =>
    //          Some((head, restLines))
    //        case _ => None
    //      }
    //    }
  }

  private object ConductingOrganizationsSection extends Section(
    sectionName = Some(conductingOrganizationsSectionName),
    uniqueColumns = Set.empty,
    defaultColumns = List(conductingOrganizationsSectionName)
  )

  private object JudgesSection extends Section(
    sectionName = Some(judgeSectionName),
    uniqueColumns = JudgeColumnKey.all.diff(CompetitionColumnKey.all),
    defaultColumns = JudgeColumnKey.list
  ) {
    override def unapply(lines: Lines): Option[(List[String], Lines)] = {
      super.unapply(lines)
    }
  }

  private object CompetitionsSection extends Section(
    sectionName = None,
    uniqueColumns = CompetitionColumnKey.all.diff(JudgeColumnKey.all),
    defaultColumns = CompetitionColumnKey.list,
  )

  private object NextSection {

    def unapply(lines: Lines): Boolean = {
      Seq(ConductingOrganizationsSection, JudgesSection, CompetitionsSection)
        .exists(_.unapply(lines).nonEmpty)
    }

  }
  //  private def dropEmptyLines(lines: Lines): Lines = {
  //    lines.dropWhile(isEmptyLine)
  //  }

  def isSection(sectionName: String, line: Seq[String], uniqueColumnKeys: Set[String] = Set.empty): Boolean = {
    val normalizedLine = normalizeLine(line)
    normalizedLine.headOption.exists(_.startsWith(sectionName)) ||
      uniqueColumnKeys.nonEmpty && uniqueColumnKeys.intersect(normalizedLine.toSet).nonEmpty
  }

  private def parse(lines: Lines): List[Competition] = {


    //    def isJudgeSection(line: Seq[String]): Boolean = {
    //      isSection(judgeSectionName, line, JudgeColumnKey.all.diff(CompetitionColumnKey.all))
    //    }
    //
    //    def isCompetitionSection(line: Seq[String]): Boolean = {
    //      isSection(competitionSectionName, line, CompetitionColumnKey.all.diff(JudgeColumnKey.all))
    //    }


    def loop0(
               lines: Lines,
               conductingOrganizations: List[String],
               judges: List[Judge],
               competitions: List[Competition],
             ): List[Competition] = lines match {
      case EmptyLine(restLines) =>
        loop0(restLines, conductingOrganizations, judges, competitions)
      case ConductingOrganizationsSection(_, restLines) =>
        val (conductingOrganizations, restLines2) = readConductingOrganizations(restLines)
        loop0(restLines2, conductingOrganizations, judges, competitions)
      case CompetitionsSection(columns, restLines) =>
        val (competitions2, restLines2) = readCompetitions(restLines, columns, conductingOrganizations, judges)
        loop0(restLines2, conductingOrganizations, judges, competitions ++ competitions2)
      case JudgesSection(columns, restLines) =>
        val (judges2, restLines2) = readJudges(restLines, columns, List.empty)
        loop0(restLines2, conductingOrganizations, judges2, competitions)
      case _ :: restLines =>
        loop0(restLines, conductingOrganizations, judges, competitions)
      case Nil =>
        competitions
    }

    @tailrec
    def readConductingOrganizations(
                                     lines: Lines,
                                     conductingOrganizations: List[String] = List.empty
                                   ): (List[String], Lines) = lines match {
      case EmptyLine(restLines) =>
        readConductingOrganizations(restLines, conductingOrganizations)
      case NextSection() =>
        (conductingOrganizations, lines)
      case (head :: _) :: restLines =>
        readConductingOrganizations(restLines, conductingOrganizations :+ head)
      case _ =>
        (conductingOrganizations, lines)
    }


    //    def loop1(lines: Lines, result: List[Competition]): List[Competition] = lines match {
    //      case EmptyLine(restLines) =>
    //        loop1(restLines, result)
    //      case ConductingOrganizationsLine(conductingOrganizations, columnsRow :: restLines) =>
    //        val columns = columnsRow.filterNot(_.trim.isEmpty).map(_.toLowerCase)
    //        val (competitions, restLines2) = readCompetitions(restLines, columns, conductingOrganizations, Nil)
    //        loop1(restLines2: Lines, result ++ competitions)
    //
    //      //      case (head :: conductingOrganizationsLine) :: columnsRow :: restLines if isSectionStart(head) =>
    //      //        val conductingOrganizations = conductingOrganizationsLine.filterNot(_.trim.isEmpty)
    //      //        val columns = columnsRow.filterNot(_.trim.isEmpty).map(_.toLowerCase)
    //      //        val (competitions, restLines2) = loop(restLines, columns, conductingOrganizations, Nil)
    //      //        loop1(restLines2: Lines, result ++ competitions)
    //      //      case (head :: _) :: columnsRow :: restLines if isJudgeSection(head) =>
    //
    //      case head :: restLines =>
    //        loop1(restLines, result)
    //      case Nil => result
    //    }

    def readJudges(
                    lines: Lines,
                    columns: List[String],
                    judges: List[Judge],
                  ): (List[Judge], Lines) = {
      lines match {
        case EmptyLine(restLines) =>
          readJudges(restLines, columns, judges)
        case NextSection() =>
          (judges, lines)
        case fields :: restLines =>
          val values = columns.zip(fields).toMap
          val judge = Judge(
            name = values.getOrElse(JudgeColumnKey.name, ""),
            position = values.getOrElse(JudgeColumnKey.position, ""),
            qualification = values.getOrElse(JudgeColumnKey.qualification, ""),
          )
          readJudges(restLines, columns, judges :+ judge)
        case _ =>
          (judges, lines)
      }


    }

    def readCompetitions(
                          lines: Lines,
                          columns: List[String],
                          conductingOrganizations: List[String],
                          judges: List[Judge],
                          result: List[Competition] = List.empty,
                        ): (List[Competition], Lines) = {
      lines match {
        case EmptyLine(restLines) =>
          readCompetitions(restLines, columns, conductingOrganizations, judges, result)
        case (head :: _) :: _ if isSectionStart(head) =>
          (result, lines)
        case fields :: restLines =>
          val values = columns.zip(fields).toMap
          val file = values.getOrElse(CompetitionColumnKey.file, "")
          val name = values.getOrElse(CompetitionColumnKey.competition, "")
          val competition = Competition(
            conductingOrganizations = conductingOrganizations,
            judges = judges,
            name = name,
            date = values.getOrElse(CompetitionColumnKey.date, ""),
            discipline = values.getOrElse(CompetitionColumnKey.discipline, ""),
            disciplineCode = values.getOrElse(CompetitionColumnKey.disciplineCode, ""),
            registry = values.getOrElse(CompetitionColumnKey.registry, ""),
            place = values.getOrElse(CompetitionColumnKey.place, ""),
            file = file,
            saveAs = values.getOrElse(CompetitionColumnKey.saveAs, file),
            shortName = values.getOrElse(CompetitionColumnKey.shortName, name),
          )
          readCompetitions(restLines, columns, conductingOrganizations, judges, result :+ competition)
        case _ =>
          (result, lines)
      }
    }

    loop0(lines, Nil, Nil, Nil)
  }

}

case class Competition(
                        conductingOrganizations: List[String],
                        judges: List[Judge],
                        name: String,
                        shortName: String,
                        date: String,
                        discipline: String,
                        disciplineCode: String,
                        registry: String,
                        place: String,
                        file: String,
                        saveAs: String,
                      )
