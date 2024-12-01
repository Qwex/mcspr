package ru.qwex.mcspr.data

import ru.qwex.mcspr.data.Applications.buildIndexKey
import ru.qwex.mcspr.data.CSVReaderUtils.{Line, Lines}

/**
 *
 * @author Aleksander Marenkov <a.marenkov at itgrp.ru>
 */
object ApplicationsCsv {

  private object ColumnKey {
    val group: String = "группа"
    val surname: String = "фамилия"
    val name: String = "имя"
    val team: String = "команда"

    val all: Set[String] = Set(group, surname, name, team)
  }

  private val encoding = "UTF8"

  def load(path: String): Applications = {
    parse(CSVReaderUtils.load(path, encoding))
  }

  private object ColumnLine {

    def unapply(columnsLine: Line): Option[Line] = {
      val columns = columnsLine.map(_.trim.toLowerCase)

      if (ColumnKey.all.subsetOf(columns.toSet)) {
        Some(columns)
      } else {
        None
      }
    }

  }

  private def parse(lines: Lines): Applications = {
    lines match {
      case ColumnLine(columns) :: restLines =>
        val applications = restLines.filter(_.exists(_.trim.nonEmpty)).map { line =>
          val map = columns.zip(line).toMap
          Application(
            group = map.getOrElse(ColumnKey.group, ""),
            name = map.getOrElse(ColumnKey.name, ""),
            surname = map.getOrElse(ColumnKey.surname, ""),
            team = map.getOrElse(ColumnKey.team, ""),
          )
        }
        new Applications(applications)
      case _ =>
        Applications.empty

    }
  }


}

class Applications(
                    val applications: List[Application],
                  ) {

  private lazy val index = applications.groupBy(application =>
    buildIndexKey(application.group.toLowerCase, application.surname.toLowerCase, application.name.toLowerCase)
  )

  def findApplications(group: String, surname: String, name: String): List[Application] = {
    findApplications(buildIndexKey(group, surname, name))
  }

  def findApplications(group: String, name: String): List[Application] = {
    findApplications(buildIndexKey(group, name))
  }

  private def findApplications(indexKey: String): List[Application] = {
    index.getOrElse(indexKey, List.empty)
  }

}

object Applications {

  def buildIndexKey(group: String, surname: String, name: String): String = {
    buildIndexKey(group, List(surname, name).map(_.trim.toLowerCase).filter(_.nonEmpty).mkString(" "))
  }

  def buildIndexKey(group: String, name: String): String = {
    List(group, name).map(_.trim.toLowerCase).filter(_.nonEmpty).mkString(" ")
  }

  lazy val empty: Applications = new Applications(List.empty)

}

case class Application(
                        group: String,
                        name: String,
                        surname: String,
                        team: String,
                      )
