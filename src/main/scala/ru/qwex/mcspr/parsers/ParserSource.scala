package ru.qwex.mcspr.parsers

import java.io.File

import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import ru.qwex.mcspr.data.{Applications, ApplicationsCsv}

import scala.util.Try

/**
 *
 * @author Aleksander Marenkov <a.marenkov at itgrp.ru>
 */
case class ParserSource(
                         sourceFile: File,
                         applications: Applications,
                       )


object ParserSource {

  def apply(sourcePath: String, extensions: Set[String]): Try[ParserSource] = Try {
    val sourceFile = new File(sourcePath)
    if (sourceFile.isDirectory) {
      readDirectory(sourceFile, extensions)
    } else if (sourceFile.isFile) {
      readFile(sourceFile)
    } else {
      throw new Exception(s"Исходный файл \"${sourceFile}\" не найден!")
    }
  }

  private def readFile(file: File): ParserSource = {
    ParserSource(
      sourceFile = file,
      applications = Applications.empty,
    )
  }

  private def readDirectory(directory: File, extensions: Set[String]): ParserSource = {
    val files = directory.listFiles()
    val sourceFile = files.find(file => extensions.exists(file.getName.endsWith))
      .getOrElse(throw new Exception(s"В директории \"${directory}\" файл с одним из расширений ${extensions.mkString("|")} не найден!"))
    val maybeCsvFile = files.find(_.getName.endsWith(".csv"))

    ParserSource(
      sourceFile = sourceFile,
      applications = maybeCsvFile.map(readCsv).getOrElse(Applications.empty),
    )
  }

  private def readCsv(file: File): Applications = {
    ApplicationsCsv.load(file.getPath)
  }

}
