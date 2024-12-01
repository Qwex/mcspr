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
case class WinOrientTableParserSource(
                                       document: Document,
                                       applications: Applications,
                                     )


object WinOrientTableParserSource {

  def apply(sourcePath: String): Try[WinOrientTableParserSource] = Try {
    val sourceFile = new File(sourcePath)
    if (sourceFile.isDirectory) {
      readDirectory(sourceFile)
    } else if (sourceFile.isFile) {
      readFile(sourceFile)
    } else {
      throw new Exception(s"Исходный файл \"${sourceFile}\" не найден!")
    }
  }

  private def readFile(file: File): WinOrientTableParserSource = {
    WinOrientTableParserSource(
      document = readHtml(file),
      applications = Applications.empty,
    )
  }

  private def readDirectory(directory: File): WinOrientTableParserSource = {
    val files = directory.listFiles()
    val htmlFile = files.find(_.getName.endsWith(".html"))
      .getOrElse(throw new Exception(s"В директории \"${directory}\" файл с расширением .html не найден!"))
    val maybeCsvFile = files.find(_.getName.endsWith(".csv"))

    WinOrientTableParserSource(
      document = readHtml(htmlFile),
      applications = maybeCsvFile.map(readCsv).getOrElse(Applications.empty),
    )
  }

  private def readHtml(file: File): Document = {
    Jsoup.parse(file)
  }

  private def readCsv(file: File): Applications = {
    ApplicationsCsv.load(file.getPath)
  }

}
