package ru.qwex.mcspr

import java.io.File

import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory
import ru.qwex.mcspr.data.Csv
import ru.qwex.mcspr.model.ProtocolItemFilter
import ru.qwex.mcspr.parsers.WinOrientTableParser
import ru.qwex.mcspr.renders.DocxRender
import ru.qwex.mcspr.utils.ZipUtils

import scala.util.Try


/**
 *
 * @author Aleksander Marenkov <a.marenkov at itgrp.ru>
 */
object Boot {
  private val version = "0.0.1.3"

  private val inputDocx = "templates/document"

  lazy val logger: Logger = Logger(LoggerFactory.getLogger(s"${getClass.getName}"))

  def main(args: Array[String]): Unit = {
    val parsedArgs = parseArgs(args)

    if (parsedArgs.printVersion) {
      printVersion()
      return
    }

    try {
      val inputDocxFile = new File(inputDocx)
      if (!inputDocxFile.exists() || !inputDocxFile.isDirectory) {
        throw new Exception(s"Потеряны файлы шаблона docx документа: ${inputDocxFile.getAbsolutePath}")
      }

      val fileName = parsedArgs.fileName
        .getOrElse {throw new Exception("Не передан аргумент пути файла к списку соревнований!")}
      val file = new File(fileName)

      if (!file.isFile) {
        throw new Exception("Список соревнований должен быть файлом!")
      }

      val competitions = Try{Csv.load(file.getPath)}.recover {
        case ex: Throwable =>
          throw new Exception(s"Не удалось прочитать файл списка соревнований: ${ex.getMessage}")
      }.getOrElse(List.empty)

      val parser = new WinOrientTableParser()

      for {
        competition <- competitions
      } yield {
        try {
          val competitionName = competition.name

          val competitionFilePath = competition.file
          val competitionFile = new File(competitionFilePath)
          if (competitionFile.exists()) {
            val protocols = Try {
              parser.parse(competition)
            }.recover {
              case ex: Throwable =>
                throw new Exception(s"Не удалось прочитать файл с протоколами!")
            }.getOrElse(Seq.empty)

            DocxRender.render(
              protocols.filterNot(_.distance.isOpen)
                .map(ProtocolItemFilter.filter)
            )

            val outputDocx = buildSaveAs(competition.saveAs, competition.saveAs)
            val outputDocxFile = new File(outputDocx)
            ZipUtils.zipIt(inputDocxFile, outputDocxFile)
            if (outputDocxFile.exists()) {
              println(s"Записан файл соревнований \"${competition.name}\":  \"${outputDocxFile.getAbsolutePath}\"")
            } else {
              throw new Exception(
                s"Не удалось записать файл соревнований \"${competition.name}\": \"${outputDocxFile.getAbsolutePath}\""
              )
            }
          } else {
            println(s"Входной файл для соревнования $competitionName Файл $competitionFilePath не найден!")
          }
        } catch {
          case ex: Throwable =>
            println(s"Ошибка: ${ex.getMessage}")
        }

      }

    } catch {
      case ex: Throwable =>
        println(s"Ошибка: ${ex.getMessage}")
    }

  }

  private val versionKeys = Set("-v", "--version")

  private def parseArgs(args: Array[String]): Args = {
    val printVersion = args.exists(versionKeys.contains)
    val args2 = args.filterNot(versionKeys.contains)
    val fileName = args2.headOption

    Args(
      fileName = fileName,
      printVersion = printVersion,
    )
  }

  private def buildSaveAs(saveAs: String, current: String, maybeI: Option[Int] = None): String = {
    val file = new File(s"$current.docx")
    if (file.exists()) {
      val i = maybeI.getOrElse(0) + 1
      buildSaveAs(saveAs, s"${saveAs}_($i)", Some(i))
    } else {
      s"${file.getPath}"
    }
  }

  private def printVersion(): Unit = {
    println(s"Version: $version")
  }

}

case class Args(
                 fileName: Option[String],
                 printVersion: Boolean,
               )
