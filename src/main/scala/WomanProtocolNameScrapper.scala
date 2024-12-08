import java.io.{File, FileWriter}
import java.nio.file.{FileSystems, Files}

import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import ru.qwex.mcspr.utils.RegexUtils

import scala.jdk.CollectionConverters._

/**
 *
 * @author Aleksander Marenkov <a.marenkov at itgrp.ru>
 */
object WomanProtocolNameScrapper {
  val protocolsDirectory = "protocols"

  def main(args: Array[String]): Unit = {
    val fileNames = loadFileNames()//.filter(_.endsWith("Moskva_2023_night_.htm"))
//    loadAndSaveProtocols()
//
//    val hrefs = loadProtocolHrefs().take(3) //.slice(1, 3)
//
    val names = fileNames.foldLeft(List.empty[String]) {
      case (names, fileName) =>
        println(fileName)
        val nextNames = loadProtocol(fileName)
          .map(_.toLowerCase)
          .filterNot(_.endsWith("."))
          .filterNot(_.contains("лично"))
          .filterNot(_.contains("мск"))
          .map(_.replaceAll("\\*", ""))
          .filter(_.matches(s"^([${RegexUtils.cyrillicSymbols}-]|\\s)+"))
          .filter(_.nonEmpty)
          .filterNot(_.endsWith(" ели"))
          .filterNot(_.startsWith("ф "))
          .filterNot(_.endsWith(" алексей"))
//        Thread.sleep(100)
        (names ++ nextNames).distinct
    }
//    names.sorted.foreach(println)
    names.map(_.split(" ").reverse.mkString(" ")).sorted.foreach(println)
//    println(names.length)
//    names.flatMap(_.split(" ").drop(1)).distinct.sorted.foreach(println)
//    println(names.flatMap(_.split(" ").drop(1)).distinct.length)
  }

  def loadAndSaveProtocols(): Unit = {
    val hrefs = loadProtocolHrefs()
    for {
      (href, index) <- hrefs.zipWithIndex.drop(757)
      document = Jsoup.connect(href).get()
      title = document.select("title").text()
      if (title.toLowerCase.contains("winorient") && !document.text().toLowerCase.contains("эстафета"))
    } yield {
      println(s"${index + 1}: $href")
      val fileName = s"$protocolsDirectory/${href.split("/").last.replaceAll("\\*", "_")}"
      val fileWriter = new FileWriter(new File(fileName))
      fileWriter.write(document.toString)
      fileWriter.close()
      Thread.sleep(10)
    }
  }

  def loadFileNames(): List[String] = {
    val dir = FileSystems.getDefault.getPath(protocolsDirectory)
    Files.list(dir).iterator().asScala.toList.map(_.toFile.getPath)
  }


  def loadProtocolHrefs(): List[String] = {
    val document = Jsoup.connect("http://o-mephi.net/index.php?pid=113").get()
    val protocolHrefs = document
      .select("table").get(22).select("a")
      .asScala
      .filter(
        a => a.text() == "Результаты"
      )
      .map(_.attr("href"))
      .map(href => if (href.startsWith("http")) href else s"http://o-mephi.net/$href")
    //    protocolHrefs.foreach(println)
    protocolHrefs.toList
  }

  def loadProtocol(href: String): List[String] = {
//    val document = Jsoup.connect(href).get()
    val document = Jsoup.parse(new File(href), "UTF8")
    val title = document.select("title").text()
    if (title.toLowerCase.contains("winorient") && !document.text().toLowerCase.contains("эстафета")) {
      parseProtocol(document)
    } else {
      List.empty
    }
  }

  def parseProtocol(document: Document): List[String] = {
    val headerWithProtocols = document.select("h2,pre").asScala
      .foldLeft((List.empty[(String, List[String])], Option.empty[String])) {
        case ((result, Some(header)), next) =>
          if (next.tag().getName == "pre" && next.text.startsWith("№п/п")) {
            (result :+ (header,
              next
                .text()
                .split("\\n")
                .drop(1)
                .map(_.replaceAll("\\s+", " "))
                .filter(_.matches("^\\s*\\d+ .*"))
                .map(row =>
                  row.trim.split(" ").slice(1, 3).mkString(" ")
                ).toList
            ),
              None
            )
          } else {
            (result, None)
          }
        case ((result, None), next) =>
          if (next.tag().getName == "h2" && next.text().toLowerCase().startsWith("ж")) {
            (result, Some(next.text()))
          } else {
            (result, None)
          }
      }._1
    headerWithProtocols.flatMap(_._2)
  }

}
