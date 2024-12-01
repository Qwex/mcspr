package ru.qwex.mcspr.renders

import scala.io.Source
import scala.util.matching.Regex

/**
 *
 * @author Aleksander Marenkov <a.marenkov at itgrp.ru>
 */
case class DocxTemplate(template: String, keys: String*) {

  def substitute(substitutions: String*): String = {
    keys.zip(substitutions).foldLeft(template) {
      case (result, (key, substitution)) => result.replace(key, substitution)
    }
  }

}

object DocxTemplate {

  private val keyRegex: Regex = "\\$\\{((\\w|\\d|_)+)\\}".r

  def load(path: String): DocxTemplate = {
    val source = Source.fromFile(path)
    val sourceTemplate = source.getLines().mkString("\n")
    source.close()
    compile(sourceTemplate)
  }

  private def compile(sourceTemplate: String): DocxTemplate = {
    DocxTemplate(sourceTemplate, keyRegex.findAllMatchIn(sourceTemplate).toList.map(_.toString()): _*)
  }

}
