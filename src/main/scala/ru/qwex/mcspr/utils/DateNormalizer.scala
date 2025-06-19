package ru.qwex.mcspr.utils

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import scala.util.Try


/**
 *
 * @author Aleksander Marenkov <a.marenkov at itgrp.ru>
 */
object DateNormalizer {

  private val  inDateTimeFormatter = DateTimeFormatter
    .ofPattern("[MM/dd/yyyy]" + "[dd-MM-yyyy]" + "[yyyy-MM-dd]" + "[dd.MM.yyyy]")

  private val outDateFormatter = DateTimeFormatter.ofPattern("dd.MM.yyyy")

  def maybeNormalize(dateString: String): Option[String] = {
    Try(normalize(dateString)).toOption
  }

  def normalize(dateString: String): String = {
    LocalDate.parse(dateString, inDateTimeFormatter).format(outDateFormatter)
  }

  def main(args: Array[String]): Unit = {
    println(normalize("2025-01-25"))
    println(normalize("17-06-2025"))
    println(normalize("2025-13-32"))
  }

}
