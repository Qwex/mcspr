package ru.qwex.mcspr.data

import java.io.File

import com.github.tototoshi.csv.CSVReader
import ru.qwex.mcspr.data.format.ExcelFormat

/**
 *
 * @author Aleksander Marenkov <a.marenkov at itgrp.ru>
 */
object CSVReaderUtils {

  type Line  = List[String]
  type Lines = List[Line]

  def load(path: String, encoding: String): Lines = {

    val reader = CSVReader.open(new File(path), encoding)(ExcelFormat)
    val lines = reader.all()
    reader.close()
    lines

  }

}
