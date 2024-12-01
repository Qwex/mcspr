package ru.qwex.mcspr.data.format

import com.github.tototoshi.csv.{CSVFormat, Quoting}

/**
 *
 * @author Aleksander Marenkov <a.marenkov at itgrp.ru>
 */
object ExcelFormat extends CSVFormat {

  import CSVFormat._

  override val delimiter: Char = ';'
  override val quoteChar: Char = defaultCSVFormat.quoteChar
  override val escapeChar: Char = defaultCSVFormat.escapeChar
  override val lineTerminator: String = defaultCSVFormat.lineTerminator
  override val quoting: Quoting = defaultCSVFormat.quoting
  override val treatEmptyLineAsNil: Boolean = true
}
