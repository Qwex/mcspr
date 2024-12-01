package ru.qwex.mcspr.renders

/**
 *
 * @author Aleksander Marenkov <a.marenkov at itgrp.ru>
 */
case class LayoutColumn(header: String, width: Int, leftAlign: Boolean)

case class LayoutTable(
                        columns: List[LayoutColumn],
                        width: Int,
                      ) {

  def headerHard: String = {
    "№п/п Фамилия, имя                       Коллектив            Квал Номер   ГР  Результат    Место"
  }

  def header: String = {
    def loop(columns: List[LayoutColumn], nextColumnStart: Int = 0, result: String = ""): String = {
      columns match {
        case _ if result.length > width =>
          result.take(width)
        case column :: Nil =>
          val columnHeaderWidth = width - result.length
          s"$result${String.format(
            s"%${columnHeaderWidth}s", if (column.header.length <= columnHeaderWidth) {
              column.header.drop(columnHeaderWidth - (column.header.length + 1))
            } else {
              column.header
            }
          )}"

        case column :: restColumns =>
          val nextHeaderStart = result.length + 1
          val columnHeaderWidth = {
            Math.max(column.header.length, column.width - (nextHeaderStart - nextColumnStart))
          }
          val nextResult = s"${
            if (result.nonEmpty) s"$result " else ""
          }${
            String.format(s"%-${columnHeaderWidth}s", column.header)
          }"
          loop(restColumns, nextColumnStart + column.width + 1, nextResult)
        case Nil =>
          result
      }
    }
    loop(columns)
  }

}

object LayoutTable {

  def main(args: Array[String]): Unit = {
    val header = LayoutTable(
      List(
        ("№п/п", 4, false),
        ("Фамилия, имя, отчество", 33, false),
        ("Коллектив", 20, false),
        ("Квал", 4, false),
        ("Номер", 4, false),
        ("ДР", 10, false),
        ("Результат", 9, false),
        ("Место", 5, false)
      ).map(p => LayoutColumn(p._1, p._2, p._3)),
      96
    ).header

  }

}
