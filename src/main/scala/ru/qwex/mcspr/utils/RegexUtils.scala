package ru.qwex.mcspr.utils

/**
 *
 * @author Aleksander Marenkov <a.marenkov at itgrp.ru>
 */
object RegexUtils {

  val cyrillicSymbols = "абвгдеёжзийклмнопрстуфхцчшщъыьэюяАБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ"

  def main(args: Array[String]): Unit = {
    println("Амамбаев Тимур Георгиевич".matches(s"^([${RegexUtils.cyrillicSymbols}]|\\s|-)+$$"))
    println("Богопольский Леoнид Геннадьевич".matches(s"^([${RegexUtils.cyrillicSymbols}]|\\s|-)+$$"))
    println("ЛеонидГеннадьевич".matches(s"^([${RegexUtils.cyrillicSymbols}]|\\s|-)+$$"))
  }

}
