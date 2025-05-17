package ru.qwex.mcspr.wdb

import java.io.{File, FileInputStream}
import java.time.Duration

import play.api.libs.json.{JsValue, Json}

import scala.io.Source
import scala.sys.process._

/**
 *
 * @author Aleksander Marenkov <a.marenkov at itgrp.ru>
 */
object PysportJsonWdbReader {
  private val tmpJsonName = "tmp_json.json"

  def main(args: Array[String]): Unit = {
    read("Примеры протоколов/Pobedy_20240509_res.wdb")
  }

  def read(wdbFilePath: String): JsValue = {
    val source = Source.fromFile(new File(System.getProperty("user.dir"), "py_wdb_command"))
    val command = source.getLines().mkString(" ").trim
    source.close()

//    println(s"${command} \"${wdbFilePath}\" \"${tmpJsonName}\"")

    s"${command} \"${wdbFilePath}\" \"${tmpJsonName}\"".!!
    val json = Json.parse(new FileInputStream(tmpJsonName))
//    new File(tmpJsonName).delete()
    json
  }

}
