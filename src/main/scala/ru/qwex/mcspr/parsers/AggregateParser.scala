package ru.qwex.mcspr.parsers

import ru.qwex.mcspr.data.Competition
import ru.qwex.mcspr.model.ProtocolData

/**
 *
 * @author Aleksander Marenkov <a.marenkov at itgrp.ru>
 */
class AggregateParser(parsers: Map[String, Parser], default: Parser) {

  def parse(competition: Competition): Seq[ProtocolData] = {
    val extensions = parsers.keySet
    val sources = ParserSource(competition.file, extensions).get
    val parser = extensions
      .find(sources.sourceFile.getName.endsWith)
      .flatMap(parsers.get)
      .getOrElse(default)
    parser.parse(competition, sources)
  }

}

object AggregateParser {

  val default:AggregateParser = {
    val winOrientTableParser = new WinOrientTableParser()
    val wdbParser = new PysportWdbParser()

    new AggregateParser(
      parsers = Map(
        "wdb" -> wdbParser,
        "html" -> winOrientTableParser,
        "htm" -> winOrientTableParser
      ),
      winOrientTableParser
    )
  }

}
