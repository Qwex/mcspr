package ru.qwex.mcspr.parsers

import ru.qwex.mcspr.data.Competition
import ru.qwex.mcspr.model.ProtocolData

/**
 *
 * @author Aleksander Marenkov <a.marenkov at itgrp.ru>
 */
trait Parser {

  def parse(competition:Competition, sources: ParserSource): Seq[ProtocolData]

}
