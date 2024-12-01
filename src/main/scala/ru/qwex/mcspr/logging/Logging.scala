package ru.qwex.mcspr.logging

import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory

/**
 *
 * @author Aleksander Marenkov <a.marenkov at itgrp.ru>
 */
trait Logging {

  protected lazy val logger: Logger = Logger(LoggerFactory.getLogger(s"${getClass.getName}"))

}
