package ru.qwex.mcspr.model

import com.typesafe.config.Config

import scala.jdk.CollectionConverters._

/**
 *
 * @author Aleksander Marenkov <a.marenkov at itgrp.ru>
 */
object RemovePerson {

  private type Cond = ProtocolItem => Boolean

  def apply(config: Config, basePath: String): RemovePerson = {
    val baseConfig = config.getConfig(basePath)
    RemovePerson(cond = or(baseConfig))
  }

  private def or(config: Config): Cond = { protocolItem =>
    config
      .root()
      .keySet()
      .asScala
      .exists(key => buildCond(key, config)(protocolItem))
  }

  private def and(config: Config): Cond = { protocolItem =>
    config
      .root()
      .keySet()
      .asScala
      .forall(key => buildCond(key, config)(protocolItem))
  }

  private def buildCond(key: String, config: Config): Cond = { protocolItem =>
    key match {
      case "or" => or(config)(protocolItem)
      case "and" => and(config)(protocolItem)
      case "by-names" => buildByNames(config)(protocolItem)
    }
  }

  private def buildByNames(config: Config): Cond = { protocolItem =>
    config
      .root()
      .keySet()
      .asScala
      .forall {
        case "start-with" => buildStartWith(config)(protocolItem)
        case "same-as" => sameAs(config: Config)(protocolItem)
      }
  }

  private def buildStartWith(config: Config): Cond = {
    val values = config.getStringList("start-with").asScala.map(_.toLowerCase.trim)
    (protocolItem: ProtocolItem) => values.exists(protocolItem.name.toLowerCase.trim.startsWith)
  }

  private def sameAs(config: Config): Cond = {
    val values = config.getStringList("same-as").asScala.map(_.toLowerCase.trim)
    (protocolItem: ProtocolItem) => values.contains(protocolItem.name.toLowerCase.trim)
  }

}

case class RemovePerson(cond: ProtocolItem => Boolean) {

}

object RemovePersonByGroupByNames {
  private val groupNamePath = "group-name"
  private val namesPath = "names"

  def apply(config: Config, basePath: String): RemovePersonByGroupByNames = {
    val baseConfig = config.getConfig(basePath)
    RemovePersonByGroupByNames(
      groupName = baseConfig.getString(groupNamePath).toLowerCase.trim,
      names = baseConfig.getStringList(namesPath).asScala.map(_.toLowerCase.trim).toList
    )
  }

}

case class RemovePersonByGroupByNames(
                                       groupName: String,
                                       names: List[String]
                                     ) {

  def hasGroup(otherGroupName: String): Boolean = {
    otherGroupName.toLowerCase.trim == groupName
  }

  def hasName(name: String): Boolean = {
    names.contains(name)
  }

}
