package ru.qwex.mcspr.renders.model

/**
 *
 * @author Aleksander Marenkov <a.marenkov at itgrp.ru>
 */
case class PageRender(rows: List[PageRowRender], pageRender: List[String] => String) extends Render {

  lazy val render: String = pageRender(rows.map(_.render))

}
