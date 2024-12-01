package ru.qwex.mcspr.renders.model

/**
 *
 * @author Aleksander Marenkov <a.marenkov at itgrp.ru>
 */
case class DocumentRender(pages: List[PageRender], documentRender: List[String] => String) extends Render {

  val render: String =  documentRender(pages.map(_.render))

}
