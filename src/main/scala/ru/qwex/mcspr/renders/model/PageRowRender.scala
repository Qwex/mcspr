package ru.qwex.mcspr.renders.model

/**
 *
 * @author Aleksander Marenkov <a.marenkov at itgrp.ru>
 */
case class PageRowRender(row: String, rowRender: String => String) extends Render {

  val render: String = rowRender(row)

}
