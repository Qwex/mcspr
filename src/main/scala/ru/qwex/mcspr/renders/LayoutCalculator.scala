package ru.qwex.mcspr.renders

import java.awt.{Font, Graphics2D}
import java.awt.font.FontRenderContext
import java.awt.geom.AffineTransform
import java.awt.image.BufferedImage

import ru.qwex.mcspr.model.ProtocolData

/**
 *
 * @author Aleksander Marenkov <a.marenkov at itgrp.ru>
 */
object LayoutCalculator {

  private val font = "Times New Roman"

  private val timeNewRomanFontName = "Times New Roman"
  private val courierNewFontName = "Courier New"

//  private val pageHeight = 1050
  private val pageHeight = 9840 - 100// 10250 //9850//10000
  private val pageWidth = 524
  private val styles = Map(
    1 -> (11,162,timeNewRomanFontName, Font.BOLD), // 11 pt tnr
    2 -> (12,177, timeNewRomanFontName, Font.BOLD), // 12 pt tnr
    3 -> (16,236, timeNewRomanFontName, Font.BOLD), // 16 pt tnr
    4 -> (9,131, courierNewFontName, Font.PLAIN), // 9 pt cn
    5 -> (10, 153, courierNewFontName, Font.PLAIN), // 10 pt cn 15? 158
    6 -> (8,115, courierNewFontName, Font.PLAIN)
  )

  private val additionalHeaderWidth = 110

  def calculate(
                  protocolData: ProtocolData,
               ): Layout = {
//    val headerHeight = 2150
    val headerHeight = {
      val conductingOrganizationsHeight = protocolData.header.conductingOrganizations.map { conductingOrganization =>
        calculateHeight(conductingOrganization, 1)
      }.sum



      val competitionHeight = calculateHeight(protocolData.header.competition, 3)
      val competitionDescriptionHeight = calculateHeight("a", 1)
      val placeAndDateDescriptionHeight = calculateHeight("a", 1)
      val titleHeight = styles(2)._2 * 2
      val tableTitleHeight = styles(2)._2 * 3


      List(
        conductingOrganizationsHeight,
        competitionHeight,
        competitionDescriptionHeight,
        placeAndDateDescriptionHeight,
        titleHeight,
        tableTitleHeight
      ).sum
    }


    val (judgeHeight1, judgeHeight2) = {
      val judgeHeight = ((protocolData.footer.judges.length * 2) - 1) * styles(5)._2
      (judgeHeight + 2 * styles(5)._2, judgeHeight)
    }

    val (footerHeight1, footerHeight2) = {
      val rangingHeight = protocolData.footer.ranking.map(_.lines.length * styles(5)._2).getOrElse(0)
      (rangingHeight + judgeHeight1, rangingHeight + judgeHeight2)
    }

//    println(judgeHeight1)


    val mainPageTableMaxHeight1 = pageHeight - headerHeight - footerHeight1
    val mainPageTableMaxHeight2 = pageHeight - headerHeight - footerHeight2
    val mainPageTableMaxHeight3 = pageHeight - headerHeight - judgeHeight1

    val tableHeight = (protocolData.table.length + 1) * styles(4)._2

    if (tableHeight <= mainPageTableMaxHeight1) {
      Layout(1, Seq(protocolData.table.length), 2)
    } else if (tableHeight <= mainPageTableMaxHeight2) {
      Layout(1, Seq(protocolData.table.length), 0)
    } else {
      // val tableRowOnMainPage = (mainPageTableMaxHeight1 - styles(4)._2) / styles(4)._2
      val tableRowOnMainPage = (mainPageTableMaxHeight3 - styles(4)._2) / styles(4)._2
      val additionalPageHeaderSample = protocolData.additionalPageHeader(1, 1)
      val additionalPageHeaderHeight = (Math.ceil(additionalPageHeaderSample.length.toDouble / additionalHeaderWidth).toInt + 1) * styles(6)._2

      val additionalPageTableMaxHeight1 = pageHeight - additionalPageHeaderHeight - footerHeight1
      val additionalPageTableMaxHeight2 = pageHeight - additionalPageHeaderHeight - judgeHeight1

      val additionPageMaxTableRows1 = Math.ceil((additionalPageTableMaxHeight1 - styles(4)._2).toDouble / styles(4)._2).toInt
      val additionPageMaxTableRows2 = Math.ceil((additionalPageTableMaxHeight2 - styles(4)._2).toDouble / styles(4)._2).toInt

      def group(count: Int, groups: List[Int] = Nil): List[Int] = {
        if (count <= additionPageMaxTableRows1) {
          groups :+ count
        } else if (count == additionPageMaxTableRows2) {
          group(1, groups :+ (additionPageMaxTableRows2 - 1))
        } else {
          group(count - additionPageMaxTableRows2, groups :+ additionPageMaxTableRows2)
        }
      }

//      val a = protocolData
//        .table.drop(tableRowOnMainPage)
//        .grouped(additionPageMaxTableRows)
//        .map(_.length)
//        .toList

      Layout(1,tableRowOnMainPage :: group(protocolData.table.drop(tableRowOnMainPage).length) , 2)
    }

  }

  private val img: BufferedImage = new BufferedImage(1, 1, BufferedImage.TYPE_INT_ARGB)
  private val g2d: Graphics2D = img.createGraphics
  private val affinetransform = new AffineTransform()
  private val frc = new FontRenderContext(affinetransform,true,true)

  private def calculateHeight(string: String, styleCode: Int): Int = {
    val (fontSize, fontHeight, fontName, fontWeight) = styles(styleCode)
    val font = new Font(fontName, fontWeight,  fontSize)

    val stringWidth = font.getStringBounds(string, frc).getWidth

    Math.ceil(stringWidth / pageWidth.toDouble).toInt * fontHeight
  }
  //233

}

case class Layout(pages: Int, tableRowOnPage: Seq[Int], judgePadding: Int ) {


  val pagesCount: Int = tableRowOnPage.length

}
