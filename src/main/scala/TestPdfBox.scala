import java.io.{File, FileOutputStream, PrintWriter}

import com.itextpdf.text.pdf.PdfReader
import com.itextpdf.text.pdf.parser.{ImageRenderInfo, PdfReaderContentParser, SimpleTextExtractionStrategy, TextExtractionStrategy, TextRenderInfo}
import org.apache.pdfbox.pdmodel.PDDocument
import org.apache.poi.xwpf.usermodel.{BreakType, XWPFDocument}
import org.fit.pdfdom.PDFDomTree

/**
 *
 * @author Aleksander Marenkov <a.marenkov at itgrp.ru>
 */
object TestPdfBox {

  def main(args: Array[String]): Unit = {

    val document = PDDocument.load(new File("E:\\Downloads\\Приказ ГКУ 'ЦСТиСК' Москомспорта от 22.10.2021 г. № 11-Р 'О присвоении (подтверждении) спортивных разрядов.pdf"))


    val output = new PrintWriter("pdf.html", "utf-8")
    println(document.getNumberOfPages)

    new PDFDomTree().writeText(document, output)

    val doc = new XWPFDocument()
    val pdf = "E:\\Downloads\\Приказ ГКУ 'ЦСТиСК' Москомспорта от 22.10.2021 г. № 11-Р 'О присвоении (подтверждении) спортивных разрядов.pdf"

    val reader = new PdfReader(pdf)
    val parser = new PdfReaderContentParser(reader)

    for {
      i <- 1 to reader.getNumberOfPages
    } {
      val strategy = parser.processContent(i, new SimpleTextExtractionStrategy)
      val text = strategy.getResultantText
      val p = doc.createParagraph()
      val run = p.createRun()
      run.setText(text)
      run.addBreak(BreakType.PAGE)
    }

    val out2 = new FileOutputStream("pdf.docx")
    doc.write(out2)

    //    println()
  }

}
