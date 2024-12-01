package ru.qwex.mcspr.utils

import java.io.{File, FileInputStream, FileOutputStream, IOException}
import java.util.zip.{ZipEntry, ZipOutputStream}

import scala.annotation.tailrec

/**
 *
 * @author Aleksander Marenkov <a.marenkov at itgrp.ru>
 */
object ZipUtils {

  def generateFileList(node: File): List[File] = {
    if (node.isFile) {
      List(node)
    } else if (node.isDirectory) {
      node.listFiles().toList.foldLeft(List.empty[File]) {
        case (files, node) => files ++ generateFileList(node)
      }
    } else {
      Nil
    }
  }

  def zipIt(sourceFile: File, zipFile: File): Unit = {
    val sourcePath = sourceFile.getPath
    val buffer = new Array[Byte](1024)

    if (sourceFile.isDirectory) {
      val fos = new FileOutputStream(zipFile)
      val zos = new ZipOutputStream(fos)

      @tailrec
      def loopFiles(files: List[File], paths: Set[String]): Unit = files match {
        case file :: restFiles =>
          val path = {
            val path = Option(file.getParent.trim()).map(_.substring(sourcePath.length())).getOrElse("")
            if (path.startsWith(File.separator)) {
              path.substring(1)
            } else {
              path
            }
          }

          val maybeAddedPath: Option[String] = if (path.nonEmpty) {
            if (!paths.contains(path)) {
              val ze = new ZipEntry(path + "/")
              zos.putNextEntry(ze)
              zos.closeEntry()
              Some(path)
            } else {
              None
            }
          } else {
            None
          }

          val entryName = (if (path.nonEmpty) path + "/" else path) + file.getName
          val ze = new ZipEntry(entryName)
          zos.putNextEntry(ze)

          val in = new FileInputStream(file)
          try {
            @tailrec
            def loop(): Unit = {
              val len = in.read(buffer)
              if (len > 0) {
                zos.write(buffer, 0, len)
                loop()
              }
            }

            loop()
          } finally {
            in.close()
          }
          loopFiles(restFiles, paths ++ maybeAddedPath.toSet)
        case Nil => ()
      }

      try {
        loopFiles(generateFileList(sourceFile), Set.empty)
      } catch {
        case ex: IOException => ex.printStackTrace();
      } finally {
        zos.close()
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val widths = Seq("4")


//    println(String.format("a = %10s", "h"))

//    val sourceFile = new File("E:\\qwex\\projects\\mcspr\\тесты\\test_build")
//    val zip = new File("E:\\qwex\\projects\\mcspr\\тесты\\ttt.docx")
//
//
//    zipIt(sourceFile, zip)
    //    generateFileList(new File("E:\\qwex\\projects\\mcspr\\тесты\\test_build\\word")).foreach(println)

  }

}
