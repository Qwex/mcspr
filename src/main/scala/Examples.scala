/**
 *
 * @author Aleksander Marenkov <a.marenkov at itgrp.ru>
 */
object Examples {

  def main(args: Array[String]): Unit = {
    val p = "/bb/vvv/fdsfsd/vvv/"

      println(p.substring(0, p.lastIndexOf("/", p.lastIndexOf("/") - 1)))

//    println("/bb/vvv/fdsfsd/vvv/".matches("/.*/.*/"))

    List(1,2,3).sorted


  }

}
