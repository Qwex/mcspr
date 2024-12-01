import sbtassembly.AssemblyPlugin.autoImport.assemblyMergeStrategy
import sbt._

ThisBuild / scalaVersion     := "2.13.7"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    name := "mcspr",
    libraryDependencies += "org.specs2" %% "specs2-core" % "4.13.1" % Test,
    //    libraryDependencies += "org.apache.poi" % "poi" % "5.3.0"
    libraryDependencies += "org.apache.poi" % "poi" % "4.1.2",
    libraryDependencies += "org.apache.poi" % "poi-ooxml" % "4.1.2",
    libraryDependencies += "org.apache.poi" % "poi-ooxml-schemas" % "4.1.2",
    libraryDependencies += "org.jsoup" % "jsoup" % "1.18.1",
    libraryDependencies += "org.playframework" %% "play-json" % "3.0.4",
    libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.11",
    libraryDependencies += "org.slf4j" % "jul-to-slf4j" % "1.7.26",
    libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2",
    libraryDependencies += "com.github.tototoshi" %% "scala-csv" % "2.0.0",
//    libraryDependencies += "fr.opensagres.xdocreport" % "fr.opensagres.odfdom.converter.pdf" % "2.1.0",
//    libraryDependencies += "fr.opensagres.xdocreport" % "fr.opensagres.poi.xwpf.converter.pdf" % "2.0.6",
      assembly / mainClass := Some("ru.qwex.mcspr.Boot"),
    assembly / assemblyMergeStrategy := {
      case PathList("META-INF", "io.netty.versions.properties", _*) => MergeStrategy.first
      case PathList("META-INF", "blueprint.handlers", _*) => MergeStrategy.first
      case PathList("META-INF", "cxf", "bus-extensions.txt", _*) => MergeStrategy.first
      case PathList("META-INF", "jing-copying.html", _*) => MergeStrategy.first
      case PathList("META-INF", "jpms.args", _*) => MergeStrategy.discard
      case PathList("mozilla", "public-suffix-list.txt", _*) => MergeStrategy.first
      case PathList("org", "apache", "tika", "mime", "custom-mimetypes.xml", _*) => MergeStrategy.first
      case x if x.endsWith("module-info.class") => MergeStrategy.discard
      case x if x.endsWith(".proto") => MergeStrategy.discard

      case x =>
        val oldStrategy = (assembly / assemblyMergeStrategy).value
        oldStrategy(x)
    }
  )



