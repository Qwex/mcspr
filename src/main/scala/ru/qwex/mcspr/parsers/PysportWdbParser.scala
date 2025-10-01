package ru.qwex.mcspr.parsers

import play.api.libs.json._
import ru.qwex.mcspr.data.{Competition, CompetitionSettings}
import ru.qwex.mcspr.model._
import ru.qwex.mcspr.parsers.WinOrientTableParser.{findApplication, findTeam}
import ru.qwex.mcspr.ranking.RankCalculator
import ru.qwex.mcspr.wdb.PysportJsonWdbReader

/**
 *
 * @author Aleksander Marenkov <a.marenkov at itgrp.ru>
 */
class PysportWdbParser extends Parser {

  import PysportWdbParser._

  def parse(competition: Competition, sources: ParserSource): Seq[ProtocolData] = {
    val json = PysportJsonWdbReader.read(sources.sourceFile.getAbsolutePath)

    val race = parseRace(json)

    val protocolHeader = ProtocolHeader.from(competition)

    race.getField[List[JsValue]](groupsField).map { group =>
      val distance = parseDistance(group)
      val groupId = group.getField[String](idField)
      val rawGroupName = group.getField[String](nameField)
      val protocolItems = race.getField[List[JsValue]](resultsField)
        .filter { result =>
          result.getField[Int](statusField) != Statuses.DID_NOT_START && {
            for {
              person <- result.getFieldSafe[JsValue](personField)
              personGroup <- person.getFieldSafe[JsValue](groupField)
            } yield personGroup.getField[String](idField) == groupId
          }.getOrElse(false)
        }
        .map { result =>
          if (competition.settings.checkControlTime) {
            val maybeResultMs = result.getFieldSafe[Int](resultMsecField)
            maybeResultMs
              .filter(resultMs =>
                distance.controlTimeMs.exists(_ < resultMs)
              )
              .filter(_ => result.getFieldSafe[Int](statusField).contains(Statuses.OK))
              .map(_ => result
                .updateField[Int](statusField, Statuses.OVERTIME)
                .updateField[Int](placeField, -1)
                .updateField[String](resultCurrentField, "Disqualified")
              )
              .getOrElse(result)
          } else result
        }
        .sorted(resultOrdering)
        .zipWithIndex.map { case (result, index) =>
        val person = result.getField[JsValue](personField)
        val name = List(
          person.getFieldSafe[String](surnameField),
          person.getFieldSafe[String](nameField),
        ).flatten.mkString(" ")
        val teamShort = for {
          organization <- person.getFieldSafe[JsValue](organizationField)
          teamShort <- organization.getFieldSafe[String](nameField)
        } yield teamShort
        val application = findApplication(sources.applications, rawGroupName, name, teamShort)
        val team = findTeam(application, teamShort, competition.settings.maybeMandatoryTeam)
        val maybePlace = result.getFieldSafe[Int](placeField)
        val maybeResultMs = for {
          status <- result.getFieldSafe[Int](statusField)
          if status == Statuses.OK
          resultMs <- result.getFieldSafe[Int](resultMsecField)
        } yield resultMs
        ProtocolItem.from(
          name = name,
          team = team,
          sportsCategory = Some(qualificationName(person.getField[Int](qualField)))
            .filter(_.nonEmpty)
            .filterNot(_.toLowerCase.contains("ю") && !distance.isJunior),
          number = person.getField[Int](bibField).toString,
          yearOfBirth = person.getFieldSafe[Int](yearField).map(_.toString).getOrElse(""),
          result = {
            maybePlace
              .filter(_ > 0)
              .flatMap(_ =>
                result.getFieldSafe[String](resultCurrentField)
              )
              .orElse(result.getFieldSafe[Int](statusField).map(getRule))
              .map(TypedColumn.result.transform)
              .getOrElse("")
          },
          place = person
            .getFieldSafe[Boolean](isOutOfCompetitionField)
            .orElse(Some(false))
            .filter(_ == false)
            .fold(
              "в/к"
            )(
              _ => result.getFieldSafe[Int](placeField).filter(_ > 0).map(_.toString).getOrElse("")
            ),
          comment = person.getFieldSafe[String](commentField),
          application = application,
          maybeResultMs = maybeResultMs,
        )
      }


      val ranking = buildRanking(protocolItems, group)
      ProtocolData(
        header = protocolHeader,
        distance = distance,
        table = protocolItems,
        footer = ProtocolFooter(
          ranking = Some(ranking),
          judges = competition.judges,
        ),
      )
    }.filter(data => data.table.nonEmpty)
  }

  private def buildRanking(protocolItems: List[ProtocolItem], group: JsValue): Ranking = {
    val ranking = group.getField[JsObject](rankingField)
    val isActive = ranking.getFieldSafe[Boolean](isActiveField).getOrElse(false)
    Option
      .when(isActive) {
        ComputableRanking {
          val rank = ranking.getField[List[JsValue]](rankField)
          val qualNames = rank
            .filter(_.getField[Boolean](isActiveField))
            .flatMap(_.getFieldSafe[Int](qualField).filter(_ < 7))
            .flatMap(qual => Some(qualificationName(qual)).filter(_.nonEmpty))

          { protocolItems => RankCalculator.calculate(qualNames, protocolItems) }
        }
      }
      .getOrElse(Ranking.notRanking)
  }

  private def parseRanking(group: JsValue): Ranking = {
    val ranking = group.getField[JsObject](rankingField)
    val isActive = ranking.getFieldSafe[Boolean](isActiveField).getOrElse(false)
    val scores = ranking.getFieldSafe[Double](rankScoresField).getOrElse(0.0).toInt
    Option
      .when(isActive && scores > 0) {
        val scores = ranking.getField[Double](rankScoresField).toInt
        val scoresLine = s"Квалификационный уровень - ${scores} ${declinationScores(scores)}"
        val rank = ranking.getField[List[JsValue]](rankField)
        val rankLines = rank.view
          .filter(_.getField[Boolean](isActiveField))
          .flatMap { r =>
            for {
              qual <- r.getFieldSafe[Int](qualField).filter(_ < 7)
              qualName <- Some(qualificationName(qual)).filter(_.nonEmpty)
              percent <- r.getFieldSafe[Int](percentField).filter(_ > 0)
              time <- r.getFieldSafe[Int](maxTimeField).map(msToTimeString)
            } yield s"${qualName.padTo(maxQualificationNameLength, ' ')} - ${percent}% - ${time}"
          }
          .toList
        Ranking(List(scoresLine) ++ rankLines)
      }
      .filter(_.lines.length > 1)
      .getOrElse(Ranking.notRanking)
  }

  private def declinationScores(scores: Int): String = {
    val rem1 = scores % 100
    val rem2 = rem1 % 10
    if ((rem1 >= 5 && rem1 <= 20) || (rem2 == 0 || (rem1 > 20 && rem2 >= 5))) {
      "баллов"
    } else if (rem2 == 1) {
      "балл"
    } else {
      "балла"
    }
  }

  import java.time.Duration

  private def msToTimeString(ms: Int): String = {
    val duration = Duration.ofMillis(ms + 500)
    val totalSeconds = duration.getSeconds
    val hours = totalSeconds / 3600
    val minutes = (totalSeconds % 3600) / 60
    val seconds = totalSeconds % 60

    f"$hours%02d:$minutes%02d:$seconds%02d"
  }

  private def parseDistance(group: JsValue): Distance = {
    val course = group.getField[JsValue](courseField)
    val length = course.getField[Int](lengthField)
    val kp = course.getField[List[JsValue]](controlsField).length
    val controlTime = course.getFieldSafe[Int](timeLimitField).filter(_ > 0)
    val _group = WinOrientTableParser.parseGroup(group.getField[String](nameField))
    Distance(
      name = _group.name,
      length = length.toString,
      kp = kp,
      controlTime = controlTime.map(_.toString),
      isOpen = _group.isOpen,
      isJunior = _group.isJunior,
      maybeGroupAgeFilter = _group.maybeGroupAgeFilter,
    )
  }

}


object PysportWdbParser {

  val organizationsField: String = "organizations"
  val coursesField: String = "courses"
  val groupsField: String = "groups"
  val resultsField: String = "results"
  val personsField: String = "persons"
  val organizationField: String = "organization"
  val organizationIdField: String = "organization_id"
  val groupField: String = "group"
  val groupIdField: String = "group_id"
  val statusField: String = "status"
  val personField: String = "person"
  val personIdField: String = "person_id"
  val courseField: String = "course"
  val courseIdField: String = "course_id"
  val nameField: String = "name"
  val placeField: String = "place"
  val idField: String = "id"
  val lengthField: String = "length"
  val controlsField: String = "controls"
  val maxTimeField: String = "max_time"
  val timeLimitField: String = "time_limit"
  val surnameField: String = "surname"
  val qualField: String = "qual"
  val bibField: String = "bib"
  val yearField: String = "year"
  val resultCurrentField: String = "result_current"
  val commentField: String = "comment"
  val isOutOfCompetitionField: String = "is_out_of_competition"
  val resultMsecField: String = "result_msec"
  val rankingField: String = "ranking"
  val isActiveField: String = "is_active"
  val rankScoresField: String = "rank_scores"
  val rankField: String = "rank"
  val percentField: String = "percent"


  implicit class JsValueUtils(val json: JsValue) extends AnyVal {

    def getField[T: Reads](field: String): T = {
      (json \ field).as[T]
    }

    def getFieldSafe[T: Reads](field: String): Option[T] = {
      (json \ field).asOpt[T]
    }

    def getPath[T: Reads](subPaths: String*): T = {
      subPaths.foldLeft(json)((obj, subPath) => (obj \ subPath).as[JsObject]).as[T]
    }

    def updateField[T: Writes](field: String, value: T): JsValue = {
      json.asOpt[JsObject].map(_ ++ Json.obj(field -> value)).getOrElse(json)
    }

  }

  implicit class JsObjectUtils(val json: JsObject) extends AnyVal {

    def addField[T: Writes](field: String, value: T): JsObject = {
      json.++(Json.obj(field -> value))
    }

  }

  private def getById(jsons: List[JsValue], id: String): JsValue = {
    jsons.find(_.getField[String]("id") == id).get
  }

  private def lt(obj1: JsObject, obj2: JsObject): Boolean = {
    obj1.getField[String](nameField).toUpperCase < obj2.getField[String](nameField)
  }

  private val resultOrdering: Ordering[JsValue] = (result1: JsValue, result2: JsValue) => {
    val status1 = result1.getField[Int](statusField)
    val status2 = result2.getField[Int](statusField)
    if (status1 != 1 && status2 != 1) {
      getStatusPriority(status1) - getStatusPriority(status2)
    } else if (status1 != 1) {
      1
    } else if (status2 != 1) {
      -1
    } else {
      val isOutOfCompetition1 = result1.getField[JsValue](personField).getField[Boolean](isOutOfCompetitionField)
      val isOutOfCompetition2 = result2.getField[JsValue](personField).getField[Boolean](isOutOfCompetitionField)
      if (isOutOfCompetition1 || isOutOfCompetition2) {
        result1.getField[Int](resultMsecField) - result2.getField[Int](resultMsecField)
      } else {
        val place1 = result1.getField[Int](placeField)
        val place2 = result2.getField[Int](placeField)

        if (place1 < 1) {
          1
        } else if (place2 < 1) {
          -1
        } else {
          if (place1 == place2) {
            val surname1 = result1.getField[JsValue](personField).getField[String](surnameField)
            val surname2 = result2.getField[JsValue](personField).getField[String](surnameField)
            val name1 = result1.getField[JsValue](personField).getField[String](nameField)
            val name2 = result2.getField[JsValue](personField).getField[String](nameField)
            (surname1 + name1).compare(surname2 + name2)
          } else {
            place1 - place2
          }
        }
      }
    }
  }

  private def parseRace(json: JsValue): JsObject = {
    val organizations = json.getField[List[JsObject]](organizationsField).sortWith(lt)
    val courses = json.getField[List[JsObject]](coursesField).sortWith(lt)
    val groups = json.getField[List[JsObject]](groupsField).flatMap { group =>
      group.getFieldSafe[String](courseIdField)
        .map(courseId => group.addField(courseIdField, getById(courses, courseId)))
    }.sortWith(lt)
    val persons = json.getField[List[JsObject]](personsField)
      .map { person =>
        person
          .addField(organizationField, getById(organizations, person.getField[String](organizationIdField)))
          .addField(groupField, getById(groups, person.getField[String](groupIdField)))
      }
    val results = json.getField[List[JsObject]](resultsField).flatMap { result =>
      val status = result.getField[Int](statusField)
      for {
        personId <- result.getFieldSafe[String](personIdField)
        person = getById(persons, personId)
      } yield {
        result
          .addField(statusField, if (status == 16) 1 else status)
          .addField(personField, getById(persons, result.getField[String](personIdField)))
      }
    }

    val race: JsObject = json.asOpt[JsObject].getOrElse(JsObject.empty) ++ Json.obj(
      organizationsField -> organizations,
      coursesField -> courses,
      groupsField -> groups,
      personsField -> persons,
      resultsField -> results,
    )
    race
  }

  def main(args: Array[String]): Unit = {
    val competition = Competition(
      conductingOrganizations = List("Организация 1"),
      judges = List(Judge("имя", "позиция", "квалификация")),
      name = "длинное имя",
      shortName = "короктое имя",
      date = "29.11.1990",
      discipline = "gewr6543",
      disciplineCode = "dst35",
      registry = "21411",
      place = "Какое-то место",
      file = "E:\\qwex\\projects\\vk_photo_downloader\\python\\pythonProject1\\Pobedy_20240509_res.wdb",
      //      file = "data.json",
      saveAs = "",
      stamp = None,
      settings = CompetitionSettings(),
    )

    //    val parser = new PysportWdbParser()
    //    val sources = ParserSource(competition.file, Set(".wdb")).get
    val parser = AggregateParser.default
    //    parser.parse(competition, sources).foreach(println)
    parser.parse(competition).foreach(println)

    //    val file = "data.json"
    //
    //    val json = Json.parse(new FileInputStream(file))
    //
    //
    //    val race = parseRace(json)
    //
    //    race.getField[List[JsObject]](resultsField).map { result =>
    //      val place = result.getField[Int](placeField)
    //      val group = result.getPath[JsObject](personField, groupField)
    //      val person = result.getField[JsObject](personField)
    //      val qual = person.getFieldSafe[Int]("qual").getOrElse(0)
    //      //      println(qualificationName(qual))
    //
    //      //      println(group.getField[String]("id"))
    //      0
    //    }

    //    println()


  }

  private val qualificationOrder: Map[Int, Int] = Map(
    4 -> 1,
    5 -> 2,
    6 -> 3,
    1 -> 4,
    2 -> 5,
    3 -> 6
  )

  private val qualificationMap: Map[Int, String] = Map(
    //    0 -> "б/р",
    0 -> "",
    3 -> "IIIю",
    2 -> "IIю",
    1 -> "Iю",
    6 -> "III",
    5 -> "II",
    4 -> "I",
    7 -> "КМС",
    8 -> "МС",
    9 -> "МСМК"
  )

  private val statusToRule: Map[Int, String] = Map(
    Statuses.DISQUALIFIED -> "3.13.12.2", // DISQUALIFIED
    Statuses.MISSING_PUNCH -> "3.13.12.2", // MISSING_PUNCH
    Statuses.DID_NOT_FINISH -> "6.6.4", // DID_NOT_FINISH
    Statuses.DID_NOT_START -> "7.2.6", // DID_NOT_START
    Statuses.OVERTIME -> "5.4.7", // OVERTIME
    Statuses.MISS_PENALTY_LAP -> "4.6.12.7", // MISS_PENALTY_LAP
  )

  private val statusPriorities: Seq[Int] = Seq(8, 4, 3, 5, 13)

  private val maxQualificationNameLength = qualificationMap.values.map(_.length).max

  private def qualificationName(qualification: Int): String = {
    qualificationMap.getOrElse(qualification, qualificationMap(0))
  }

  private def getStatusPriority(status: Int): Int = {
    statusPriorities.indexOf(status)
  }

  def competition2ProtocolHeader(competition: Competition): ProtocolHeader = {
    WinOrientTableParser.competition2ProtocolHeader(competition)
  }

  def getRule(status: Int): String = {
    statusToRule.getOrElse(status, "")
  }

  private object Statuses {
    val OK = 1
    val DISQUALIFIED = 3
    val MISSING_PUNCH = 4
    val DID_NOT_FINISH = 5
    val DID_NOT_START = 13
    val OVERTIME = 8
    val MISS_PENALTY_LAP = 17
  }

}
