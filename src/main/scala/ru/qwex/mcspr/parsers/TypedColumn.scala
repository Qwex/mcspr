package ru.qwex.mcspr.parsers

import ru.qwex.mcspr.model.ProtocolItem

/**
 *
 * @author Aleksander Marenkov <a.marenkov at itgrp.ru>
 */
case class TypedColumn(
                        displayName: String,
                        typeName: String,
                        transform: String => String = identity,
                        acceptFilter: String => Boolean,
                      )

object TypedColumn {


  private val sequenceNumberDisplayName = "№п/п"
  private val sequenceNumberTypeName = "sequenceNumber"
  private val numberDisplayName = "Номер"
  private val numberTypeName = "number"
  private val fullNameDisplayName = "Фамилия, имя"
  private val fullNameTypeName = "fullName"
  private val teamDisplayName = "Коллектив"
  private val teamTypeName = "team"
  private val sportsCategoryDisplayName = "Квал"
  private val sportsCategoryTypeName = "sportsCategory"
  private val yearOfBirthDisplayName = "ГР"
  private val yearOfBirthTypeName = "yearOfBirth"
  private val resultDisplayName = "Результат"
  private val resultTypeName = "result"
  private val backlogDisplayName = "Отставание"
  private val backlogTypeName = "backlog"
  private val placeDisplayName = "Место"
  private val placeTypeName = "place"
  private val achievedDisplayName = "Выполнил"
  private val achievedTypeName = "achieved"
  private val commentDisplayName = "Комментарий"
  private val commentTypeName = "comment"

  private def typedColumn(
                           displayName: String,
                           typeName: String,
                           transform: String => String = identity,
                           acceptFilter: String => Boolean = anyAcceptFilter,
                         ) = {
    TypedColumn(displayName, typeName, transform, acceptFilter)
  }


  val sequenceNumber: TypedColumn = typedColumn(
    displayName = sequenceNumberDisplayName,
    typeName = sequenceNumberTypeName,
    acceptFilter = anyAcceptFilter,
  )

  val number: TypedColumn = typedColumn(
    displayName = numberDisplayName,
    typeName = numberTypeName,
  )

  val fullName: TypedColumn = typedColumn(
    displayName = fullNameDisplayName,
    typeName = fullNameTypeName,
  )

  val team: TypedColumn = typedColumn(
    displayName = teamDisplayName,
    typeName = teamTypeName
  )

  val sportsCategory: TypedColumn = typedColumn(
    displayName = sportsCategoryDisplayName,
    typeName = sportsCategoryTypeName,
    acceptFilter = sportCategoryFilter,
  )

  val yearOfBirth: TypedColumn = typedColumn(
    displayName = yearOfBirthDisplayName,
    typeName = yearOfBirthTypeName,
    acceptFilter = yearOfBirthFilter,
  )

  val result: TypedColumn = typedColumn(
    displayName = resultDisplayName,
    typeName = resultTypeName,
    transform = resultTransform,
  )

  val backlog: TypedColumn = typedColumn(
    displayName = backlogDisplayName,
    typeName = backlogTypeName,
    acceptFilter = backlogFilter,
  )

  val place: TypedColumn = typedColumn(
    displayName = placeDisplayName,
    typeName = placeTypeName,
    acceptFilter = placeFilter,
  )

  val achieved: TypedColumn = typedColumn(
    displayName = achievedDisplayName,
    typeName = achievedTypeName,
    acceptFilter = sportCategoryFilter,
  )

  val comment: TypedColumn = typedColumn(
    displayName = commentDisplayName,
    typeName = commentTypeName,
  )

  def any(parsedName: String): TypedColumn = typedColumn(
    displayName = parsedName,
    typeName = "any",
  )

  private val parsedNameToTypedColumn: Map[String, TypedColumn] = {
    Map(
      "№п/п" -> sequenceNumber,
      "Номер" -> number,
      "Фамилия, имя" -> fullName,
      "Субъект РФ Организация" -> team,
      "ГР" -> yearOfBirth,
      "Квал" -> sportsCategory,
      "Результат" -> result,
      "Отстав" -> backlog,
      "Место" -> place,
      "Вып" -> achieved,
    )
  }

  def findTypedColumn(parsedName: String): TypedColumn = {
    parsedNameToTypedColumn.getOrElse(parsedName, any(parsedName))
  }


  private def anyAcceptFilter: String => Boolean = _ => true

  private val knownSportCategory = {
    Set("I", "II", "III", "МСМК", "ЗМС", "Iю", "IIю", "IIIю", "", "МС", "КМС", "Б.Р", "Б/Р").map(_.toUpperCase)
  }

  private def sportCategoryFilter: String => Boolean = { str =>
    knownSportCategory.contains(str.trim.toUpperCase())
  }

  private def yearOfBirthFilter: String => Boolean = { str =>
    str.trim.matches("^[0-9]+$")
  }

  private def backlogFilter: String => Boolean = normalize(_) { str =>
    str.isEmpty || str.matches("^\\+\\d\\d:\\d\\d")
  }

  private def placeFilter: String => Boolean = normalize(_) { str =>
    str.isEmpty || ProtocolItem.isOutOfCompetitionPLaces(str.toLowerCase) || str.matches("^=?\\s*[0-9]+$")
  }


  private def normalize[T](str: String)(transform: String => T): T = {
    transform(str.trim.split("\\n").mkString(" "))
  }


  private val successResultRegex: String = "\\d\\d:\\d\\d:\\d\\d"

  private def resultTransform: String => String =  normalize(_) { str =>
    if (str.matches(successResultRegex)) {
      str
    } else {
      s"п.п.$str"
    }
  }

}
