package ru.qwex.mcspr.parsers

/**
 *
 * @author Aleksander Marenkov <a.marenkov at itgrp.ru>
 */
case class TypedField(
                       fieldName: String,
                       typeName: String,
                       transform: String => String = identity,
                       acceptFilter: String => Boolean,
                     )

object TypedField {

  val sequenceNumber: TypedField = fromTypedColumn("sequenceNumber", TypedColumn.sequenceNumber)
  val number: TypedField = fromTypedColumn("number", TypedColumn.number)
  val fullName: TypedField = fromTypedColumn("fullName", TypedColumn.fullName)
  val team: TypedField = fromTypedColumn("team", TypedColumn.team)
  val sportsCategory: TypedField = fromTypedColumn("sportsCategory", TypedColumn.sportsCategory)
  val yearOfBirth: TypedField = fromTypedColumn(" yearOfBirth",  TypedColumn.yearOfBirth)
  val result: TypedField = fromTypedColumn("result", TypedColumn.result)
  val backlog: TypedField = fromTypedColumn("backlog", TypedColumn.backlog)
  val place: TypedField = fromTypedColumn("place", TypedColumn.place)
  val achieved: TypedField = fromTypedColumn("achieved", TypedColumn.achieved)
  val comment: TypedField = fromTypedColumn("comment", TypedColumn.comment)

  private def fromTypedColumn(fieldName: String, typedColumn: TypedColumn): TypedField = {
    TypedField(
      fieldName = fieldName,
      typeName = typedColumn.typeName,
      transform = typedColumn.transform,
      acceptFilter = typedColumn.acceptFilter,
    )
  }

}
