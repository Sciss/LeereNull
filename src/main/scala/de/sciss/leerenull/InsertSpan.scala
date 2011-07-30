package de.sciss.leerenull

object InsertSpan {
   sealed trait Action
   case object Ignore extends Action
   case object Move extends Action
   case object Split extends Action
}