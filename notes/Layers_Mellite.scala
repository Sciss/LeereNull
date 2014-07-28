val doc = Application.documentHandler.activeDocument.get.asInstanceOf[Workspace.Confluent]

def getLayerCursor(i: Int) = doc.cursors.cursor.step { implicit tx =>
  val name = s"layer$i"
  implicit val dtx = doc.system.durableTx(tx)
  val c1 = doc.cursors.descendants.toList.find(_.name.value == "fanatic").get
  c1.descendants.toList.find(_.name.value == name).get.cursor
}

type S = proc.Confluent

import de.sciss.lucre.expr.{Int => IntEx, Boolean => BooleanEx}
import proc.{BooleanElem, IntElem}
import de.sciss.span.Span
import proc.Obj

val c = getLayerCursor(2)

def yPos(in: proc.Obj[S])(implicit tx: S#Tx): Span = {
  val trIdx = in.attr.expr[Int]("track-index" ).map(_.value).getOrElse(0)
  val trH   = in.attr.expr[Int]("track-height").map(_.value).getOrElse(0)
  Span(trIdx, trIdx + trH)
}

def mkSolo(tMin: Int, tMax: Int): Unit = c.step { implicit tx =>
  val tl = doc.collectObjects {
    case proc.Timeline.Obj(tl) => tl.elem.peer
  } .head

  val bSpan = Span(tMin * 4, (tMax + 1) * 4)

  tl.iterator.toList.foreach {
    case (span @ Span(_, _), rs) =>
      rs.foreach { r =>
        val rp    = r.value
        val aSpan = yPos(rp)
        val muted = !(aSpan overlaps bSpan)
        // rp.attr.put("mute", Obj(BooleanElem(BooleanEx.newVar(BooleanEx.newConst(muted)))))
        if (muted) tl.modifiableOption.foreach(_.remove(r.span, r.value))
      }

    case _ =>
  }
}

mkSolo(tMin = 2, tMax = 3)
