val doc = Application.documentHandler.activeDocument.get.asInstanceOf[Workspace.Confluent]

val c = doc.cursors.cursor.step { implicit tx =>
  implicit val dtx = doc.system.durableTx(tx)
  doc.cursors.descendants.toList.find(_.name.value == "fanatic").get.cursor
}

type S = proc.Confluent

import de.sciss.lucre.expr.{Int => IntEx}
import proc.IntElem
import de.sciss.span.Span
import proc.Obj

def yPos(in: proc.Obj[S])(implicit tx: S#Tx): Span = {
  val trIdx = in.attr.expr[Int]("track-index" ).map(_.value).getOrElse(0)
  val trH   = in.attr.expr[Int]("track-height").map(_.value).getOrElse(0)
  Span(trIdx, trIdx + trH)
}

def mkFanatic(): Unit = c.step { implicit tx =>
  val tl = doc.collectObjects {
    case proc.Timeline.Obj(tl) => tl.elem.peer
  } .head

  tl.iterator.toList.foreach {
    case (span @ Span(_, _), rs) =>
      rs.foreach { r =>
        val rp    = r.value
        // val aSpan = yPos(rp)
        val bs = tl.intersect(span).toList
        val bsy: Set[Span] = bs.flatMap {
          case (Span(_,_), xs) =>
            val xsf = xs.toSet - r
            xsf.map(timed => yPos(timed.value))
          case _               => Set.empty[Span]
        } (collection.breakOut)

        @annotation.tailrec def find(i: Int): Int = {
          val aSpan = Span(i, i + 4)
          if (bsy.find(_ overlaps aSpan).isEmpty) i else find(i + 4)
        }

        val y = find(0)
        rp.attr.put("track-index", Obj(IntElem(IntEx.newVar(IntEx.newConst(y)))))
      }

    case _ =>
  }
}

// run twice
mkFanatic()
mkFanatic()
