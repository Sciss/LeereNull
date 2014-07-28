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

import proc.FadeSpec
import proc.Timeline

def mkFades(): Unit = c.step { implicit tx =>
  val tl = doc.collectObjects {
    case proc.Timeline.Obj(tl) => tl.elem.peer
  } .head

  val len = (Timeline.SampleRate * 0.004).toLong

  tl.iterator.toList.foreach {
    case (span @ Span(_, _), rs) =>
      rs.foreach { r =>
        val rp    = r.value

        def test(key: String) = {
          val n = rp.attr.expr[FadeSpec](key).map(_.value.numFrames).getOrElse(0L)
          if (n < len) {
            val fd = FadeSpec(len)
            rp.attr.put(key, Obj(FadeSpec.Elem(FadeSpec.Expr.newVar(FadeSpec.Expr.newConst(fd)))))
          }
        }

        test("fade-in")
        test("fade-out")
      }

    case _ =>
  }
}

import proc.Proc
import proc.Implicits._
import proc.SynthGraphs
import proc.ExprImplicits

def fixBuses(): Unit = c.step { implicit tx =>
  val tl = doc.collectObjects {
    case proc.Timeline.Obj(tl) => tl.elem.peer
  } .head

  val glob: List[Proc.Obj[S]] = tl.iterator.toList.flatMap {
    case (Span.All, rs) => rs.map(_.value).collect { case Proc.Obj(p) => p }
    case _ => Vector.empty
  }

  glob.zipWithIndex.foreach { case (p, i) =>
    val d = Proc[S]
    val pp: Proc[S] = p.elem.peer
    val name = p.attr.name
    val numCh = if (name.startsWith("1") || name.startsWith("$1")) 1 else 1
    val g = SynthGraph {
      import de.sciss.synth._
      import ugen._
      import de.sciss.synth.proc.graph._
      val sig = DC.ar(Seq.fill(numCh)(0))
      scan.Out("out", sig)
    }
    d.graph() = SynthGraphs.newConst(g)
    val obj = Obj(Proc.Elem(d))
    obj.attr.put("track-index" , Obj(IntElem(IntEx.newVar(IntEx.newConst(i)))))
    obj.attr.put("track-height", Obj(IntElem(IntEx.newVar(IntEx.newConst(1)))))
    val scanOut = d.scans.add("out")
    val scanIn  = pp.scans.get("in").get
    scanOut.addSink(scanIn)
    scanIn.addSource(scanOut)
    val imp = ExprImplicits[S]
    import imp._
    tl.modifiableOption.foreach { tlm => tlm.add(Span.All, obj) }
  }
}
