
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

mkFades()
