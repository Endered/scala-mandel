import scala.util.Random
case class Color(r: Double, g: Double, b: Double) {
  private def clamp(v: Double, low: Int, up: Int): Int =
    Math.min(Math.max(v.toInt, low), up)
  def to256: String =
    s"${clamp(r * 256, 0, 255)} ${clamp(g * 256, 0, 255)} ${clamp(b * 256, 0, 255)}"
}

case class Pos(y: Double, x: Double) {
  def abs: Double = Math.sqrt(y * y + x * x)

  def +(that: Pos): Pos = Pos(x + that.x, y + that.y)
  def -(that: Pos): Pos = Pos(x - that.x, y - that.y)
  def *(that: Pos): Pos = Pos(x * that.x - y * that.y, x * that.y + y * that.x)
  def /(that: Pos): Pos = this * Pos(x / (x * x + y * y), -y / (x * x + y * y))
}

case class Scale(
    fromX: (Double, Double) = (0, 1),
    fromY: (Double, Double) = (0, 1),
    toX: (Double, Double) = (0, 1),
    toY: (Double, Double) = (0, 1)
) {
  require(fromX._1 != fromX._2)
  require(fromY._1 != fromY._2)
  require(toX._1 != toX._2)
  require(toY._1 != toY._2)

  def from(bottom: Double, right: Double): Scale = {
    this.copy(fromY = (0, bottom), fromX = (0, right))
  }
  def from(vertical: (Double, Double), horizontal: (Double, Double)): Scale = {
    this.copy(fromY = vertical, fromX = horizontal)
  }
  def to(bottom: Double, right: Double): Scale = {
    this.copy(toY = (0, bottom), toX = (0, right))
  }
  def to(vertical: (Double, Double), horizontal: (Double, Double)): Scale = {
    this.copy(toY = vertical, toX = horizontal)
  }

  def mulHorizontal(k: Double): Scale = {
    val (fromL, fromR) = fromX
    val fromCenter = (fromL + fromR) / 2
    val (toL, toR) = toX
    val toCenter = (toL + toR) / 2
    this.copy(
      fromX = (
        (fromL - fromCenter) * k + fromCenter,
        (fromR - fromCenter) * k + fromCenter
      ),
      toX = ((toL - toCenter) * k + toCenter, (toR - toCenter) * k + toCenter)
    )
  }

  def mulVertical(k: Double): Scale = {
    val (fromL, fromR) = fromY
    val fromCenter = (fromL + fromR) / 2
    val (toL, toR) = toY
    val toCenter = (toL + toR) / 2
    this.copy(
      fromY = (
        (fromL - fromCenter) * k + fromCenter,
        (fromR - fromCenter) * k + fromCenter
      ),
      toY = ((toL - toCenter) * k + toCenter, (toR - toCenter) * k + toCenter)
    )
  }

  def apply(f: Pos => Color)(p: Pos): Color = f(this(p))
  def apply(p: Pos): Pos = p match {
    case Pos(y, x) =>
      val yp =
        toY._1 + (toY._2 - toY._1) * (y - fromY._1) / (fromY._2 - fromY._1)
      val xp =
        toX._1 + (toX._2 - toX._1) * (x - fromX._1) / (fromX._2 - fromX._1)
      Pos(yp, xp)
  }
}

object Scale {
  def from(bottom: Double, right: Double): Scale = Scale().from(bottom, right)
  def from(vertical: (Double, Double), horizontal: (Double, Double)): Scale =
    Scale().to(vertical, horizontal)
  def to(bottom: Double, right: Double): Scale = Scale().from(bottom, right)
  def to(vertical: (Double, Double), horizontal: (Double, Double)): Scale =
    Scale().to(vertical, horizontal)
}

object Canvas {
  def apply(height: Int, width: Int): Canvas =
    new Canvas(height, width, Seq.fill(width * height)(Color(0, 0, 0)))
  def apply(height: Int, width: Int, board: Seq[Color]): Canvas =
    new Canvas(height, width, board)

  def Scaled(height: Double, width: Double)(f: Pos => Color): Pos => Color = {
    case Pos(y, x) =>
      f(Pos(y / height, x / width))
  }
}

class Canvas(
    val height: Int,
    val width: Int,
    val board: Seq[Color]
) {
  require(width * height == board.length)

  def positions: Seq[Pos] = (for {
    i <- 0 until height;
    j <- 0 until width
  } yield Pos(i, j)).toSeq

  def change(f: Pos => Color): Canvas =
    Canvas(height, width, positions.map(f))

  def map(f: Color => Color): Canvas =
    Canvas(height, width, board.map(f))

  def filter(f: (Pos, Color) => Color): Canvas =
    Canvas(height, width, positions.zip(board).map { case (p, c) => f(p, c) })

  def ppm: String = {
    "P3\n" +
      s"${width} ${height}\n" +
      "256\n" +
      board.map(_.to256).mkString(" ")
  }
}

object Main {

  implicit class AnyIsChain[C](data: C) {
    def chain[D](f: C => D): D = f(data)
  }

  def toWidthHeight(args: Array[String]): Option[(Int, Int)] = args match {
    case Array(height, width)
        if height.toIntOption.isDefined && width.toIntOption.isDefined =>
      Option((height.toInt, width.toInt))
    case _ => None
  }

  def main(args: Array[String]): Unit = {
    val heightWidth = toWidthHeight(args)
    val (height, width) = heightWidth match {
      case Some((h, w)) if 0 < h && 0 < w => (h, w)
      case _ =>
        System.err.println("Error invalid Height and Width")
        sys.exit(1)
    }
    var canvas = Canvas(height, width)

    def mandelTest(n: Int)(pos: Pos): Color = {
      def rec(p: Pos, depth: Int): Color = {
        if (depth <= 0) Color(0, 0, 0)
        else if (p.abs > 1e10) Color(1, 1, 1)
        else rec(p * p + pos, depth - 1)
      }
      rec(Pos(0, 0), n)
    }

    def mandelSearch(start: Scale, n: Int): Scale = {
      val canvas = Canvas(50, 50)

      def next1(range: (Double, Double)): (Double, Double) = range match {
        case (left, right) =>
          if (Random.nextInt(2) == 0) {
            ((left - right) * 0.9 + right, right)
          } else {
            (left, (right - left) * 0.9 + left)
          }
      }

      def next2(
          range1: (Double, Double),
          range2: (Double, Double)
      ): ((Double, Double), (Double, Double)) = {
        (next1(range1), next1(range2))
      }

      def test(scale: Scale): Boolean = {
        val changed = canvas.change(scale(mandelTest(1000)))
        val color = changed.board.count(_.r > 0.5)
        val black = changed.board.length - color
        black < color * 2 && black * 100 > color
      }

      def rec(scale: Scale, rem: Int, failed: Int): Scale = {
        System.err.println(rem)
        if (failed > 10) rec(start.from(50, 50), n, 0)
        else if (rem <= 0) scale
        else {
          val (vertical, horizontal) = next2(scale.toY, scale.toX)
          val next = scale.to(vertical, horizontal)
          if (test(next)) rec(next, rem - 1, 0)
          else rec(scale, rem, failed + 1)
        }
      }
      rec(start.from(50, 50), n, 0)
    }

    def mandelColor(pos: Pos): Color = {
      def color(n: Int): Color = {
        val d = n * 0.03
        val r = (Math.sin(d) + 2) / 3
        val g = (Math.cos(d) + 2) / 3
        val b = (Math.cos(d * d) + 3) / 3
        Color(0.0, g, b)
      }

      def rec(p: Pos, depth: Int): Color = {
        if (depth > 1000) Color(0.0, 0.4, 0.7)
        else if (p.abs > 1e10) color(depth)
        else rec(p * p + pos, depth + 1)
      }
      rec(Pos(0, 0), 0)
    }

    val start =
      Scale.from(height, width).to((-1.0, 1.0), (-1.0, 1.0))
    var finish = mandelSearch(start, 100)
      .mulVertical(1.0 * height / width)
      .from(height, width)

    canvas = canvas.change {
      finish(mandelColor _)
    }

    println(canvas.ppm)
  }
}
