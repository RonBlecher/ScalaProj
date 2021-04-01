import scala.math._

class Line(val points: Array[Point]) {
  private val xs: Array[Double] = points.map(p => p.x)
  private val ys: Array[Double] = points.map(p => p.y)
  val a: Double = Util.covariance(xs, ys) / Util.variance(xs)
  val b: Double = Util.mean(ys) - (a * Util.mean(xs))

  def f(x: Double) : Double = {
    (a * x) + b
  }

  def dist(p: Point) : Double = {
    Math.abs(f(p.x) - p.y)
  }
}
