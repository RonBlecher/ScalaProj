class Line(val points: Array[Point]) {
    val xs: Array[Double] = points.map(p => p.x)
    val ys: Array[Double] = points.map(p => p.y)
    val a: Double = Util.covariance(xs, ys) / Util.variance(xs)
    val b: Double = Util.mean(ys) - (a * Util.mean(xs))

    def f(x: Double) : Double = {
        (a * x) + b
    }

    def dist(p: Point) : Double = {
        Line.dist(a, b, p.x, p.y)
    }

    def maxDist() : Double = {
        points.map(p => dist(p)).max
    }
}

object Line {
    def dist(a: Double, b: Double, x: Double, y: Double) : Double = {
        math.abs(((a * x) + b) - y)
    }
}
