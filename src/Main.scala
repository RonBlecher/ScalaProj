object Main {
  def main(args: Array[String]) {
    val l = new Line(Array(new Point(0, 0.1), new Point(1, 2.01), new Point(5.1, 10)))
    println("A=" + l.a + " B=" + l.b) // A=1.94 B=0.085
    println(l.f(4)) // 7.85
    println(l.dist(new Point(4, 8))) // 0.14
  }
}
