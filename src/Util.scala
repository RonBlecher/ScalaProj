import scala.annotation.tailrec
import scala.math._

object Util {
    def max[A](lst: List[A], op: (A, A) => Int) : A = {
        lst.reduceLeft((a, b) => {
            if (op(a, b) >= 0)
                a
            else
                b
        })
    }

    def map[A,B,C](lst: List[A], f: A => B, g: B => C) : List[C] = {
        @tailrec
        def inner(l: List[A], accLst: List[C]): List[C] = {
            l match {
                case Nil => accLst
                case head :: tail => inner(tail, accLst :+ g(f(head)))
            }
        }
        inner(lst, Nil)
    }

    def isSorted[A](lst : List[A], op: (A, A) => Boolean) : Boolean = {
        @tailrec
        def inner(l: List[A]): Boolean = {
            l match {
                case Nil => true
                case head :: Nil => true
                case head :: tail =>
                    if (op(head, tail.head))
                        inner(tail)
                    else
                        false
            }
        }
        inner(lst)
    }

    def probs(arr: Array[Double]) : Array[Double] = {
        arr.map(elem => arr.count(x => x == elem).toDouble / arr.length)
    }

    def entropy(arr: Array[Double]) : Double = {
        arr.zip(probs(arr)).distinct.map(p => -p._2 * log10(p._2) / log10(2.0)).sum
    }

    def mean(arr: Array[Double]) : Double = {
        arr.sum / arr.length
    }

    def mu(arr: Array[Double]) : Double = {
        arr.zip(probs(arr)).distinct.map(x => x._1 * x._2).sum
    }

    def variance(arr: Array[Double]) : Double = {
        val m = mu(arr)
        arr.zip(probs(arr)).distinct.map(x => pow(x._1 - m, 2.0) * x._2).sum
    }

    def covariance(xs: Array[Double], ys: Array[Double]) : Double = {
        mu(xs.zip(ys).map(elem => elem._1 * elem._2)) - (mu(xs) * mu(ys))
    }

    def zscore(arr: Array[Double], x: Double) : Double = {
        (x - mu(arr)) / sqrt(variance(arr))
    }

    def maxZScore(arr: Array[Double]) : Double = {
        arr.map(x => abs(zscore(arr, x))).max
    }

    def pearson(xs: Array[Double], ys: Array[Double]) : Double = {
        covariance(xs, ys) / (sqrt(variance(xs)) * sqrt(variance(ys)))
    }

    def dist(p1: Point, p2: Point) : Double = {
        sqrt(sqrDist(p1, p2))
    }

    def sqrDist(p1: Point, p2: Point) : Double = {
        pow(p1.x - p2.x, 2.0) + pow(p1.y - p2.y, 2.0)
    }

    def sqrDistSum(p: Point, pArr: Array[Point]) : Double = {
        pArr.map(x => sqrDist(p, x)).sum
    }

    def getMaxCorrelation(feature: String, compareWith: Vector[String], ts: TimeSeries) : Option[(String, Double)] = {
        var maxCorrelation = (feature, -1.0)
        val curColumn = ts.getValues(feature).get.toArray
        compareWith.foreach(f => {
            val col = ts.getValues(f).get.toArray
            val corr = abs(pearson(curColumn, col))
            if (corr > maxCorrelation._2)
                maxCorrelation = (f, corr)
        })
        if (maxCorrelation._2 != -1.0)
            Some(maxCorrelation)
        else
            None
    }
}
