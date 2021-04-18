object HybridAnomalyDetector extends AnomalyDetector {
    override def learn(normal: TimeSeries) : Map[String, String] = {
        var model: Map[String, String] = Map()
        var compareWith = normal.features
        normal.features.foreach(f => {
            compareWith = compareWith.drop(1)
            val col1 = normal.getValues(f).get.toArray
            val corr = Util.getMaxCorrelation(f, compareWith, normal)
            if (corr.isEmpty || corr.get._2 <= 0.5) {
                val maxZScore = Util.maxZScore(col1)
                val corrValue = if (corr.isEmpty) "-1" else corr.get._2.toString
                model += (f -> s"$corrValue,$maxZScore")
            }
            else {
                val col2 = normal.getValues(corr.get._1).get
                val pArr = col1.zip(col2).map(x => new Point(x._1, x._2))
                if (corr.get._2 > 0.5 && corr.get._2 < 0.9) {
                    val minSqrSumPoint = sqrSumMinPoint(pArr)
                    val maxDist = pArr.map(p => Util.dist(minSqrSumPoint, p)).max
                    model += (s"$f,${corr.get._1}" -> s"${corr.get._2},$maxDist")
                }
                else { // corr.get._2 >= 0.9
                    val regLine = new Line(pArr)
                    val maxDist = regLine.maxDist()
                    model += (s"$f,${corr.get._1}" -> s"${corr.get._2},$maxDist,${regLine.a},${regLine.b}")
                }
            }
        })
        model
    }

    override def detect(model: Map[String, String], test: TimeSeries) : Vector[(String, Int)] = {
        var anomalies: Vector[(String, Int)] = Vector()
        model.keys.foreach(corr => {
            val keySplit = corr.split(',')
            val valueSplit = model(corr).split(',')
            val col1 = test.getValues(keySplit(0)).get.toArray
            val corrValue = valueSplit(0).toDouble
            val threshold = valueSplit(1).toDouble
            var row = 0
            if (corrValue <= 0.5) { // no correlation = -1.0
                col1.foreach(x => {
                    if (math.abs(Util.zscore(col1, x)) > threshold) {
                        anomalies = anomalies :+ (corr, row)
                    }
                    row += 1
                })
            }
            else {
                val col2 = test.getValues(keySplit(1)).get
                val pArr = col1.zip(col2).map(x => new Point(x._1, x._2))
                if (corrValue > 0.5 && corrValue < 0.9) {
                    val minSqrSumPoint = sqrSumMinPoint(pArr)
                    pArr.foreach(p => {
                        if (Util.dist(p, minSqrSumPoint) > threshold)
                            anomalies = anomalies :+ (corr, row)
                        row += 1
                    })
                }
                else { // corr.get._2 >= 0.9
                    val a = valueSplit(2).toDouble
                    val b = valueSplit(3).toDouble
                    pArr.foreach(p => {
                        if (Line.dist(a, b, p.x, p.y) > threshold)
                            anomalies = anomalies :+ (corr, row)
                        row += 1
                    })
                }
            }
        })
        anomalies
    }

    private def sqrSumMinPoint(pArr: Array[Point]) : Point = {
        val sqrSumArr = pArr.map(p => Util.sqrDistSum(p, pArr))
        var counter = 0
        var minSqrSum = sqrSumArr(counter)
        var minPoint = pArr(counter)
        sqrSumArr.drop(1).foreach(x => {
            counter += 1
            if (x < minSqrSum) {
                minSqrSum = x
                minPoint = pArr(counter)
            }
        })
        minPoint
    }
}
