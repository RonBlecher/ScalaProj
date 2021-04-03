object LinearRegAnomalyDetector extends AnomalyDetector {
    override def learn(normal: TimeSeries): Map[String, String] = {
        var model: Map[String, String] = Map()
        var compareWith = normal.features
        normal.features.foreach(f => {
            compareWith = compareWith.drop(1)
            val corr = Util.getMaxCorrelation(f, compareWith, normal)
            if (corr.isDefined && corr.get._2 >= 0.9) {
                val col1 = normal.getValues(f).get.toArray
                val col2 = normal.getValues(corr.get._1).get.toArray
                val regLine = new Line(col1.zip(col2).map(x => new Point(x._1, x._2)))
                val maxDist = regLine.maxDist()
                model += (s"$f,${corr.get._1}" -> s"$maxDist,${regLine.a},${regLine.b}")
            }
        })
        model
    }

    override def detect(model: Map[String, String], test: TimeSeries): Vector[(String, Int)] = {
        var anomalies: Vector[(String, Int)] = Vector()
        model.keys.foreach(corr => {
            val keySplit = corr.split(',')
            val valueSplit = model(corr).split(',')
            val col1 = test.getValues(keySplit(0)).get
            val col2 = test.getValues(keySplit(1)).get
            val maxDist = valueSplit(0).toDouble
            val a = valueSplit(1).toDouble
            val b = valueSplit(2).toDouble
            var row = 0
            col1.zip(col2).foreach(p => {
                if (Line.dist(a, b, p._1, p._2) > maxDist)
                    anomalies = anomalies :+ (corr, row)
                row += 1
            })
        })
        anomalies
    }
}
