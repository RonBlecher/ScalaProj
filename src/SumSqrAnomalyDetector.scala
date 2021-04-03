object SumSqrAnomalyDetector extends AnomalyDetector {
    override def learn(normal: TimeSeries): Map[String, String] = {
        var model: Map[String, String] = Map()
        var compareWith = normal.features
        normal.features.foreach(f => {
            compareWith = compareWith.drop(1)
            val corr = Util.getMaxCorrelation(f, compareWith, normal)
            if (corr.isDefined && corr.get._2 >= 0.9) {
                val col1 = normal.getValues(f).get.toArray
                val col2 = normal.getValues(corr.get._1).get.toArray
                val pArr = col1.zip(col2).map(x => new Point(x._1, x._2))
                val maxSqrSum = pArr.map(p => Util.sqrDistSum(p, pArr)).max
                model += (s"$f,${corr.get._1}" -> maxSqrSum.toString)
            }
        })
        model
    }

    override def detect(model: Map[String, String], test: TimeSeries): Vector[(String, Int)] = {
        var anomalies: Vector[(String, Int)] = Vector()
        model.keys.foreach(corr => {
            val keySplit = corr.split(',')
            val col1 = test.getValues(keySplit(0)).get
            val col2 = test.getValues(keySplit(1)).get
            val pArr = col1.zip(col2).map(x => new Point(x._1, x._2)).toArray
            val maxSqrSum = model(corr).toDouble
            var row = 0
            pArr.foreach(p => {
                if (Util.sqrDistSum(p, pArr) > maxSqrSum)
                    anomalies = anomalies :+ (corr, row)
                row += 1
            })
        })
        anomalies
    }
}
