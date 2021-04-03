object ZAnomalyDetector extends AnomalyDetector {
    override def learn(normal: TimeSeries): Map[String, String] = {
        var model: Map[String, String] = Map()
        normal.features.foreach(f => {
            val column = normal.getValues(f).get.toArray
            val maxZScore = Util.maxZScore(column)
            model += (f -> maxZScore.toString)
        })
        model
    }

    override def detect(model: Map[String, String], test: TimeSeries): Vector[(String, Int)] = {
        var anomalies: Vector[(String, Int)] = Vector()
        test.features.foreach(f => {
            var row = 0
            val column = test.getValues(f).get.toArray
            column.foreach(x => {
                if (math.abs(Util.zscore(column, x)) > model(f).toDouble) {
                    anomalies = anomalies :+ (f, row)
                }
                row += 1
            })
        })
        anomalies
    }
}
