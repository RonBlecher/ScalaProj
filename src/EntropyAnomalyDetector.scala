object EntropyAnomalyDetector extends ParAnomalyDetector {
    override def map(ts: TimeSeries) : Reports = {
        val reports = new Reports()
        ts.features.foreach(feature => {
            val col = ts.getValues(feature).get.toArray
            val colEntropy = Util.entropy(col)
            var entropies: Vector[(Double, Int)] = Vector()
            for (i <- col.indices) {
                val shortColEntropy = Util.entropy(col.patch(i, Nil, 1))
                entropies = entropies :+ (colEntropy - shortColEntropy, i)
            }
            val maxEntropy = entropies.maxBy(_._1)
            reports += Report(feature, maxEntropy._2, maxEntropy._1)
        })
        reports
    }

    override def reduce(r1: Reports, r2: Reports) : Reports = {
        val reports = new Reports()
        r1.zip(r2).foreach(x => {
            val max = if (x._1.anomalyScore >= x._2.anomalyScore) x._1 else x._2
            reports += max
        })
        reports
    }
}
