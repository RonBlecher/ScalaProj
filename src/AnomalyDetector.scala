trait AnomalyDetector {
    def learn(normal: TimeSeries) : Map[String, String]
    def detect(model: Map[String, String], test: TimeSeries) : Vector[(String, Int)] // Vector[Tuple2[String, Int]]
}
