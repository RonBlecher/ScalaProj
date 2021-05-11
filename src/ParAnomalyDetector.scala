import scala.collection.mutable
import java.util.concurrent.{ExecutorService, Callable}

case class Report(feature: String, var timeStep: Int, anomalyScore: Double)

trait ParAnomalyDetector {
    type Reports = mutable.ListBuffer[Report]

    def map(ts: TimeSeries) : Reports
    def reduce(r1: Reports, r2: Reports) : Reports

    def detect(ts: TimeSeries, es: ExecutorService, chunks: Int) : Vector[Report] = {
        val tsSplit = ts.split(chunks)
        val tasks = tsSplit.map(series => {
            es.submit(new Callable[Reports] {
                override def call() : Reports = {
                    map(series)
                }
            })
        })
        // wait for all threads
        val taskResults = tasks.map(task => task.get())

        var i = 0
        val chunkSize = tsSplit.head.length()
        taskResults.foreach(reports => {
            reports.foreach(rpt => {
                rpt.timeStep = (i * chunkSize) + rpt.timeStep
            })
            i += 1
        })
        taskResults.reduce((r1, r2) => reduce(r1, r2)).toVector
    }
}
