import scala.io.Source

class TimeSeries(csvFileName: String) {
    private val fileSource = Source.fromFile(csvFileName)
    val features: Vector[String] = fileSource.getLines().map(_.split(',').toVector).next()
    val values: Array[Array[Double]] = fileSource.getLines().map(_.split(',').map(_.toDouble)).toArray
    fileSource.close()

    def getValue(feature: String, timeStep: Int) : Option[Double] = {
        val f_col = features.indexOf(feature)
        if (f_col == -1 || timeStep < 0 || timeStep > values.length - 1)
            None
        else
            Some(values(timeStep)(f_col))
    }

    def getValues(feature: String) : Option[Vector[Double]] = {
        val f_col = features.indexOf(feature)
        if (f_col == -1)
            None
        else
            Some(values.map(_(f_col)).toVector)
    }

    def getValues(feature: String, range: Range) : Option[Vector[Double]] = {
        val f_col = features.indexOf(feature)
        if (f_col == -1 ||
            range.start < 0 || range.start > values.length - 1 ||
            range.end < 0   || range.end > values.length - 1)
            None
        else
            Some(range.map(row => values(row)(f_col)).toVector)
    }
}
