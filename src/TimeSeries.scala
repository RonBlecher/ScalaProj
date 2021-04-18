import scala.collection.mutable
import scala.io.Source

class TimeSeries(csvFileName: String) {
    val (features: Vector[String], colsMap: mutable.HashMap[String, Vector[Double]]) = {
        val fileSource = Source.fromFile(csvFileName)
        val linesIterator = fileSource.getLines()
        val fieldsName = linesIterator.map(_.split(',').toVector).next()
        val valuesMatrix = linesIterator.map(_.split(',').map(_.toDouble)).toArray
        fileSource.close()

        val hashMap = new mutable.HashMap[String, Vector[Double]]
        var col = 0
        fieldsName.foreach(field => {
            hashMap.put(field, valuesMatrix.map(_(col)).toVector)
            col += 1
        })
        (fieldsName, hashMap)
    }

    def getValue(feature: String, timeStep: Int) : Option[Double] = {
        if (features.indexOf(feature) == -1 ||
                timeStep < 0 || timeStep > colsMap(feature).length - 1)
            None
        else
            Some(colsMap(feature)(timeStep))
    }

    def getValues(feature: String) : Option[Vector[Double]] = {
        if (features.indexOf(feature) == -1)
            None
        else
            Some(colsMap(feature))
    }

    def getValues(feature: String, range: Range) : Option[Vector[Double]] = {
        if (features.indexOf(feature) == -1 ||
                range.start < 0 || range.start > colsMap(feature).length - 1 ||
                range.end < 0   || range.end > colsMap(feature).length - 1)
            None
        else
            Some(range.map(i => colsMap(feature)(i)).toVector)
    }
}
