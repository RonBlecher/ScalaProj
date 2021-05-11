import scala.collection.mutable
import scala.io.Source

class TimeSeries(csvFileName: String, from: Int, until: Int) {
    val (features: Vector[String], colsMap: mutable.HashMap[String, Vector[Double]]) = {
        val fileSource = Source.fromFile(csvFileName)
        val linesIterator = fileSource.getLines()
        val fieldsName = linesIterator.map(_.split(',').toVector).next()
        val valuesMatrix = linesIterator.map(_.split(',').map(_.toDouble)).toArray
        fileSource.close()

        val hashMap = new mutable.HashMap[String, Vector[Double]]
        var col = 0
        fieldsName.foreach(field => {
            val colValues = valuesMatrix.map(_(col)).toVector
            if (until > 0 && until > from)
                hashMap.put(field, colValues.slice(from, until))
            else
                hashMap.put(field, colValues.slice(from, valuesMatrix.length))
            col += 1
        })
        (fieldsName, hashMap)
    }

    def this(csvFileName: String) = {
        this(csvFileName, 0, 0)
    }

    def split(n: Int) : List[TimeSeries] = {
        var splitList: List[TimeSeries] = List()
        val rowsNum = length()
        if (n <= 0 || n > rowsNum) {
            // splitList already set
        }
        else if (n == 1) {
            splitList = splitList :+ new TimeSeries(csvFileName)
        }
        else {
            val chunkSize = rowsNum / n
            val fullBlocks = n - 1
            val remainder = rowsNum - (chunkSize * fullBlocks)
            var from = 0
            for (i <- 0 until fullBlocks) {
                from = chunkSize * i
                splitList = splitList :+ new TimeSeries(csvFileName, from, from + chunkSize)
            }
            if (remainder != 0) {
                from = chunkSize * fullBlocks
                splitList = splitList :+ new TimeSeries(csvFileName, from, from + remainder)
            }
        }
        splitList
    }

    def length() : Int = {
        colsMap(features(0)).length
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
        val lastIndexRow = length() - 1
        if (features.indexOf(feature) == -1 ||
                range.start < 0 || range.start > lastIndexRow ||
                range.end < 0   || range.end > lastIndexRow)
            None
        else
            Some(range.map(i => colsMap(feature)(i)).toVector)
    }
}
