package com.github.sweb.machinelearning

/**
  * Created by Florian on 19.12.2015.
  */
object Preprocessor {

  def standardize(featureMatrix: MLMatrix): MLMatrix = {
    val columns = (0 until featureMatrix.numberOfCols).map(featureMatrix.data.extractVector(false, _).getMatrix.getData).toList
    val sizes = columns.map(_.size)
    val means = columns.map(col => col.sum / col.size)
    val standardDeviations = columns.zip(means)
      .map(tuple => Math.sqrt(tuple._1.map(v => Math.pow(v - tuple._2, 2)).sum / (tuple._1.size - 1)))

    val allInOne = columns.zip(means).zip(standardDeviations)
      .map(x => (x._1._1, x._1._2, x._2))
      .map(triple => triple match {
        case (column, mean, std) => {
          if (std == 0.0) {
            column
          } else {
            val c = column.map(x => (x - mean) / std)
            println(c.mkString(","))
            c
          }
        }
      })
    MLMatrix(allInOne.toArray).transpose
  }

}
