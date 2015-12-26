package com.github.sweb.machinelearning

/**
  * Created by Florian on 19.12.2015.
  */
object Preprocessor {

  def standardize(featureMatrix: MLMatrix): (MLMatrix, List[Double], List[Double]) = {
    val columnIds = 0 until featureMatrix.numberOfCols
    val columns = columnIds.map(featureMatrix.data.extractVector(false, _).getMatrix.getData).toList
    val sizes = columns.map(_.size)
    val means = columns.map(col => col.sum / col.size)
    val standardDeviations = columns.zip(means).map {
      case (column, mean) => Math.sqrt(column.map(v => Math.pow(v - mean, 2)).sum / (column.size - 1))
    }

    standardize(featureMatrix, means, standardDeviations)

  }

  def standardize(featureMatrix: MLMatrix, means: List[Double], stds: List[Double]): (MLMatrix, List[Double], List[Double]) = {
    val columnIds = 0 until featureMatrix.numberOfCols
    val columns = columnIds.map(featureMatrix.data.extractVector(false, _).getMatrix.getData).toList

    val allInOne = columns.zip(means).zip(stds)
      .map(x => (x._1._1, x._1._2, x._2))
      .map { case (column, mean, std) => {
        if (std == 0.0) {
          column
        } else {
          column.map(x => (x - mean) / std)
        }
      }
      }
    (MLMatrix(allInOne.toArray).transpose, means, stds)
  }

}
