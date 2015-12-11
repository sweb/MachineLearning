package com.github.sweb.machinelearning

import com.github.sweb.machinelearning.utils.DataFrame

/**
  * Created by f.mueller on 08.12.15.
  */
object ExampleDataUtil extends App {

  val data = DataFrame.readFromCsv("src/test/resources/data.csv")

  val train = data.filter(9, "T")

  val trainFeatures = train.select((0 to 7).toList).toFeatureMatrix
  val trainObservations = train.select(List(8)).toFeatureMatrix.flatten

  val model = LeastSquares(trainFeatures, trainObservations, true)

  val result = model.fittedParameters

  println(result.mkString(","))




}



