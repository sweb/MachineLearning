package com.github.sweb.machinelearning

import com.github.sweb.machinelearning.utils.DataFrame

/**
  * Created by f.mueller on 08.12.15.
  */
object ExampleDataUtil extends App {

  val data = DataFrame.readFromCsv("src/test/resources/data.csv")

  val prepRelevData = data.select((0 to 7).toList).toFeatureMatrix

  val (_, means, stds) = Preprocessor.standardize(MLMatrix(prepRelevData))

  val train = data.filter(9, "T")

  val trainFeatures = Preprocessor.standardize(MLMatrix(train.select((0 to 7).toList).toFeatureMatrix), means, stds)._1.to2DArray
  val trainObservations = train.select(List(8)).toFeatureMatrix.flatten

  val model = LeastSquares(trainFeatures, trainObservations, true)

  val result = model.fittedParameters

  val reducedFeatures = Preprocessor.standardize(MLMatrix(train.select(List(0, 1, 3, 4)).toFeatureMatrix), means, stds)._1.to2DArray
  val reducedModel = LeastSquares(reducedFeatures, trainObservations, true)

  println("-" * 80)
  println(result.mkString(","))
  println("-" * 80)
  println(model.covarianceMatrix.data)
  println("-" * 80)
  println(model.variance)
  println("-" * 80)
  println(model.zScores.mkString(","))
  println("-" * 80)
  println(model.fStatistic(reducedModel))




}



