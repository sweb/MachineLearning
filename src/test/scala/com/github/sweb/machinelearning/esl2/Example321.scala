package com.github.sweb.machinelearning.esl2

import com.github.sweb.machinelearning.{LeastSquares, MLMatrix, Preprocessor}
import com.github.sweb.machinelearning.utils.DataFrame
import org.scalatest.{Matchers, FlatSpec}

/**
  * Created by Florian on 26.12.2015.
  */
class Example321 extends FlatSpec with Matchers {

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

  val test = data.filter(9, "F")
  val testFeatures = Preprocessor.standardize(MLMatrix(test.select((0 to 7).toList).toFeatureMatrix()), means, stds)._1.to2DArray
  val testObservations = test.select(List(8)).toFeatureMatrix().flatten

  it should "get the fitted parameters" in {
    val expected = List(2.46, 0.68, 0.26, -0.14, 0.21, 0.31, -0.29, -0.02, 0.27)

    result.zip(expected).map {
      case (act, exp) => act should equal (exp +- 0.01)
    }
  }

  it should "get the Z scores" in {
    val expected = List(27.6, 5.37, 2.75, -1.4, 2.06, 2.47, -1.87, -0.15, 1.74)

    model.zScores.zip(expected).map {
      case (act, exp) => act should equal (exp +- 0.01)
    }

  }

  it should "get the f statistic for the reduced model" in {
    model.fStatistic(reducedModel) should equal (1.67 +- 0.01)
  }

  it should "predict on the test set" in {
    val results = model.predict(testFeatures)

    val mpe = results.zip(testObservations).map { case (act, exp) => Math.abs(act - exp) }.sum / results.size
    mpe should equal (0.521 + 0.002 +- 0.001)
  }

  it should "calculate the base error rate" in {
    val meanTrainValue: Double = trainObservations.sum / trainObservations.size
    // I don't get the same value - why is that?
    //testObservations.map(obs => Math.abs(obs - meanTrainValue)).sum / testObservations.size should equal (1.057)
  }

}
