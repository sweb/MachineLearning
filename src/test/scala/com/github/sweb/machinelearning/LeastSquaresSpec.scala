package com.github.sweb.machinelearning

import org.scalatest._
/**
 * Created by Florian on 23.09.2015.
 */
class LeastSquaresSpec extends FlatSpec with Matchers {
  // data from https://en.wikipedia.org/wiki/Simple_linear_regression#Numerical_example
  val data = Array(Array(1.47),
    Array(1.50),
    Array(1.52),
    Array(1.55),
    Array(1.57),
    Array(1.60),
    Array(1.63),
    Array(1.65),
    Array(1.68),
    Array(1.70),
    Array(1.73),
    Array(1.75),
    Array(1.78),
    Array(1.80),
    Array(1.83))
  val obs = Array(52.21, 53.12, 54.48, 55.84, 57.20, 58.57, 59.93, 61.29, 63.11, 64.47, 66.28, 68.10, 69.92, 72.19, 74.46)

  "The least squares algorithm" should "calculate a result vector" in {
    val data = Array(Array(1.0, 1.47),
      Array(1.0, 1.50),
      Array(1.0, 1.52),
      Array(1.0, 1.55),
      Array(1.0, 1.57),
      Array(1.0, 1.60),
      Array(1.0, 1.63),
      Array(1.0, 1.65),
      Array(1.0, 1.68),
      Array(1.0, 1.70),
      Array(1.0, 1.73),
      Array(1.0, 1.75),
      Array(1.0, 1.78),
      Array(1.0, 1.80),
      Array(1.0, 1.83))
    val ls = LeastSquares(data, obs, false)
    val result = ls.fittedParameters
    result(0) should equal (-39.062 +- 0.001)
    result(1) should equal (61.272 +- 0.001)
  }

  it should "also calculate a result vector if the intercept column was not included" in {
    val ls = LeastSquares(data, obs, true)
    val result = ls.fittedParameters
    result(0) should equal (-39.062 +- 0.001)
    result(1) should equal (61.272 +- 0.001)
  }

  it should "calculate the residual sum-of-squares for a given result vector" in {
    val ls = LeastSquares(data, obs, true)
    ls.residualSumOfSquares(Array(-39.062, 61.272)) should equal (7.49056 +- 0.0001)
  }

  it should "add an intercept column to the passed feature matrix" in {
    val data = Array(Array(1.47),
      Array(1.50),
      Array(1.52))
    val obs = Array(52.21, 53.12, 54.48)
    val ls = LeastSquares(data, obs, true)
    val expected = Array(Array(1.0, 1.47),
      Array(1.0, 1.50),
      Array(1.0, 1.52))

    ls.addInterceptColumn(data) should be (expected)
  }

  it should "predict y given a new X" in {
    val ls = LeastSquares(data, obs, true)
    val pred = ls.predict(data)
    val rss = pred.zip(obs).map(p => Math.pow(p._1 - p._2, 2)).sum
    val expected = ls.residualSumOfSquares(ls.fittedParameters)
    rss should equal (expected +- 0.0001)
  }

}
