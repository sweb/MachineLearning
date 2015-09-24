package com.github.sweb.machinelearning

import org.ejml.simple.SimpleMatrix

/**
 * Implementation of least squares, according to chapter 3.2 of ESL II.
 */
case class LeastSquares(featureMatrix: Array[Array[Double]], observations: Array[Double], addIntercept: Boolean) {
  val processedFeatures = preprocessFeatures(featureMatrix)
  val X = new SimpleMatrix(processedFeatures)
  val y = new SimpleMatrix(Array(observations)).transpose()

  lazy val fittedParameters = solve()

  def solve(): Array[Double] = {
    // X has to have full column rank - how to check?
    require(X.getMatrix.getNumRows == y.getMatrix.getNumRows,
      "Number of feature rows does not equal number of observations")

    val T = X.transpose()

    val beta = ( T.mult(X) ).invert().mult(T).mult(y)

    beta.getMatrix().getData()
  }

  def addInterceptColumn(featureMatrix: Array[Array[Double]]): Array[Array[Double]] = {
    featureMatrix.map(row => row.+:(1.0))
  }

  private def preprocessFeatures(featureMatrix: Array[Array[Double]]) = {
    if (addIntercept) addInterceptColumn(featureMatrix) else featureMatrix
  }

  def residualSumOfSquares(parameters: Array[Double]): Double = {
    val beta = new SimpleMatrix(Array(parameters)).transpose()
    val rss = (y.minus( (X.mult(beta)) )).transpose().mult( (y.minus(X.mult(beta))) )
    rss.getMatrix.getData()(0)
  }

  def predict(featureMatrix: Array[Array[Double]]): Array[Double] = {
    val processedFeatures = preprocessFeatures(featureMatrix)
    val X = new SimpleMatrix(processedFeatures)
    val beta = new SimpleMatrix(Array(fittedParameters)).transpose()
    val prediction = X.mult(beta)
    prediction.getMatrix.getData
  }

}