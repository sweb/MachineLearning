package com.github.sweb.machinelearning

/**
 * Implementation of least squares, according to chapter 3.2 of ESL II.
 */
case class LeastSquares(featureMatrix: Array[Array[Double]], observations: Array[Double], addIntercept: Boolean) {
  val processedFeatures = preprocessFeatures(featureMatrix)
  val X = MLMatrix(processedFeatures)
  val y = MLVector(observations)

  lazy val fittedParameters = solve()

  def solve(): Array[Double] = {
    // X has to have full column rank - how to check?
    require(X.numberOfRows == y.numberOfItems,
      "Number of feature rows does not equal number of observations")

    val T = X.transpose
    val beta = ( T * X ).invert()* T * y
    beta.getData()
  }

  def addInterceptColumn(featureMatrix: Array[Array[Double]]): Array[Array[Double]] = {
    featureMatrix.map(row => row.+:(1.0))
  }

  private def preprocessFeatures(featureMatrix: Array[Array[Double]]) = {
    if (addIntercept) addInterceptColumn(featureMatrix) else featureMatrix
  }

  def residualSumOfSquares(parameters: Array[Double]): Double = {
    val beta = MLVector(parameters)
    val rss = (y - (X * beta) ).transpose * (y -(X * beta))
    rss(0)
  }

  def predict(featureMatrix: Array[Array[Double]]): Array[Double] = {
    val processedFeatures = preprocessFeatures(featureMatrix)
    val X = MLMatrix(processedFeatures)
    val beta = MLVector(fittedParameters)
    val prediction = X * beta
    prediction.getData
  }

}