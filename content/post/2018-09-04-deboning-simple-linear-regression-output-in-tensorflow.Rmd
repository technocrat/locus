---
title: Deboning simple linear regression output in TensorFlow
author: Richard Careaga
date: '2018-09-04'
slug: deboning-simple-linear-regression-output-in-tensorflow
categories:
  - Data Science
  - Programming
tags:
  - debugging
  - statistics
---
A simple linear regression example in TensorFlow(TM) is given at (https://www.tensorflow.org/tutorials/keras/basic_regression). By default, it produces *none* of the outputs of linear regression, other than MAE or MSE, on request through a print statement.

    Testing set Mean Abs Error: $2788.86

Time spent searching the documentation turned up no references to "intercept", "coefficient", "F-statistic", "t-value," or "p-value."

I am only beginning to dip my toes into TensorFlow(TM). I suspect that it exists not so much as a suite of modeling tools so much as a platform to express them in n-dimensional space very efficiently. Like the output of the example itself, some users seem concerned only with the spread between the model prediction and results when testing (https://web.stanford.edu/class/cs20si/2017/lectures/notes_03.pdf).

There must be a way to use stats packages in Python to specify a model, pass it to TF, have it processed, and retrieve the full diagnostics. Otherwise, it seems too much like *trust me, would I ... .*
