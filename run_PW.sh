#!/bin/bash

cd PW
# Pre-processing AWS data 
./konversi_AWS.R
# independent predictor Obs Data and Machine Learning
./decompose.R
cd ..
