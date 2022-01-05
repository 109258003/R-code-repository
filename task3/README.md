# work3 : predict protein subcellular localization

## Description
Perform k-fold cross-validation for protein subcellular localization problem.

### cmd
```R
Rscript hw3_studentID.R --fold k --input Archaeal_tfpssm.csv --output performance.csv
```
* Perform *k*-fold cross-validation
* % of training, % of calibration, % of testing= *k*-2, 1, 1

## Input: Archaeal_tfpssm.csv

[Archaeal_tfpssm.csv download](https://drive.google.com/file/d/1L-gv1dPaEonnaASeBtakePT1t3FJwPFI/view?usp=sharing)

This CSV doesn't contain a header. The information of columns as below:

V2: labels of proteins

* CP: Cytoplasmic
* CW: Cell Wall
* EC: Extracellular
* IM: Inner membrane

V3 ~ V5602: the gapped-dipeptide features of each protein

## Model

* Any model you want
* Predict V2 value for each protein

## Output: performance.csv

* accuracy = *P*/*N*, average of *k*-fold cross-validation

set|training|validation|test
---|---|---|---
fold1|0.93|0.91|0.88
fold2|0.92|0.91|0.89
fold3|0.94|0.92|0.90
fold4|0.91|0.89|0.87
fold5|0.90|0.92|0.87
ave.|0.92|0.91|0.88

