.modelNames <- c("CForestModel", "CoxModel", "CoxStepAICModel", "GBMModel",
                 "GLMNetModel")

lapply(.modelNames, setOldClass)

setClass("CForestFit", contains = "RandomForest")

setOldClass("AbstractControl")
setOldClass("CVControl")
