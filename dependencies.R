# LIST OF REQUIRED PACKAGES -----------------------------------------------

required_packages <- c(
  "checkpoint"
)

# install missing packages

new.packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]

if (length(new.packages)) {
  install.packages(new.packages, dependencies = TRUE)
}

rm(new.packages)

library(checkpoint)
checkpoint(snapshotDate = "2021-05-13")

library(effectsize)
library(GA)
library(irr)
library(lpSolve)
library(rel)
library(rstatix)
