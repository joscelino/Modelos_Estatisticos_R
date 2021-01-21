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
checkpoint(snapshotDate = "2021-01-21")

library(benford.analysis)
library(BenfordTests)
library(readr)
library(reshape)
library(corrplot)
library(tibble)
library(nortest)
library(pacman)
library(nortest)
library(tsoutliers)
library(MVN)
library(GGally)
library(HH)
library(emmeans)
library(ez)
library(DescTools)
library(ggplot2)
library(ggiraph)
library(plotly)
library(rstatix)
library(stringr)
library(dplyr)
library(psych)
library(patchwork)
library(pastecs)
library(lawstat)
library(multcomp)