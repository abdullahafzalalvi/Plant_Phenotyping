####----
####Project:Plant Phenotyping Image Analysis
# Script 01: Setup
####----------------
# Packages
library(tidyverse)
library(ggplot2)
library(magick)
library(caret)
library(randomForest)
library(fs)
library(EBImage)
library(dplyr)
library(tidyr)
library(gridExtra)
library(viridis)
library(scales)
library(RColorBrewer)
set.seed(123)
getwd()
list.files("scripts")
file.exists("scripts/scripts_01_setup.R")
source("scripts/scripts_01_setup.R")


folders <- c(
  "data/raw",
  "data/processed",
  "scripts",
  "figures",
  "outputs",
  "models"
)

for (f in folders) {
  if (dir.exists(f)) {
    cat("✔ Found:", f, "\n")
  } else {
    cat("✘ Missing:", f, "\n")
  }
}

cat("\nSetup complete.\n")
