################################################################################
# Article: Multimorbidity parity progression ratio in Mexico
# Title:   0.0 Packages and functions
# Authors: Daniel,
# Data:    MHAS 2001-2021
################################################################################


# Packages

if (!require(pacman)) {install.packages("pacman")}

pacman::p_load(tidyverse, data.table, foreign, magrittr, crunch, survey,
               patchwork, hrbrthemes, here, ggdark, segmented, readstata13, readxl,
               lmtest, scales, RColorBrewer, ggpubr, purrr, gridExtra, ggpattern,
               openxlsx, haven, ggridges, ggalluvial, DemoDecomp, parallel, tictoc,
               sp, grid, stringr,ggh4x, geofacet, purrr,labelled)

