main_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(main_dir)

#if (!requireNamespace("devtools", quietly = TRUE))
#install.packages("devtools")
#devtools::install_github("YuLab-SMU/ggmsa")


library(ggmsa)
library(ggplot2)
library(ggplotify)
library(cowplot)

NADH_K_sequences <- 'Alignment/trimmed_MSAs/NADH-quinone_oxidoreductase_subunit_K_trim.fa'

NADH_K <- ggmsa(
  NADH_K_sequences,
  posHighligthed = c(60, 78),
  char_width = 0.5,
  seq_name = T,
  consensus_views = TRUE,
  disagreement = FALSE,
  use_dot = FALSE
) #+
  #geom_seed(seed = "LGFVLNRKNIILMLISIEIMLL", star = TRUE) +
  #geom_seed(seed = "ITFLIL", star = TRUE) +
  #geom_seed(seed = "IAIAGAESAIGLGILVAFY", star = TRUE)

NADH_K_MSA <- as.grob(NADH_K)

ggsave(
  "imgs/NADH_K_MSA_test.png",
  plot = NADH_K_MSA,
  width = 20,
  height = 6,
  dpi = 600
)
