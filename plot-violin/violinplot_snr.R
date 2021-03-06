# Clear variables
rm(list = ls())

# load libraries
library(ggplot2)
library(Hmisc)

# get data
sr_table <- read.csv("../data/IPL_snr.csv", header = TRUE, sep = ",", quote = "")

# confidence intervals for medians
source("./med_confint_e.R")

# fig : subjective rate violinplots
ylab <- expression(paste("SNR (dB) by Gaussian noise measurement"))
title2a <- expression(bold("b"))
pj <- position_jitter(width = .025, seed = 6012)

figobj <- ggplot(
  data = sr_table,
  aes(
    y = snr,
    x = songtype
  )
) +
  geom_violin(aes(fill = songtype),
              trim = FALSE,
              alpha = .8
  ) +
  scale_fill_manual(values = c("blue", "red")) +
  geom_line(aes(group = pair_id),
            position = pj,
            alpha = .1
  ) +
  geom_point(
    aes(y = snr),
    position = pj,
    size = 1.1,
    pch = 21,
    fill = "white"
  ) +
  stat_summary(
    geom = "crossbar",
    #fun.data = med_confint,
    #fun.args = list(al = 0.95, verbose = FALSE),
    fun.data = mean_cl_normal,
    fun.args = list(conf.int = 0.95),
    fill = "white",
    width = 0.8,
    alpha = 0.2,
    size = 0.4
  ) +
  scale_x_discrete(labels = c("Lullabies", "Non-lullabies")) +
  theme_bw() +
  theme(
    axis.text = element_text(colour = "black", size = 10),
    axis.title.x = element_text(size = 10, color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "n"
  ) +
  ylab(ylab) +
  xlab("") +
  ggtitle(title2a)

# save figure
plot(figobj)
ggsave("./figure/violinplot_snr.png", plot = figobj, width = 2.8, height = 4)