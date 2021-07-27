# load libraries
library(ggplot2)
library(Hmisc)

# get data
sr_table <- read.csv("../data/IPL_sr.csv", header = TRUE, sep = ",", quote = "")

# descriptive statistics
median_lul_sr <- median(sr_table[sr_table$songtype == 'Lullaby', ]$rating)
median_nlul_sr <- median(sr_table[sr_table$songtype == 'Non-lullaby', ]$rating)

# confidence intervals for medians
source("./med_confint_e.R")

# fig : subjective rate violinplots
ylab <- expression(paste("Subjective rating of audio quality"))
title2a <- expression(bold("a"))
figobj <- ggplot(
  data = sr_table,
  aes(
    y = rating,
    x = songtype
  )
) +
  geom_hline(
    yintercept = 5,
    linetype = "dashed",
    alpha = .8,
    size = .5
  ) +
  geom_violin(aes(fill = songtype),
              trim = FALSE,
              alpha = .8
  ) +
  scale_fill_manual(values = c("blue", "red")) +
  geom_line(aes(group = pair_id),
            position = position_jitter(
              width = .025,
              seed = 6012
            ),
            alpha = .1
  ) +
  geom_point(
    aes(y = rating),
    position = position_jitter(
      width = .025,
      seed = 6012
    ),
    size = 1.1,
    pch = 21,
    fill = "white"
  ) +
   stat_summary(
     geom = "crossbar",
     fun.data = med_confint,
     fun.args = list(al = 0.95, verbose = FALSE),
     fill = "white",
     width = 0.8,
     alpha = 0.2,
     size = 0.4
  ) +
  scale_x_discrete(labels = c("Lullaby", "Non-lullaby")) +
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
ggsave("./figure/violinplot_sr.png", plot = figobj, width = 2.8, height = 4)