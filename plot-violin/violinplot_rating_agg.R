# Clear variables
rm(list = ls())

# load libraries
library(ggplot2)
library(lsr)

# get data
T <- read.csv("../data/IPL_sr.csv", header = TRUE, sep = ",", quote = "")

# convert into composite scores
P <- unique(T$participant)
likscale <- data.frame(participant = character(), aq_l = numeric(), aq_nl = numeric()) 
NUM_SONG <- 8

for (i in 1:length(P)) {
  aq_l_i <- T[T$participant == P[i] & T$songtype == "Lullaby", ]$rating
  aq_nl_i <- T[T$participant == P[i] & T$songtype == "Non-lullaby", ]$rating
  
  if (!(length(aq_l_i) == NUM_SONG && length(aq_nl_i) == NUM_SONG)) {
    print("Data is not properlly prepared!")
    return()
  }
  
  likscale[i, ] <- list(P[i], sum(aq_l_i), sum(aq_nl_i))
}

likscale$aq_l <- likscale$aq_l/NUM_SONG
likscale$aq_nl <- likscale$aq_nl/NUM_SONG

# reshaping for the ease of ggplot
likscale_plot <- rbind(
  data.frame(participant = likscale$participant, rating = likscale$aq_l, songtype = "Lullaby"),
  data.frame(participant = likscale$participant, rating = likscale$aq_nl, songtype = "Non-lullaby")
)

# fig : subjective rate violinplots (composite score version)
ylab <- expression(paste("Subjective rating of audio quality"))
title2a <- expression(bold("a"))
pj = position_jitter(width = .025, seed = 4000)

figobj <- ggplot(
  data = likscale_plot,
  aes(
    y = rating,
    x = songtype
  )
) +
  geom_hline(
    yintercept = 4.5,
    linetype = "dashed",
    alpha = .8,
    size = .5
  ) +
  geom_violin(aes(fill = songtype),
              trim = FALSE,
              alpha = .8
  ) +
  scale_fill_manual(values = c("blue", "red")) +
  geom_line(aes(group = participant),
            position = pj,
            alpha = .1
  ) +
  geom_point(
    aes(y = rating),
    position = pj,
    size = 1.1,
    pch = 21,
    fill = "white"
  ) +
  stat_summary(
    geom = "crossbar",
    fun.data = mean_cl_normal,
    fun.args = list(conf.int = 0.95),
    fill = "white",
    width = 0.8,
    alpha = 0.2,
    size = 0.4
  ) +
  scale_x_discrete(labels = c("Lullaby", "Non-lullaby")) +
  scale_y_continuous(breaks = 1:8, limits = c(1, 8), labels = as.character(1:8)) + 
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
ggsave("./figure/violinplot_sr_agg.png", plot = figobj, width = 2.8, height = 4)