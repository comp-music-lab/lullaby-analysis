# Effect size references
#
# ---Hedge's g / Bias-adjusted Hedge's g---
# Grissom, R. J., & Kim, J. J. (2012). Effect sizes for research: Univariate and multivariate applications (2nd ed.). New York, NY: Routledge.
# Hedges, L. V. (1981). Distribution theory for Glass's estimator of effect size and related estimators. Journal of Educational Statistics, 6(2), 107-128.
# Hedges, L. V., & Olkin, I. (1985). Statistical methods for meta-analysis. San Diego, CA: Academic Press.
# Lakens, D. (2013). Calculating and reporting effect sizes to facilitate cumulative science: a practical primer for t-tests and ANOVAs. Front Psychol. 4, 863.
#
# ---normally distributed paired data (unequal variance)---
# Dunlap, W. P., Cortina, J. M., Vaslow, J. B., & Burke, M. J. (1996). Meta-analysis of experiments with matched groups or repeated measures designs. Psychological Methods, 1(2), 170???177.
# Lakens, D. (2013). Calculating and reporting effect sizes to facilitate cumulative science: a practical primer for t-tests and ANOVAs. Front Psychol. 4, 863.

# Clear variables
rm(list = ls())

# load libraries
library(ggplot2)

# get data
T <- read.csv("../data/IPL_sr.csv", header = TRUE, sep = ",", quote = "")

# convert into composite scores - use it for effect size calculation
P <- unique(T$participant)
likscale <- data.frame(participant = character(), aq_l = numeric(), aq_nl = numeric()) 

for (i in 1:length(P)) {
  aq_l_i <- T[T$participant == P[i] & T$songtype == "Lullaby", ]$rating
  aq_nl_i <- T[T$participant == P[i] & T$songtype == "Non-lullaby", ]$rating
  
  if (!(length(aq_l_i) == 8 && length(aq_nl_i) == 8)) {
    print("Data is not properlly prepared!")
    return()
  }
  
  likscale[i, ] <- list(P[i], sum(aq_l_i), sum(aq_nl_i))
}

likscale$aq_l <- likscale$aq_l/length(P)
likscale$aq_nl <- likscale$aq_nl/length(P)

# reshaping for the ease of ggplot
likscale_plot <- rbind(
  data.frame(participant = likscale$participant, rating = likscale$aq_l, songtype = "Lullaby"),
  data.frame(participant = likscale$participant, rating = likscale$aq_nl, songtype = "Non-lullaby")
)

# fig : subjective rate violinplots (composite score version)
ylab <- expression(paste("Subjective rating of audio quality"))
title2a <- expression(bold("a"))

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
            alpha = .1
  ) +
  geom_point(
    aes(y = rating),
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
  xlab("")
  ggtitle(title2a)

# save figure
plot(figobj)
ggsave("./figure/violinplot_sr_agg.png", plot = figobj, width = 2.8, height = 4)

# effect size - Hedge's g
N <- length(likscale$participant)
M_1 <- sum(likscale$aq_l)/N
M_2 <- sum(likscale$aq_nl)/N
var_1 <- sum((likscale$aq_l - M_1)^2)/(N - 1)
var_2 <- sum((likscale$aq_nl - M_2)^2)/(N - 1)
var_p <- ((N - 1)*var_1 + (N - 1)*var_2)/(N + N - 2)
g <- (M_1 - M_2)/sqrt(var_p)

# effect size - bias corrected Hedge's g
al <- N + N - 2
J <- gamma(al/2)/(sqrt(al/2) * gamma((al - 1)/2))
g_bc <- J*g

# effect size - normally distributed paired data (unequal variance)
M_d <- sum(likscale$aq_l - likscale$aq_nl)/N
cov_d <- sum((likscale$aq_l - M_1)*(likscale$aq_nl - M_2))/(N - 1)
var_d <- var_1 + var_2 - 2*cov_d
d_paired <- M_d/sqrt(var_d)

# effect size - normally distributed paired data (unequal variance) with correlation normalization
rho <- sum((likscale$aq_l - M_1)*(likscale$aq_nl - M_2))/sqrt(sum((likscale$aq_l - M_1)^2) * sum((likscale$aq_nl - M_2)^2))
d_rm <- d_paired * sqrt(2*(1 - rho))

# 
cat(sprintf("Hedge's g = %3.3f (%3.3f), unequal variance paired data = %3.3f (%3.3f)\n", g, g_bc, d_paired, d_rm))