#Comparison of audio quality for lullabies vs. non-lullabies in Bainbridge et al. (2021) Nature Human Behavior

library(ggpubr)
library(lsr)

d<-read.csv("../data/AudioQuality.csv")
#d$LullabyQuality<-mean(d$LullabyQualityES,d$LullabyQualityPP)
#d$NonLullabyQuality<-rowMeans(d[,c(18,20)])

#Compare subjective audio quality (1-7 Likert scale - preliminary data from Pat)
ggpaired(d, cond1 = "LullabyQuality", cond2 = "NonLullabyQuality", fill = "condition", palette = "jco")
cohensD( x = d$LullabyQuality, y = d$NonLullabyQuality, method = "paired")
#[1] 0.5400617
t.test(x = d$LullabyQuality, y = d$NonLullabyQuality, method = "paired")
#t = 1.372, df = 13.453, p-value = 0.1925

#Compare automated audio quality (signal-noise ratio)

ggpaired(d, cond1 = "SNRLullaby", cond2 = "SNRNonLullaby", fill = "condition", palette = "jco")
cohensD( x = d$SNRLullaby, y = d$SNRNonLullaby, method = "paired")
#[1] 0.3484813
t.test(x = d$SNRLullaby, y = d$SNRNonLullaby, method = "paired")
#t = 1.0074, df = 11.986, p-value = 0.3336

#Replicate Bainbridge et al. analysis, adding calculation of pre-registered effect size

hr <- read.csv("../data/IPL_hr_clean.csv")

# mean-based analyses
hr_lul_means <- hr %>%
  filter(lultrial == 1) %>%
  group_by(id) %>%
  summarise(mean_lul_hr = mean(zhr_pt, na.rm = TRUE))
hr_lul_descriptives <- t.test(hr_lul_means$mean_lul_hr) %>%
  tidy() %>%
  mutate(sd = sd(hr_lul_means$mean_lul_hr, na.rm = TRUE)) %>%
  mutate(cohen.d = cohensD(hr_lul_means$mean_lul_hr, mu = 0))
hr_nlul_means <- hr %>%
  filter(lultrial == 0) %>%
  group_by(id) %>%
  summarise(mean_nlul_hr = mean(zhr_pt, na.rm = TRUE))
hr_nlul_descriptives <- t.test(hr_nlul_means$mean_nlul_hr) %>%
  tidy() %>%
  mutate(sd = sd(hr_nlul_means$mean_nlul_hr, na.rm = TRUE)) %>%
  mutate(cohen.d = cohensD(hr_nlul_means$mean_nlul_hr, mu = 0))
hr_joined <- inner_join(hr_lul_means, hr_nlul_means)
t_mean_hr <-
  t.test(hr_joined$mean_lul_hr, hr_joined$mean_nlul_hr, paired = TRUE)
t_mean_hr
#t = -2.7499, df = 138, p-value = 0.00676

#Calculate pre-registered effect size:
cohensD( x = hr_joined$mean_lul_hr, y = hr_joined$mean_nlul_hr, method = "paired")

#Including comparison of recording year for completeness, but this is probably not worth showing because it seems more balanced than it really is as some non-lullabies have quite recent recordings. But this obscures the fact that the main differences in recording quality appear in the very earliest year.

ggpaired(d, cond1 = "YearLullaby", cond2 = "YearNonLullaby", fill = "condition", palette = "jco")
cohensD( x = d$YearLullaby, y = d$YearNonLullaby, method = "paired")
#[1] 0.3484813
t.test(x = d$YearLullaby, y = d$SYearonLullaby, method = "paired")
#t = 1.0074, df = 11.986, p-value = 0.3336