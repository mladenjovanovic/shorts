
# Optimal profiles
MSS <- 10
MAC <- 8
bodymass <- 75

fv <- make_FV_profile(MSS, MAC, bodymass)

dist <- 1:50

opt_slope_perc <- find_optimal_MSS_MAC(
  distance = dist,
  MSS,
  MAC)[["probe_IMB"]]

opt_dist <- find_optimal_MSS_MAC_distance(MSS, MAC, metric = "probe_IMB")

opt_FV_slope_perc <- find_optimal_FV(
  distance = dist,
  fv$F0_poly,
  fv$V0_poly,
  fv$bodymass)[["probe_IMB"]]

opt_FV_dist <- find_optimal_FV_distance(fv$F0_poly, fv$V0_poly, fv$bodymass, metric = "probe_IMB")

plot(x = dist, y = opt_slope_perc, type = "l")
lines(x = dist, y = opt_FV_slope_perc, type = "l", col = "blue")
points(x = opt_dist, y = 100)
points(x = opt_FV_dist, y = 100, col = "blue")
abline(h = 100, col = "gray", lty = 2)


####
require(tidyverse)

MSS <- 10
MAC <- 8
bodymass <- 75

fv <- make_FV_profile(MSS, MAC, bodymass)

opt_df <- tibble(
  dist = 1:50) %>%
  mutate(
    `Sprint Profile` = find_optimal_MSS_MAC(
      distance = dist,
      MSS,
      MAC)[["slope_perc"]],
    `FV Profile` = find_optimal_FV(
      distance = dist,
      fv$F0_poly,
      fv$V0_poly,
      bodymass)[["FV_slope_perc"]]
  ) %>%
  pivot_longer(-dist, names_to = "profile")

opt_dist <- tibble(
  `Sprint Profile` = find_optimal_MSS_MAC_distance(MSS, MAC),
  `FV Profile` = find_optimal_FV_distance(fv$F0_poly, fv$V0_poly)
) %>%
  pivot_longer(cols = 1:2, names_to = "profile")

ggplot(opt_df, aes(x = dist, y = value, color = profile)) +
  theme_bw() +
  geom_hline(yintercept = 100, linetype = "dashed", alpha = 0.6) +
  geom_line() +
  geom_point(data = opt_dist, aes(x = value, y = 100), size = 2) +
  xlab("Distance (m)") +
  ylab("Profile imalance")
