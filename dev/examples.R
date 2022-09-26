
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
