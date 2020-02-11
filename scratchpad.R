library(ggplot2)
library(rayshader)

voter_18_scaled %>% na.omit()

ggdiamonds = ggplot(view_grid) +
  stat_density_2d(aes(x = social, y = fiscal, fill = stat(nlevel)), 
                  geom = "polygon", n = 100, bins = 10, contour = TRUE) +
#  facet_wrap(clarity~.) +
  scale_fill_viridis_c(option = "D")

ggdiamonds

par(mfrow = c(1, 2))

plot_gg(ggdiamonds, width = 5, height = 5, raytrace = FALSE, preview = TRUE)
plot_gg(ggdiamonds, width = 5, height = 5, multicore = TRUE, scale = 250, 
        zoom = 0.7, theta = 10, phi = 30, windowsize = c(800, 800))
Sys.sleep(0.2)
render_snapshot(clear = TRUE)

             