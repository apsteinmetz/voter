# 3D spinning plot of voter positions
library(tidyverse)
#devtools::install_github("https://github.com/tylermorganwall/rayshader.git")

library(rayshader)

#install.packages("rgl", repos = "http://R-Forge.R-project.org", type = "source")

load("data/voter_18_scaled.rdata")
view_grid <- voter_18_scaled %>% 
  group_by(caseid) %>% 
  summarise(social = mean(social,na.rm = TRUE),fiscal = mean(fiscal,na.rm = TRUE)) %>%
  na.omit() %>% 
  select(-caseid) %>% 
  # mutate(social = cut(social,5,labels=FALSE)) %>% 
  # mutate(fiscal = cut(fiscal,5,labels=FALSE)) %>% 
  #   mutate(social = round(social,digits=2)) %>% 
  #   mutate(fiscal = round(fiscal,digits=2)) %>% 
  group_by(social,fiscal) %>% 
  tally() %>% 
  {.}


# p <- view_grid %>%  
#   round(digits=1) %>% 
# #  ggplot(aes(social,fiscal,fill=n)) + geom_tile() + 
#   ggplot(aes(social,fiscal)) + 
#   geom_hex(bins = 8,color="black") +   
#   scale_fill_viridis_c(option = "C") +
#   labs(title = "The Nation Is Not Polarized",
#        subtitle = "YouGov 2018 Voter Survey",
#        y = "More Fiscally Conservative --->",
#        x = "More Socially Conservative --->") + 
#   NULL
# 
# p


col_scheme = "D"
p <- view_grid %>% rename(count = n) %>%   
  ggplot() +
  stat_density_2d(aes(x = social, y = fiscal, fill = stat(nlevel)), 
                  geom = "polygon", bins = 10, contour = TRUE) +
  #  facet_wrap(clarity~.) +
  scale_fill_viridis_c(option = col_scheme)  +
  labs(title = "The Voters Are Not Polarized",
       subtitle = "YouGov 2018 Voter Attitude Survey",
       y = "More Fiscally Conservative --->",
       x = "More Socially Conservative --->") + 
  NULL
p

plot_ggg <- function(save_height_matrix = FALSE){
  ret_val <- plot_gg(p, width = 4, height = 3, scale = 150, 
              multicore = TRUE, 
              raytrace = FALSE,
              reduce_size = 0.5,
              offset_edges = TRUE,
              windowsize = c(640,640),
              save_height_matrix = save_height_matrix )
  return(ret_val)
}

render_label_pct <- function(height_matrix,text="Text",x_frac=0.5,y_frac=0.5,elev=500){
  render_label(height_matrix,
               text,
               x = dim(height_matrix)[1]*x_frac,
               y = dim(height_matrix)[2]*y_frac,
               z = elev,
               freetype = FALSE) 
  
}
render_snapshot(clear=TRUE)
plot_ggg(FALSE)
hm <- plot_ggg(TRUE)

render_label_pct(hm,"Republican",0.72,0.72,100)
render_label_pct(hm,"Democrat",0.18,0.18,100)
render_label_pct(hm,"Libertarian",0.18,0.72,100)
render_label_pct(hm,"Populist?",0.72,0.18,100)


render_camera(
  fov = 70, 
  zoom = 0.6, 
  theta = 0, 
  phi = 90)
render_snapshot()


