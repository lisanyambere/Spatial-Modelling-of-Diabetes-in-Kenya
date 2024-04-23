library(ggplot2)
# Plotting
## posterior mean
map1 <- ggplot(data = inla.res)+
  geom_sf(aes(geometry = geometry, fill = BYM_mean))+
  theme_void()+
  labs(title = "Posterior mean Estimates of Diabetes by County")+
       #caption = " Posterior means")+
  theme(plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12))+
  scale_fill_gradient(low = "grey", high = "red")
  #scale_fill_viridis_c()
map1
##posterior median
map2 <- ggplot(data = inla.res)+
          geom_sf(aes(geometry = geometry, fill = BYM_median))+
            theme_void()+
            labs(title = "Posterior Median Estimates of Diabetes by County")+
                 #caption = " Posterior Medians")+
            theme(plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
                  legend.title = element_blank(),
                  plot.caption = element_text(family = "URW Palladio L, Italic",size = 12))+
  scale_fill_gradient(low = "grey", high = "red")
  #scale_fill_viridis_c()
           
map2
## posterior 2.5%
map3 <- ggplot(data = inla.res)+
  geom_sf(aes(geometry = geometry, fill = BYM_2.5))+
  theme_void()+
  labs(title = "Posterior 2.5% Estimates of Diabetes by County")+
      # caption = " Posterior Medians")+
  theme(plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12))+
  scale_fill_gradient(low = "grey", high = "red")
  #scale_fill_viridis_c()
map3
##posterior 97.5%
map4 <- ggplot(data = inla.res)+
  geom_sf(aes(geometry = geometry, fill = BYM_97.5))+
  theme_void()+
  labs(title = "Posterior 97.5% Estimates of Diabetes by County")+
       #caption = " Posterior Medians")+
  theme(plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12))+
  scale_fill_gradient(low = "grey", high = "red")
  #scale_fill_viridis_c()
map4

##grid for the four maps
library(gridExtra)
library(tmap)
grid.arrange(map1, map2, map3, map4, ncol= 2, scales = "free")

tmap_arrange(map1, map2, map3, map4, ncol= 2)
