library(reshape)
library(tidyverse)

#-------- visualize evolution index 100---------
evolucion <- read.delim("data/output/demografia/madrid_censo-poblacion-por-barrios_evolucion-2010-2018_base-100-es-2015.csv",sep = ",")

# reshape data to long format to prepare to plot time series timeline
m <- reshape(evolucion, direction = "long", varying = list(names(evolucion)[5:13]), v.names = "Value", 
               idvar = c("nombre_barrio"), timevar = "Year", times = 2010:2018)

# The "almendra central" lines and points are plotted for a second time later to bediplayed on top
ggplot() +
  geom_point(data=m,aes(x = Year, y = Value, color = Ubicación)) + 
  geom_line(data=m,aes(x = Year, y = Value, color = Ubicación, group=nombre_barrio)) + 
  geom_point(data=m[m$Ubicación=="Almendra central",],aes(x = Year, y = Value, color = Ubicación)) + 
  geom_line(data=m[m$Ubicación=="Almendra central",],aes(x = Year, y = Value, color = Ubicación, group=nombre_barrio)) + 
  coord_cartesian(ylim = c(70, 125)) +
  labs(title = "Evolución población por barrio. Madrid.") +
  ylab("100 = año 2010") + xlab("Años") + labs(colour = "Ubicación") +
  theme(axis.text.y = element_text(size=12),
        # axis.title.y=element_blank(),
        axis.ticks.y =element_blank(),
        axis.ticks.x =element_blank(),
        axis.text.x=element_text(size=12),
        axis.title.x=element_text(size=14,face="bold"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="white"),
        panel.grid.major.y = element_line( size=.1, color="grey" ),
        legend.position = "bottom",
        legend.text = element_text(size=15)
  )