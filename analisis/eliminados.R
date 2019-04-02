# Este script analiza las diferencias entre los diferentes listings de una localización de InsideAirbnb
# Usa los archivos listings-summary de Insideairbnb 

# Load libraries ----
library(gsubfn)
library(tidyverse)
# extends color paletter
library(RColorBrewer)
library("reshape2")
library(ggthemes) #install ggthemes

# ------ Get dates when data are --------
# Loads dates with listings data
dates <- c("2019-03-08","2019-02-06","2019-01-14","2018-12-10","2018-11-07","2018-10-10","2018-09-11","2018-08-14","2018-07-10","2018-05-14","2018-04-12",
           "2018-01-17","2017-04-08","2017-03-06","2015-10-02","2015-09-04","2015-07-17")
dates <- rev(dates)

# Colors
# extends color paletter
getPalette <- colorRampPalette(brewer.pal(9, "Set1"))

# loop starts
# listings  <- select(as.data.frame(read.delim("data/original/airbnb/2015-07-17/visualizations/listings.csv",sep = ",")),
#                     id,room_type,calculated_host_listings_count,neighbourhood,neighbourhood_group,number_of_reviews,availability_365)
listings  <- select(as.data.frame(read.delim("data/original/airbnb/2019-03-08/visualizations/listings.csv",sep = ",")),
                    id,room_type,calculated_host_listings_count,neighbourhood,neighbourhood_group,number_of_reviews,availability_365)


# names(listings) <- "id"
listings.total <- listings
# saves for later in other df
listings.total.all <- listings.total 

# Set up style theme for ggplot
# theme_our <- function{ theme(theme_minimal(base_family = "Roboto Condensed")) }

# Loop para ir insertando todos los listings en un dataframe
# El objetivo es tener una lista con todos los id de los anuncios
for (i in 1:length(dates)) {
  print("listings totales: ")
  print(nrow(listings.total))
  print("------")
  print(paste("id:",i))
  print(paste("fecha: ",dates[i]))
  # Adds rows to the original
  neo <- select(as.data.frame(read.delim(paste("data/original/airbnb/",dates[i],"/visualizations/listings.csv",sep=""),sep = ",")),
                id,room_type,calculated_host_listings_count,neighbourhood,neighbourhood_group,number_of_reviews,availability_365)
  # names(neo) <- "id"
  print(paste("neo a leer",nrow(neo)))
  # TODO. Solamente inserta los que no estaban ya. Si el calculated_host_listings_count ha camiado (el usuario ahora tiene más anuncios) no lo actualizaría
  # Se podría guardar el host_id y luego mirar si ha tenido más de una anuncio en algún momento, o hacerlo inversamente, de los actuales a los antiguos
  neo2 <- select(as.data.frame(neo[!neo$id %in% listings.total$id,]),
                 id,room_type,calculated_host_listings_count,neighbourhood,neighbourhood_group,number_of_reviews,availability_365)
  # names(neo2) <- "id"
  print(paste("neo2 a insertar",nrow(neo2)))
  # añade los anuncios al listado original
  listings.total <- rbind(listings.total,neo2)
}

# counts whether a listing exists in a scraping
# each row is one listings. each column is one date when scraping was made by insideairbnb
for (i in 1:length(dates)) {
  print(i)
  # Adds rows to the original
  neo <- select(as.data.frame(read.delim(paste("data/original/airbnb/",dates[i],"/visualizations/listings.csv",sep=""),sep = ",")),
                id,room_type,calculated_host_listings_count,neighbourhood,neighbourhood_group)
  # names(neo) <- "id"
  print(paste("n listings:",nrow(neo)))
  # sets to 0 for all
  listings.total[,paste("d",dates[i],sep="")] <- 0
  # sets to 1 if listing is in that date
  listings.total[listings.total$id %in% neo$id,paste("d",dates[i],sep="")] <- 1
}

# Translate room type values
levels(listings.total$room_type) <- c("Piso completo","Habitación","Habitación compartida")

# converts to long format ------------------------------------------------------------------------------
data_long <- listings.total %>% gather(fecha, exists, 8:24) #starts in 5th column after other variables
# parse date
data_long$fechab <- as.Date( strapplyc( as.character(data_long$fecha), "d(.*)", simplify = TRUE), "%Y-%m-%d")

# classify by type of host ----------------------------------------------------------------------------
data_long$host.type <- ""
data_long[data_long$calculated_host_listings_count == 1,]$host.type <- "1 anuncio"
data_long[data_long$calculated_host_listings_count > 1,]$host.type <- "varios anuncios"

# data_long$host.type.m <- ""
# data_long[data_long$calculated_host_listings_count == 1,]$host.type.m <- "1 anuncio"
# data_long[data_long$calculated_host_listings_count == 2,]$host.type.m <- "2 anuncios"
# data_long[data_long$calculated_host_listings_count == 3,]$host.type.m <- "3 anuncios"
# data_long[data_long$calculated_host_listings_count == 4,]$host.type.m <- "4 anuncios"
# data_long[data_long$calculated_host_listings_count > 4,]$host.type.m <- "5 o más anuncios"

data_long$host.type.m <- ""
data_long[data_long$calculated_host_listings_count == 1,]$host.type.m <- "1 anuncio"
data_long[data_long$calculated_host_listings_count == 2,]$host.type.m <- "2 anuncios"
data_long[data_long$calculated_host_listings_count > 2 & data_long$calculated_host_listings_count < 6,]$host.type.m <- "3-5 anuncios"
data_long[data_long$calculated_host_listings_count > 5 & data_long$calculated_host_listings_count < 15,]$host.type.m <- "6-14 anuncios"
data_long[data_long$calculated_host_listings_count > 14,]$host.type.m <- "15 o más anuncios"

# review type -------------------------
data_long$reviews.type <- ""
data_long[data_long$number_of_reviews == 0,]$reviews.type <- "ninguna review"
data_long[data_long$number_of_reviews > 0,]$reviews.type <- "1 o más reviews"
# data_long[data_long$number_of_reviews ==1,]$reviews.type <- "1"
# data_long[data_long$number_of_reviews > 1,]$reviews.type <- "2 o más"

# availability type -------------------------
data_long$availability.type <- ""
data_long[data_long$availability_365 == 0,]$availability.type <- "no"
data_long[data_long$availability_365 > 0 & data_long$availability_365 <31,]$availability.type <- "0-30 días"
data_long[data_long$availability_365 > 30 & data_long$availability_365 <91,]$availability.type <- "31-90 días"
data_long[data_long$availability_365 > 90 & data_long$availability_365 <181,]$availability.type <- "91-181 días"
data_long[data_long$availability_365 > 180,]$availability.type <- "91-181 días"

data_long$availability.type.s <- ""
data_long[data_long$availability_365 == 0,]$availability.type.s <- "no"
data_long[data_long$availability_365 > 0 & data_long$availability_365 > 0,]$availability.type.s <- "con alguna disponibilidad"


# counts listings per scraping date ----------------------------------------------------------------------------
dates.count <- data_long %>% filter (exists ==1) %>% group_by(fechab) %>% summarise(anuncios=n())
df <- data.frame(x1 = 2.62, x2 = 3.57, y1 = 21.0, y2 = 15.0)
png(filename=paste("images/airbnb/evolucion/anuncios-madrid-por-mes-linea.png", sep = ""),width = 1000,height = 400)
dates.count  %>%
  ggplot(aes(fechab,anuncios)) + 
  geom_line(size=1.5) +
  # geom_line(aes(fechab,anuncios))
  geom_point(size=2.5,color="#BB3300") +
  geom_text(data=filter(dates.count,fechab > as.Date("2019-03-01")),
            aes(fechab+10,anuncios,label=format(anuncios, nsmall=1, big.mark=".")), size=4,hjust=0) +
  # annotate("text",x=as.Date("2018-04-20"),y=4000,label="acuerdo",color="#000000",size=5) +
  # geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  scale_y_continuous(limits=c(0, max(dates.count$anuncios)),labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  # geom_text(aes(label=anuncios)) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank()
    # legend.position = "bottom"
  ) +
  labs(title = "Número de anuncios de Airbnb en cada descarga de datos de InsideAirbnb",
       subtitle = "Madrid 2015-2018",
       y = "número de anuncios",
       x = "",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb") +
  # nota
  annotate(geom = "text", x = as.Date("2017-01-1"), y = 2000, label = "Cada punto es un scraping de InsideAirbnb", 
           family = "Roboto Condensed", hjust = 1,size=6) +
  # annotate(geom = "segment", x = as.Date("2017-01-1"), xend = as.Date("2017-04-1"), y = 12000, yend = 17200,
           # color="#999999") +
  geom_curve(aes(x = as.Date("2017-01-01"), y = 2000, xend = as.Date("2017-04-08"), yend = 12900), 
             color="#999999", data =df,  curvature = 0.2)
dev.off()

# filter by number of reviews ---------------------------------
dates.count.active <- data_long %>% filter (exists ==1) %>% group_by(fechab,reviews.type) %>% summarise(anuncios=n())

png(filename=paste("images/airbnb/evolucion/anuncios-madrid-reviews-por-mes-linea-multi.png", sep = ""),width = 1000,height = 400)
ggplot(NULL) + 
  geom_line(data=dates.count.active,
            aes(fechab,anuncios,color=reviews.type),size=1.5) +
  geom_point(data=dates.count.active,
             aes(fechab,anuncios,color=reviews.type),size=2.5) +
  geom_line(data=dates.count,
            aes(fechab,anuncios),size=1.5) +
  scale_y_continuous(limits=c(0, max(dates.count$anuncios)),labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  geom_text(data=filter(dates.count,fechab > as.Date("2019-03-01")),
            aes(fechab+10,anuncios,label=format(anuncios, nsmall=1, big.mark=".")), size=4,hjust=0) +
  geom_text(data=filter(dates.count.active,fechab > as.Date("2019-03-01")),
            aes(fechab+10,anuncios,label=format(anuncios, nsmall=1, big.mark=".")), size=4,hjust=0) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "top"
  ) +
  labs(title = "Número de anuncios de Airbnb en cada descarga de datos de InsideAirbnb",
       subtitle = "Madrid 2015-2018",
       y = "número de anuncios",
       x = "",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb",
       color = "Según nº reviews" ) 
dev.off()

# filter by number of reviews and availability ---------------------------------
dates.count.reviews.availablity <- data_long %>% filter (exists ==1) %>% group_by(fechab,reviews.type,availability.type) %>% summarise(anuncios=n())

png(filename=paste("images/airbnb/evolucion/anuncios-madrid-reviews-por-mes-linea-multi.png", sep = ""),width = 1000,height = 400)
ggplot(NULL) + 
  geom_line(data=dates.count.reviews.availablity,
            aes(fechab,anuncios,color=availability.type),size=1.5) +
  scale_color_brewer(palette="PuBu") +
  # geom_point(data=dates.count.reviews.availablity,
  #            aes(fechab,anuncios,color=reviews.type),size=2.5) +
  # geom_line(data=dates.count,
  #           aes(fechab,anuncios),size=1.5) +
  scale_y_continuous(limits=c(0, max(dates.count$anuncios)),labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  # geom_text(data=filter(dates.count,fechab > as.Date("2019-03-01")),
  #           aes(fechab+10,anuncios,label=format(anuncios, nsmall=1, big.mark=".")), size=4,hjust=0) +
  # geom_text(data=filter(dates.count.active,fechab > as.Date("2019-03-01")),
            # aes(fechab+10,anuncios,label=format(anuncios, nsmall=1, big.mark=".")), size=4,hjust=0) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    # panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "top"
  ) +
  labs(title = "Número de anuncios de Airbnb en cada descarga de datos de InsideAirbnb",
       subtitle = "Madrid 2015-2018",
       y = "número de anuncios",
       x = "",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb",
       color = "Según días disponbles" ) +
  facet_wrap(~reviews.type)
dev.off()

# counts listings por barrio --------------------------------------------------------------------
dates.count.barrio <- data_long %>% filter (exists ==1) %>% group_by(fechab,neighbourhood) %>% summarise(anuncios=n())

png(filename=paste("images/airbnb/evolucion/anuncios-por-mes-barrio.png", sep = ""),width = 1000,height = 900)
dates.count.barrio %>% 
  ggplot () +
  # annotate("text",x=as.Date("2018-05-25"),y=5000,label="acuerdo",color="#000000",
  #          size=5,family = "Roboto Condensed",hjust=1) +
  geom_point(aes(fechab,anuncios,group=neighbourhood,color=neighbourhood),size=1) +
  geom_line(aes(fechab,anuncios,group=neighbourhood,color=neighbourhood),size=1) +
  # geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  # barrios labels
  geom_text(data=filter(dates.count.barrio,fechab==as.Date("2019-03-08"),anuncios>400), 
            aes(fechab+5,anuncios,label=paste(anuncios,neighbourhood)),
            size=4,
            hjust=0,
            family = "Roboto Condensed") +
  ylim(0, max(dates.count.barrio$anuncios)) +
  xlim(as.Date(min(dates.count.barrio$fechab)),as.Date("2020-06-4")) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "none"
  ) +
  labs(title = "Número de anuncios de Airbnb por barrio en Madrid",
       subtitle = "2015-2018 (publicados en cada scraping de InsideAirbnb)",
       y = "número de anuncios",
       x = "fecha",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb")
dev.off()

# counts listings por barrio y room type--------------------------------------------------------------

dates.count.barrio.room <- data_long %>% filter (exists ==1) %>% group_by(fechab,neighbourhood,room_type) %>% summarise(anuncios=n())

png(filename=paste("images/airbnb/evolucion/anuncios-por-mes-barrio-room.png", sep = ""),width = 1000,height = 900)
dates.count.barrio.room %>% filter(!room_type=="Habitación compartida") %>%
  ggplot () +
  # annotate("text",x=as.Date("2018-05-25"),y=5000,label="acuerdo",color="#000000",
  #          size=5,family = "Roboto Condensed",hjust=1) +
  geom_point(aes(fechab,anuncios,group=neighbourhood,color=neighbourhood),size=1) +
  geom_line(aes(fechab,anuncios,group=neighbourhood,color=neighbourhood),size=1) +
  scale_color_manual(values = getPalette(128)) +
  # geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  # barrios labels
  geom_text(data=filter(dates.count.barrio.room,fechab==as.Date("2019-03-08"),!room_type=="Habitación compartida",anuncios>400), 
            aes(fechab+7,anuncios,label=paste(anuncios,neighbourhood)),
            size=4,
            hjust=0,
            family = "Roboto Condensed") +
  ylim(0, max(dates.count.barrio.room$anuncios)) +
  xlim(as.Date(min(dates.count.barrio.room$fechab)),as.Date("2020-06-4")) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.y = element_blank(),
    legend.position = "none"
  ) +
  labs(title = "Número de anuncios de Airbnb por barrio y tipo de alojamiento en Madrid",
       subtitle = "2015-2018 (publicados en cada scraping de InsideAirbnb)",
       y = "número de anuncios",
       x = "fecha",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb") +
  facet_wrap(~room_type)
dev.off()

# counts listings por barrio y host type--------------------------------------------------------------
dates.count.barrio.host <- data_long %>% filter (exists ==1) %>% group_by(fechab,neighbourhood,host.type) %>% summarise(anuncios=n())

png(filename=paste("images/airbnb/evolucion/anuncios-por-mes-barrio-host.png", sep = ""),width = 1000,height = 600)
dates.count.barrio.host %>% 
  ggplot () +
  # annotate("text",x=as.Date("2018-05-25"),y=5000,label="acuerdo",color="#000000",
  #          size=5,family = "Roboto Condensed",hjust=1) +
  geom_point(aes(fechab,anuncios,group=neighbourhood,color=neighbourhood),size=1.5) +
  geom_line(aes(fechab,anuncios,group=neighbourhood,color=neighbourhood),size=1.5) +
  # geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  # barrios labels
  geom_text(data=filter(dates.count.barrio.host,fechab==as.Date("2018-09-11"),anuncios>400),
            aes(fechab+7,anuncios,label=paste(anuncios,neighbourhood)),
            size=4,
            hjust=0,
            family = "Roboto Condensed") +
  ylim(0, max(dates.count.barrio.host$anuncios)) +
  xlim(as.Date(min(dates.count.barrio.host$fechab)),as.Date("2020-06-4")) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "none"
  ) +
  labs(title = "Número de anuncios de Airbnb por barrio y tipo de host en Madrid",
       subtitle = "2015-2018 (publicados en cada scraping de InsideAirbnb)",
       y = "número de anuncios",
       x = "fecha",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb") +
  facet_wrap(~host.type)
dev.off()


# counts listings por distrito --------------------------------------------------------------------
dates.count.distrito <- data_long %>% filter (exists ==1) %>% group_by(fechab,neighbourhood_group) %>% summarise(anuncios=n())

png(filename=paste("images/airbnb/evolucion/anuncios-por-mes-distrito.png", sep = ""),width = 1000,height = 600)
dates.count.distrito %>% 
  ggplot () +
  # annotate("text",x=as.Date("2018-05-25"),y=5000,label="acuerdo",color="#000000",
  #          size=5,family = "Roboto Condensed",hjust=1) +
  geom_point(aes(fechab,anuncios,group=neighbourhood_group,color=neighbourhood_group),size=1.5) +
  geom_line(aes(fechab,anuncios,group=neighbourhood_group,color=neighbourhood_group),size=1.5) +
  # geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  # distritos labels
  geom_text(data=filter(dates.count.distrito,fechab==as.Date("2018-09-11")), 
            aes(fechab+5,anuncios,label=neighbourhood_group),
            size=4,
            hjust=0,
            family = "Roboto Condensed") +
  ylim(0, max(dates.count.distrito$anuncios)) +
  xlim(as.Date(min(dates.count.distrito$fechab)),as.Date("2020-06-4")) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "none"
  ) +
  labs(title = "Número de anuncios de Airbnb por distrito en Madrid",
       subtitle = "2015-2018 (publicados en cada scraping de InsideAirbnb)",
       y = "número de anuncios",
       x = "fecha",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb")
dev.off()

# counts listings por distrito y room type--------------------------------------------------------------
dates.count.distrito.room <- data_long %>% filter (exists ==1) %>% group_by(fechab,neighbourhood_group,room_type) %>% summarise(anuncios=n())

png(filename=paste("images/airbnb/evolucion/anuncios-por-mes-distrito-room.png", sep = ""),width = 1000,height = 600)
dates.count.distrito.room %>% filter(!room_type=="Habitación compartida") %>%
  ggplot () +
  # annotate("text",x=as.Date("2018-05-25"),y=5000,label="acuerdo",color="#000000",
  #          size=5,family = "Roboto Condensed",hjust=1) +
  geom_point(aes(fechab,anuncios,group=neighbourhood_group,color=neighbourhood_group),size=1.5) +
  geom_line(aes(fechab,anuncios,group=neighbourhood_group,color=neighbourhood_group),size=1.5) +
  # geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  # distritos labels
  geom_text(data=filter(dates.count.distrito.room,fechab==as.Date("2018-09-11"),!room_type=="Habitación compartida"), 
            aes(fechab+7,anuncios,label=neighbourhood_group),
            size=4,
            hjust=0,
            family = "Roboto Condensed") +
  ylim(0, max(dates.count.distrito.room$anuncios)) +
  xlim(as.Date(min(dates.count.distrito.room$fechab)),as.Date("2020-06-4")) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "none"
  ) +
  labs(title = "Número de anuncios de Airbnb por distrito y tipo de alojamiento en Madrid",
       subtitle = "2015-2018 (publicados en cada scraping de InsideAirbnb)",
       y = "número de anuncios",
       x = "fecha",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb") +
  facet_wrap(~room_type)
dev.off()

# counts listings por distrito y host type--------------------------------------------------------------
dates.count.distrito.host <- data_long %>% filter (exists ==1) %>% group_by(fechab,neighbourhood_group,host.type) %>% summarise(anuncios=n())

png(filename=paste("images/airbnb/evolucion/anuncios-por-mes-distrito-host.png", sep = ""),width = 1000,height = 600)
dates.count.distrito.host %>% 
  ggplot () +
  # annotate("text",x=as.Date("2018-05-25"),y=5000,label="acuerdo",color="#000000",
           # size=5,family = "Roboto Condensed",hjust=1) +
  geom_point(aes(fechab,anuncios,group=neighbourhood_group,color=neighbourhood_group),size=1.5) +
  geom_line(aes(fechab,anuncios,group=neighbourhood_group,color=neighbourhood_group),size=1.5) +
  # geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  # distritos labels
  geom_text(data=filter(dates.count.distrito.host,fechab==as.Date("2018-09-11")), 
            aes(fechab+7,anuncios,label=neighbourhood_group),
            size=4,
            hjust=0,
            family = "Roboto Condensed") +
  ylim(0, max(dates.count.distrito.host$anuncios)) +
  xlim(as.Date(min(dates.count.distrito.host$fechab)),as.Date("2020-06-4")) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "none"
  ) +
  labs(title = "Número de anuncios de Airbnb por distrito y tipo de host en Madrid",
       subtitle = "2015-2018 (publicados en cada scraping de InsideAirbnb)",
       y = "número de anuncios",
       x = "fecha",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb") +
  facet_wrap(~host.type)
dev.off()

# counts listings per scraping date and room type --------------------------------------------------------------------
dates.count.room_type <- data_long %>% filter (exists ==1) %>% group_by(fechab,room_type) %>% summarise(anuncios=n())

# todos los anuncios
png(filename=paste("images/airbnb/evolucion/anuncios-por-mes.png", sep = ""),width = 1000,height = 200)
dates.count.room_type %>%
  ggplot(aes(fechab,anuncios)) + 
    geom_col() +
    # annotate("text",x=as.Date("2018-05-15"),y=21000,label="acuerdo",color="#000000",size=4) +
    # geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
    # geom_text(aes(label=anuncios)) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 14) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank()
    # legend.position = "bottom"
  ) +
  labs(title = "Número de anuncios en cada scraping de InsideAirbnb",
         subtitle = "Madrid 2015-2018",
         y = "número de anuncios",
         x = "fecha",
         caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb")
dev.off()

# separado por tipo de alojamiento ----
png(filename=paste("images/airbnb/evolucion/anuncios-por-mes-room-type.png", sep = ""),width = 1000,height = 300)
dates.count.room_type %>% filter(!room_type=="Habitación compartida") %>%
  ggplot () +
  # annotate("text",x=as.Date("2018-04-15"),y=1000,label="acuerdo",color="#000000",size=4) +
  geom_point(aes(fechab,anuncios,group=room_type,color=room_type),size=1.5) +
  geom_line(aes(fechab,anuncios,group=room_type,color=room_type),size=1.5) +
  # geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  ylim(0, max(dates.count.room_type$anuncios)) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 14) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "top"
  ) +
  labs(title = "Número de anuncios de Airbnb por tipo de alojamiento en Madrid",
       subtitle = "2015-2018 (publicados en cada scraping de InsideAirbnb)",
       y = "número de anuncios",
       x = "fecha",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb")
dev.off()

# separado por tipo de alojamiento y reviews ----
dates.count.room_type.reviews <- data_long %>% filter (exists ==1) %>% group_by(fechab,room_type,reviews.type) %>% summarise(anuncios=n())

png(filename=paste("images/airbnb/evolucion/anuncios-por-mes-room-type-reviews.png", sep = ""),width = 1000,height = 500)
ggplot (NULL) +
  geom_point(data=dates.count.room_type.reviews  %>% filter(!room_type=="Habitación compartida" & !reviews.type == "ninguna review"),
             aes(fechab,anuncios,color=room_type),size=1.5) +
  geom_line(data=dates.count.room_type.reviews  %>% filter(!room_type=="Habitación compartida" & !reviews.type == "ninguna review"),
            aes(fechab,anuncios,group=room_type,color=room_type),size=1.5) +
  # todas las habitaciones y viviendas completas
  geom_point(data=dates.count.room_type%>% filter(!room_type=="Habitación compartida"),
            aes(fechab,anuncios,color=room_type),size=1.5) +
  geom_line(data=dates.count.room_type%>% filter(!room_type=="Habitación compartida"),
            aes(fechab,anuncios,group=room_type,color=room_type),size=1.5,linetype = "dotted") +
  # número a final de línea
  geom_text(data=dates.count.room_type.reviews  %>% 
              filter(!room_type=="Habitación compartida" & !reviews.type == "ninguna review" & fechab > as.Date("2019-03-01")),
            aes(fechab+10,anuncios,label=format(anuncios, nsmall=1, big.mark=".")), size=4,hjust=0) +
  geom_text(data=dates.count.room_type %>% 
              filter(!room_type=="Habitación compartida" & fechab > as.Date("2019-03-01")),
            aes(fechab+10,anuncios,label=format(anuncios, nsmall=1, big.mark=".")), size=4,hjust=0) +
  scale_y_continuous(limits=c(0, max(dates.count.room_type$anuncios)),labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 14) +
  theme(
    panel.grid.minor.x = element_blank(),
    # panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "top"
  ) +
  labs(title = "Número de anuncios de Airbnb por tipo de alojamiento en Madrid",
       subtitle = "2015-2018 (publicados en cada scraping de InsideAirbnb)",
       y = "número de anuncios",
       x = "fecha",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb",
       color="Tipo de alojamiento") +
  facet_wrap(~reviews.type)
dev.off()


# separado por tipo de alojamiento, availability y reviews ----
dates.count.reviews.ava.rev <- data_long %>% filter (exists ==1) %>% group_by(fechab,room_type,reviews.type,availability.type) %>% summarise(anuncios=n())

png(filename=paste("images/airbnb/evolucion/anuncios-por-mes-room-type-reviews-ava-multi.png", sep = ""),width = 1000,height = 500)
ggplot (NULL) +
  # geom_point(data=dates.count.reviews.ava.rev  %>% filter(!room_type=="Habitación compartida" & !reviews.type == "ninguna review" & !availability.type == "no"),
  #            aes(fechab,anuncios,color=room_type),size=1.5) +
  geom_line(data=dates.count.reviews.ava.rev  %>% filter(!room_type=="Habitación compartida" & !reviews.type == "ninguna review" & !availability.type == "no"),
            aes(fechab,anuncios,group=room_type,color=room_type),size=1.5) +
  # todas las habitaciones y viviendas completas
  # geom_point(data=dates.count.room_type%>% filter(!room_type=="Habitación compartida"),
  #            aes(fechab,anuncios,color=room_type),size=1.5) +
  # geom_line(data=dates.count.room_type%>% filter(!room_type=="Habitación compartida"),
  #           aes(fechab,anuncios,group=room_type,color=room_type),size=1.5,linetype = "dotted") +
  # número a final de línea
  # geom_text(data=dates.count.room_type.reviews  %>% 
  #             filter(!room_type=="Habitación compartida" & !reviews.type == "ninguna review" & fechab > as.Date("2019-03-01")),
  #           aes(fechab+10,anuncios,label=format(anuncios, nsmall=1, big.mark=".")), size=4,hjust=0) +
  # geom_text(data=dates.count.room_type %>% 
  #             filter(!room_type=="Habitación compartida" & fechab > as.Date("2019-03-01")),
  #           aes(fechab+10,anuncios,label=format(anuncios, nsmall=1, big.mark=".")), size=4,hjust=0) +
  scale_y_continuous(limits=c(0, max(dates.count.reviews.ava.rev$anuncios)),labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 14) +
  theme(
    panel.grid.minor.x = element_blank(),
    # panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "top"
  ) +
  labs(title = "Número de anuncios de Airbnb por tipo de alojamiento en Madrid",
       subtitle = "Filtrado por: pisos con reviews y con disponibilidad",
       y = "número de anuncios",
       x = "fecha",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb",
       color="Tipo de alojamiento") +
  facet_wrap(~availability.type)
dev.off()



# separado por tipo de alojamiento, availability.simple y reviews ----
dates.count.reviews.ava.s.rev <- data_long %>% filter (exists ==1) %>% group_by(fechab,room_type,reviews.type,availability.type.s) %>% summarise(anuncios=n())

png(filename=paste("images/airbnb/evolucion/anuncios-por-mes-room-type-reviews-ava.s-multi.png", sep = ""),width = 1000,height = 500)
ggplot (NULL) +
  # geom_point(data=dates.count.reviews.ava.rev  %>% filter(!room_type=="Habitación compartida" & !reviews.type == "ninguna review" & !availability.type == "no"),
  #            aes(fechab,anuncios,color=room_type),size=1.5) +
  geom_line(data=dates.count.reviews.ava.s.rev  %>% filter(!room_type=="Habitación compartida" & !reviews.type == "ninguna review" & !availability.type.s == "no"),
            aes(fechab,anuncios,group=room_type,color=room_type),size=1.5) +
  # todas las habitaciones y viviendas completas
  # geom_point(data=dates.count.room_type%>% filter(!room_type=="Habitación compartida"),
  #            aes(fechab,anuncios,color=room_type),size=1.5) +
  # geom_line(data=dates.count.room_type%>% filter(!room_type=="Habitación compartida"),
  #           aes(fechab,anuncios,group=room_type,color=room_type),size=1.5,linetype = "dotted") +
  # número a final de línea
  geom_text(data=dates.count.reviews.ava.s.rev  %>% filter(!room_type=="Habitación compartida" & 
                                                             !reviews.type == "ninguna review" & !availability.type.s == "no"& fechab > as.Date("2019-03-01")
              ),
            aes(fechab+10,anuncios,label=format(anuncios, nsmall=1, big.mark=".")), size=4,hjust=0) +
  # geom_text(data=dates.count.room_type %>% 
  #             filter(!room_type=="Habitación compartida" & fechab > as.Date("2019-03-01")),
#           aes(fechab+10,anuncios,label=format(anuncios, nsmall=1, big.mark=".")), size=4,hjust=0) +
scale_y_continuous(limits=c(0, max(dates.count.reviews.ava.s.rev$anuncios)),labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 14) +
  theme(
    panel.grid.minor.x = element_blank(),
    # panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "top"
  ) +
  labs(title = "Número de anuncios de Airbnb por tipo de alojamiento en Madrid",
       subtitle = "Filtrado por: pisos con reviews y con disponibilidad",
       y = "número de anuncios",
       x = "fecha",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb",
       color="Tipo de alojamiento") +
  facet_wrap(~availability.type.s)
dev.off()

# anuncios de pisos completos
dates.count.room_type %>% filter(room_type=="Vivienda completa") %>%
  ggplot () + 
  geom_col(aes(fechab,anuncios))



# separado por tipo de host ----
dates.count.host.type <- data_long %>% filter (exists ==1) %>% group_by(fechab,host.type) %>% summarise(anuncios=n())

# timeline
png(filename=paste("images/airbnb/evolucion/anuncios-por-mes-host-type.png", sep = ""),width = 1000,height = 300)
dates.count.host.type %>%
  ggplot () +
  # annotate("text",x=as.Date("2018-05-1"),y=1000,label="acuerdo",color="#000000",size=4) +
  geom_line(aes(fechab,anuncios,group=host.type,color=host.type),size=1.5) +
  geom_point(aes(fechab,anuncios,group=host.type,color=host.type),size=1.5) +
  # geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  ylim(0, max(dates.count.host.type$anuncios)) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 14) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "top"
  ) +
  labs(title = "Número de anuncios de Airbnb por tipo de host en Madrid",
       subtitle = "2015-2018 (publicados en cada scraping de InsideAirbnb)",
       y = "número de anuncios",
       x = "fecha",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb")
dev.off()

# separado por tipo de host com más clasificaciones ----
dates.count.host.type.m <- data_long %>% filter (exists ==1) %>% group_by(fechab,host.type.m) %>% summarise(anuncios=n())

# timeline
png(filename=paste("images/airbnb/evolucion/anuncios-por-mes-host-type-1-2-3-more.png", sep = ""),width = 1000,height = 300)
dates.count.host.type.m %>%
  ggplot () +
  # annotate("text",x=as.Date("2018-05-1"),y=1000,label="acuerdo",color="#000000",size=4) +
  geom_line(aes(fechab,anuncios,group=host.type.m,color=host.type.m),size=1.5) +
  geom_point(aes(fechab,anuncios,group=host.type.m,color=host.type.m),size=1.5) +
  # geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  ylim(0, max(dates.count.host.type.m$anuncios)) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 14) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "top"
  ) +
  labs(title = "Número de anuncios de Airbnb por tipo de host en Madrid",
       subtitle = "2015-2018 (publicados en cada scraping de InsideAirbnb)",
       y = "número de anuncios",
       x = "fecha",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb",
       color="El host gestiona")
dev.off()

# separado por tipo de host y alojamiento ----
dates.count.host.room.type <- data_long %>% filter (exists ==1) %>% group_by(fechab,room_type,host.type) %>% summarise(anuncios=n())

# timeline
png(filename=paste("images/airbnb/evolucion/anuncios-por-mes-host-room-type.png", sep = ""),width = 1000,height = 300)
dates.count.host.room.type %>% filter(!room_type=="Habitación compartida") %>%
  ggplot () +
  # annotate("text",x=as.Date("2018-05-15"),y=1000,label="acuerdo",color="#000000",size=4,base_family = "Roboto Condensed",hjust=1) +
  geom_line(aes(fechab,anuncios,group=host.type,color=host.type),size=1.5) +
  # geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  ylim(0, max(dates.count.host.room.type$anuncios)) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 14) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "top"
  ) +
  labs(title = "Número de anuncios en cada scraping de InsideAirbnb por tipo de host",
       subtitle = "Madrid 2015-2018",
       y = "número de anuncios",
       x = "fecha",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb",
       color="El host gestiona") +
  facet_wrap(~room_type)
dev.off()

png(filename=paste("images/airbnb/evolucion/anuncios-por-mes-room-host-type.png", sep = ""),width = 1000,height = 300)
dates.count.host.room.type %>% filter(!room_type=="Habitación compartida") %>%
  ggplot () +
  # annotate("text",x=as.Date("2018-05-15"),y=1000,label="acuerdo",color="#000000",size=4) +
  # geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  geom_step(aes(fechab,anuncios,group=room_type,color=room_type),size=1.5) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 14) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "top"
  ) +
  labs(title = "Número de anuncios en cada scraping de InsideAirbnb por tipo de host",
       subtitle = "Madrid 2015-2018",
       y = "número de anuncios",
       x = "fecha",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb") +
  facet_wrap(~host.type)
dev.off()


# separado por tipo de host multiple y alojamiento -----------------------------------
dates.count.host.room.type.m <- data_long %>% filter (exists ==1) %>% group_by(fechab,room_type,host.type.m) %>% summarise(anuncios=n())

# timeline
png(filename=paste("images/airbnb/evolucion/anuncios-por-mes-host-m-room-type.png", sep = ""),width = 1000,height = 300)
dates.count.host.room.type.m %>% filter(!room_type=="Habitación compartida") %>%
  ggplot () +
  geom_line(aes(fechab,anuncios,group=host.type.m,color=host.type.m),size=1.5) +
  # annotate("text",x=as.Date("2018-05-15"),y=100,label="acuerdo",color="#000000",size=4, hjust = 1) +
  # geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  ylim(0, max(dates.count.host.room.type.m$anuncios)) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 14) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "top"
  ) +
  labs(title = "Número de anuncios en cada scraping de InsideAirbnb por tipo de host",
       subtitle = "Madrid 2015-2018",
       y = "número de anuncios",
       x = "fecha",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb",
       color="El host gestiona") +
  facet_wrap(~room_type)
dev.off()

png(filename=paste("images/airbnb/evolucion/anuncios-por-mes-room-host-m-type.png", sep = ""),width = 1000,height = 300)
dates.count.host.room.type.m %>% filter(!room_type=="Habitación compartida") %>%
  ggplot () +
  # geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  geom_line(aes(fechab,anuncios,group=room_type,color=room_type),size=1.5) +
  # annotate("text",x=as.Date("2018-05-15"),y=100,label="acuerdo",color="#000000",size=4, hjust = 1 ) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 14) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "top"
  ) +
  labs(title = "Número de anuncios en cada scraping de InsideAirbnb por tipo de host",
       subtitle = "Madrid 2015-2018",
       y = "número de anuncios",
       x = "fecha",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb",
       color="El host gestiona") +
  facet_wrap(~host.type.m)
dev.off()

# data_long[data_long$exists == 1 & data_long$fechab > "2018-04-01",] %>%
# ggplot(aes(x = as.factor(fecha), y = as.factor(id))) +
#   # geom_dotplot(binaxis = "y", stackdir = "center", position = "dodge",alpha=0.3,size=0.1)
#   geom_point(size=0.1,alpha=0.8)


# detects which listings were erased/dissapeared -------------------------------------------------------------------------------
listings.desaparecidos <- listings.total.all
# sets all values to 0
listings.desaparecidos[,8:24] <- 0

# column.select <- paste("d",dates[1],sep = "")
# column.select2 <- paste("d",dates[1+1],sep = "")
# tesx <- listings.total %>% filter(  (!!sym(column.select)) == 1 & (!!sym(column.select2)) == 0 ) %>% select(id)
# 
# listings.desaparecidos[listings.desaparecidos$id %in% tesx$id,column.select2] <-33

for (i in 1:length(dates)) {
  print(i)
  column.select <- paste("d",dates[i],sep = "")
  print(column.select)
  column.select2 <- paste("d",dates[i+1],sep = "")
  print(column.select2)
  # debe cumplir condición: está en columna 1 pero no está en 2
  id.removed <- listings.total %>% filter(  (!!sym(column.select)) == 1 & (!!sym(column.select2)) == 0 ) %>% select(id)
  # sets to 1 if erased and stores it in column.select2 date
  listings.desaparecidos[listings.desaparecidos$id %in% id.removed$id,column.select2] <- 1
}

# cuántas veces "desapareció" cada anuncios en el periodo estudiado -------
listings.desaparecidos$sum <- rowSums(listings.desaparecidos[,8:24])
table(listings.desaparecidos$sum)
# De los 63.704 anuncios que han pasado por la plataforma 15.147 (24%) siempre han estado presentes desde que se publicaron alguna vez. 
# 39.389 (62%) desaparecieron una vez. 
# 6.843 (10%) desaparecieron 2 veces.
# 2.325 (4%) desparecieron entre 3 y 7 veces.

# ¿cuántos anuncios desparecieron en junio y luego volvieron a desaperecer? 198
# filter(listings.desaparecidos, d180609 == 1 & (d180710 == 1 | d180818 == 1 | d180911  == 1) ) %>% select(room_type,calculated_host_listings_count,d180710,d180818,d180911 )

# converts to long format -----
listings.desaparecidos_long <-  listings.desaparecidos %>% gather(fecha, eliminated, 8:24) #starts in 4th column after other variables
# parse date
listings.desaparecidos_long$fechab <- as.Date( strapplyc( as.character(listings.desaparecidos_long$fecha), "d(.*)", simplify = TRUE), "%Y-%m-%d")

# classify type of host
listings.desaparecidos_long$host.type <- ""
listings.desaparecidos_long[listings.desaparecidos_long$calculated_host_listings_count == 1,]$host.type <- "1 anuncio"
listings.desaparecidos_long[listings.desaparecidos_long$calculated_host_listings_count > 1,]$host.type <- "varios anuncios"

desaparecidos.count <- listings.desaparecidos_long %>% filter (eliminated ==1) %>% group_by(fechab) %>% summarise(anuncios=n())
desaparecidos.count.room_type <- listings.desaparecidos_long %>% filter (eliminated ==1) %>% group_by(fechab,room_type) %>% summarise(anuncios=n())
desaparecidos.count.host_type <- listings.desaparecidos_long %>% filter (eliminated ==1) %>% group_by(fechab,host.type) %>% summarise(anuncios=n())

# png(filename=paste("images/airbnb/evolucion/anuncios-por-mes-host-type.png", sep = ""),width = 1000,height = 300)
desaparecidos.count %>%
  ggplot () +
  # annotate("text",x=as.Date("2018-05-1"),y=1000,label="acuerdo",color="#000000",size=4) +
  # geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  geom_col(aes(fechab,anuncios),size=1.5) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 14) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "top"
  ) +
  labs(title = "Número de anuncios desaparecidos",
       subtitle = "Madrid 2015-2018 (entre fechas consecutivas de scraping)",
       y = "número de anuncios",
       x = "fecha",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb")
# dev.off()

# 1 Miramos cuántos son según habitación entre los eliminados
png(filename=paste("images/airbnb/evolucion/anuncios-eliminados-room-type.png", sep = ""),width = 1000,height = 300)
desaparecidos.count.room_type  %>%
  ggplot () +
  geom_col(aes(fechab,anuncios,fill=room_type)) +
  geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 14) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "top"
  ) +
  labs(title = "Número de anuncios desaparecidos según tipo de habitación",
       subtitle = "Madrid 2015-2018 (entre fechas consecutivas de scraping)",
       y = "número de anuncios",
       x = "fecha",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb",
       fill="Tipo de alojamiento")
dev.off()

# 2 Miramos cuál es la proporción de tipo de anuncios según habitación entre los eliminados
png(filename=paste("images/airbnb/evolucion/anuncios-eliminados-porcentaje-room-type.png", sep = ""),width = 1000,height = 300)
desaparecidos.count.room_type  %>%
  ggplot () +
  geom_col(aes(fechab,anuncios,fill=room_type),position = "fill" ) +
  geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 14) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "top"
  ) +
  labs(title = "Porcentaje de anuncios desaparecidos según tipo de habitación",
       subtitle = "Madrid 2015-2018 (entre fechas consecutivas de scraping)",
       y = "%",
       x = "fecha",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb",
       fill="Tipo de alojamiento")
dev.off()

# 3 Miramos cuál es el número de anuncios según host type entre los eliminados
png(filename=paste("images/airbnb/evolucion/anuncios-eliminados-host-type.png", sep = ""),width = 1000,height = 300)
desaparecidos.count.host_type  %>%
  ggplot () +
  geom_col(aes(fechab,anuncios,fill=host.type)) +
  geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 14) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "top"
  ) +
  labs(title = "Número de anuncios desaparecidos según tipo de host",
       subtitle = "Madrid 2015-2018 (entre fechas consecutivas de scraping)",
       y = "número de anuncios",
       x = "fecha",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb",
       fill="El host gestiona")
dev.off()

# 4 Miramos cuál es la proporción según habitación entre los eliminados
png(filename=paste("images/airbnb/evolucion/anuncios-eliminados-porcentaje-host-type.png", sep = ""),width = 1000,height = 300)
desaparecidos.count.host_type  %>%
  ggplot () +
  geom_col(aes(fechab,anuncios,fill=host.type),position = "fill" ) +
  geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 14) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "top"
  ) +
  labs(title = "Porcentaje de anuncios desaparecidos según tipo de host",
       subtitle = "Madrid 2015-2018 (entre fechas consecutivas de scraping)",
       y = "%",
       x = "fecha",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb",
       fill="El host gestiona")
dev.off()

# aparecidos o nuevos
# detects which listings are NEW in each scraping date (compare with previous) -------------------------------------------------------------------------------
listings.new <- listings.total.all
# sets all values to 0
listings.new[,8:24] <- 0

for (i in 1:length(dates)) {
  print(i)
  column.select <- paste("d",dates[i],sep = "")
  print(column.select)
  column.select2 <- paste("d",dates[i+1],sep = "")
  print(column.select2)
  # debe cumplir condición: no está en columna 1 pero sí está en 2
  id.new <- listings.total %>% filter(  (!!sym(column.select)) == 0 & (!!sym(column.select2)) == 1 ) %>% select(id)
  # sets to 1 if newand stores it in column.select2 date
  listings.new[listings.new$id %in% id.new$id,column.select2] <- 1
}

# converts to long format 
listings.new_long <-  listings.new %>% gather(fecha, new, 8:24) #starts in 5th column after other variables
# parse date
listings.new_long$fechab <- as.Date( strapplyc( as.character(listings.new_long$fecha), "d(.*)", simplify = TRUE), "%Y-%m-%d")
# classify type of host
listings.new_long$host.type <- ""
listings.new_long[listings.new_long$calculated_host_listings_count == 1,]$host.type <- "single listing"
listings.new_long[listings.new_long$calculated_host_listings_count > 1,]$host.type <- "multiple listings"

new.count <- listings.new_long %>% filter (new ==1) %>% group_by(fechab) %>% summarise(anuncios=n())
new.count.room_type <- listings.new_long %>% filter (new ==1) %>% group_by(fechab,room_type) %>% summarise(anuncios=n())
new.count.host_type <- listings.new_long %>% filter (new ==1) %>% group_by(fechab,host.type) %>% summarise(anuncios=n())

new.count %>%
  ggplot () +
  # annotate("text",x=as.Date("2018-05-1"),y=1000,label="acuerdo",color="#000000",size=4) +
  # geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  geom_col(aes(fechab,anuncios),size=1.5) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 14) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "top"
  ) +
  labs(title = "Número de anuncios new",
       subtitle = "Madrid 2015-2018 (entre fechas consecutivas de scraping)",
       y = "número de anuncios",
       x = "fecha",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb")

# 1 Miramos cuántos son según habitación entre los new
png(filename=paste("images/airbnb/evolucion/anuncios-new-room-type.png", sep = ""),width = 1000,height = 300)
new.count.room_type  %>%
  ggplot () +
  geom_col(aes(fechab,anuncios,fill=room_type)) +
  geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 14) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "top"
  ) +
  labs(title = "Número de anuncios nuevos según tipo de habitación",
       subtitle = "Madrid 2015-2018 (entre fechas consecutivas de scraping)",
       y = "número de anuncios",
       x = "fecha",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb",
       fill="Tipo de alojamiento")
dev.off()

# 2 Miramos cuál es la proporción de tipo de anuncios según habitación entre los new
png(filename=paste("images/airbnb/evolucion/anuncios-new-porcentaje-room-type.png", sep = ""),width = 1000,height = 300)
new.count.room_type  %>%
  ggplot () +
  geom_col(aes(fechab,anuncios,fill=room_type),position = "fill" ) +
  geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 14) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "top"
  ) +
  labs(title = "Porcentaje de anuncios nuevos según tipo de habitación",
       subtitle = "Madrid 2015-2018 (entre fechas consecutivas de scraping)",
       y = "%",
       x = "fecha",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb",
       fill="Tipo de alojamiento")
dev.off()

# 3 Miramos cuál es el número de anuncios según host type entre los new
png(filename=paste("images/airbnb/evolucion/anuncios-new-host-type.png", sep = ""),width = 1000,height = 300)
new.count.host_type  %>%
  ggplot () +
  geom_col(aes(fechab,anuncios,fill=host.type)) +
  geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 14) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "top"
  ) +
  labs(title = "Número de anuncios nuevos según tipo de host",
       subtitle = "Madrid 2015-2018 (entre fechas consecutivas de scraping)",
       y = "número de anuncios",
       x = "fecha",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb",
       fill="El host gestiona")
dev.off()

# 4 Miramos cuál es la proporción según habitación entre los new
png(filename=paste("images/airbnb/evolucion/anuncios-new-porcentaje-host-type.png", sep = ""),width = 1000,height = 300)
new.count.host_type  %>%
  ggplot () +
  geom_col(aes(fechab,anuncios,fill=host.type),position = "fill" ) +
  geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 14) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "top"
  ) +
  labs(title = "Porcentaje de anuncios nuevos según tipo de host",
       subtitle = "Madrid 2015-2018 (entre fechas consecutivas de scraping)",
       y = "%",
       x = "fecha",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb",
       fill="El host gestiona")
dev.off()


# Adds when listing was first found ------------------------------------------------------
listings.total.found <- data_long %>% filter(exists == 1) %>% group_by(id) %>% summarise(found = min(fechab)) %>% ungroup()
# listings.total <- listings.total %>% select(-"min(fechab)")
listings.total <- inner_join(listings.total,listings.total.found)

# calculates in how many scrapings has been every listing ----
listings.total$sum <- rowSums(listings.total[,8:24])
listings.total$sum2018 <- rowSums(listings.total[,22:29])

# hist(listings.total$sum)

# extends color paletter
colourCount <- length(unique(listings.total$found))
getPalette <- colorRampPalette(brewer.pal(9, "Set1"))

# Plots: histograms -----

# histograma básico
png(filename=paste("images/airbnb/evolucion/eliminados-01.png", sep = ""),width = 1000,height = 750)
listings.total %>%
  ggplot(aes(sum)) + 
  geom_histogram(binwidth = 1) +
  scale_fill_manual(values = getPalette(colourCount)) +
  # scale_fill_continuous() +
  theme_minimal(base_family = "Roboto Condensed",base_size = 14) +
  theme(
    # panel.grid.minor.x = element_blank(), 
    # panel.grid.major.x = element_blank(),
    legend.position = "bottom"
  ) +
  labs(title = "Cuántas veces está un anuncio en los 27 scraping de InsideAirbnb",
       subtitle = "2015-2018. Número de listings analizados",
       y = "número de anuncios",
       x = "número de veces que aparece",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb")
dev.off()

# histograma coloreado según cuándo fue encontrado
png(filename=paste("images/airbnb/evolucion/eliminados-02.png", sep = ""),width = 1000,height = 750)
listings.total %>%
  ggplot(aes(sum,fill=as.factor(found))) + 
  geom_histogram(binwidth = 1) +
  scale_fill_manual(values = getPalette(colourCount)) +
  # scale_fill_continuous() +
  theme_minimal(base_family = "Roboto Condensed",base_size = 14) +
  theme(
    # panel.grid.minor.x = element_blank(), 
    # panel.grid.major.x = element_blank(),
    legend.position = "bottom"
  ) +
  labs(title = "Cuántas veces está un anuncio en los 27 scraping de InsideAirbnb",
       subtitle = paste("2015-2018. Número de listings analizados",nrow(listings.total) ),
       y = "número de anuncios",
       x = "número de veces que aparece",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb")
dev.off()

# histograma coloreado según cuándo fue encontrado y filtrando por mayor que una fecha
png(filename=paste("images/airbnb/evolucion/eliminados-03.png", sep = ""),width = 1000,height = 750)
listings.total %>% filter(found > "2018-04-01") %>%
  ggplot(aes(sum,fill=as.factor(found))) + 
  geom_histogram(binwidth = 1) +
  scale_fill_manual(values = getPalette(colourCount)) +
  # scale_fill_continuous() +
  theme_minimal(base_family = "Roboto Condensed",base_size = 14) +
  theme(
    # panel.grid.minor.x = element_blank(), 
    # panel.grid.major.x = element_blank(),
    legend.position = "bottom"
  ) +
  labs(title = "Cuántas veces está un anuncio en los 27 scraping de InsideAirbnb",
       subtitle = paste("2015-2018. Número de listings analizados",nrow(listings.total) ),
       y = "número de anuncios",
       x = "número de veces que aparece",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb")
dev.off()

# histograma coloreado según cuándo fue encontrado y filtrando por fecha concreta
png(filename=paste("images/airbnb/evolucion/eliminados-04.png", sep = ""),width = 1000,height = 750)
listings.total %>% filter(d180911 == 1) %>%
  ggplot(aes(sum,fill=as.factor(found))) + 
  geom_histogram(binwidth = 1) +
  scale_fill_manual(values = getPalette(colourCount)) +
  # scale_fill_continuous() +
  theme_minimal(base_family = "Roboto Condensed",base_size = 14) +
  theme(
    # panel.grid.minor.x = element_blank(), 
    # panel.grid.major.x = element_blank(),
    legend.position = "bottom"
  ) +
  labs(title = "Cuántas veces está un anuncio en los 27 scraping de InsideAirbnb",
       subtitle = paste("2015-2018. Número de listings analizados",nrow(listings.total) ),
       y = "número de anuncios",
       x = "número de veces que aparece",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb")
dev.off()

# Plots: when they were first found. Bars -----

# barra  coloreado según cuándo fue encontrado y filtrando por fecha concreta
png(filename=paste("images/airbnb/evolucion/eliminados-06.png", sep = ""),width = 1000,height = 750)
listings.total %>% filter(d180818 == 1) %>%
  ggplot(aes(x=1,fill=as.factor(found))) + 
  geom_bar() +
  scale_fill_manual(values = getPalette(colourCount)) +
  # scale_fill_continuous() +
  theme_minimal(base_family = "Roboto Condensed",base_size = 14) +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "bottom"
  ) +
  labs(title = "Cuándo se publicaron por 1ª vez los anuncios que estaban en agosto 2018",
       subtitle = "",
       y = "nº de anuncios",
       x = "",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb"
       )+
  coord_flip() 
dev.off()

# loop all the dates
for (i in 1:length(dates)) {
# for (i in 1:2) {
  # barra  coloreado según cuándo fue encontrado y filtrando por fecha concreta
  column.select <- paste("d",dates[i],sep = "")
  print(column.select)
  png(filename=paste("images/airbnb/evolucion/matrix/",column.select,".png", sep = ""),width = 400,height = 1000)
  filename <- paste("images/airbnb/evolucion/matrix/",column.select,".png", sep = "")
  print(filename)
  # seen in Pass a string as variable name in dplyr::filter https://stackoverflow.com/questions/48219732/pass-a-string-as-variable-name-in-dplyrfilter
  p <- listings.total %>% filter((!!sym(column.select)) == 1) %>%
      ggplot(aes(x=1,fill=as.factor(found))) + # reverse orden of factors
      geom_bar() +
      scale_fill_manual(values = getPalette(colourCount)) +
      theme_minimal(base_size =10) +
      theme(
        panel.grid.minor.y = element_blank(),
        # panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        # axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        legend.title = element_blank()
        # legend.position = "none"
      ) +
      labs(title = dates[i],
           subtitle = "",
           y = "",
           x = ""
           # caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb"
      )+
    scale_y_continuous(limits = c(0,20000)) 
      # coord_flip()
  print(p) 
  dev.off()
}

# Then build matrix with "montage" the imagemagick function with bash:
# montage d1* -geometry 200x800+0+0 vertical.png

# for network graph ------
# subset data for speed the process. Only after 2018-04 
test <- data_long[data_long$exists == 1 & data_long$fechab > "2018-04-01",]
ggplot(test[sample(nrow(test),100),],aes(x = as.factor(fecha), y = as.factor(id))) +
  geom_point(size=3,alpha=0.8)

# png(filename=paste("temp/eliminados-03.png", sep = ""),width = 2000,height = 5000)
# ggplot(test,aes(x = as.factor(fecha), y = as.factor(id))) +
#   geom_bin2d()
# dev.off()

# Exports data to use them in gephi
rownames(test) <- 1:111557

links <- test[,1:2]
names(links) <- c("source","target")
nodes <- as.data.frame(c(unique(links$target),unique(links$source)))

# Sve data if needed
write.csv(links, file="temp/links-listings-post-2017.csv")
write.csv(nodes, file="temp/nodes-listings-post-2017.csv")

# build matrix for heat map -----
# # Tests for calculating with 2 databases
# # Find listings that are in first two scrapings
# listings.total[listings.total$d150717 == 1 & listings.total$d150430 == 1,1:3]
# listings.total %>% filter(d150717 == 1 & d150430 == 1)
# 
# # count listings
# nrow(listings.total[listings.total$d150717 == 1 & listings.total$d150430 == 1,])
# nrow(listings.total %>% filter(d150717 == 1 & d150430 == 1))

# Analyses ony Entire home/apartment ads --------
listings.total.all <- listings.total # saves original
listings.total <- filter(listings.total.all,room_type == "Piso completo") #only entire homes

# Creates matrix
heat.matrix <- data.frame(matrix(ncol = length(dates),nrow = length(dates)  ))
names(heat.matrix) <- dates
row.names(heat.matrix) <- dates

for (j in 1:length(dates)) {
  print(paste("j:",j))
  for (i in 1:length(dates)) {
    print(paste("i:",i))
    column.select.i <- paste("d",dates[j],sep = "")
    column.select.j <- paste("d",dates[i],sep = "")
    print(paste(column.select.i,"interseccion con",column.select.j,":"))
      coinciden <- nrow(listings.total %>% filter((!!sym(column.select.i))== 1 & (!!sym(column.select.j)) == 1) )
      print(coinciden)
      print(paste("mete dato en ","[j:",j,", i:",i,"]",sep = ""))
      heat.matrix[j,i] <- coinciden 
      # heat.matrix[i,j] <- coinciden 
  }
}

write.csv(heat.matrix, file="temp/heat.matrix.csv")

# basic heatmap
image(as.matrix(heat.matrix), xlab = 'Matrix rows', ylab = 'Matrix columns', axes = F)

# add id to column
heat.matrix$id <- colnames(heat.matrix)
# melt, from wide to long format
heat.matrix.m <- melt(heat.matrix)
# heat.matrix.m <- melt(heat.matrix,variable.name = "sample",value.name = "other",id="id")
ggplot(heat.matrix.m , aes(x = id, y = variable, fill = -value)) + geom_tile()

# help https://blog.aicry.com/r-heat-maps-with-ggplot2/index.html
# extends color palette
hm.palette <- colorRampPalette(rev(brewer.pal(9, 'YlOrBr')), space='Lab')

# png(filename=paste("images/airbnb/evolucion/heat-map-coincidencias-madrid-insideairbnb-02.png", sep = ""),width = 1200,height = 1200)
png(filename=paste("images/airbnb/evolucion/heat-map-coincidencias-madrid-insideairbnb-02-pisos-completos.png", sep = ""),width = 1200,height = 1200)
ggplot(heat.matrix.m , aes(x = id, y = variable, fill = value)) +
  geom_tile() +
  coord_equal() +
  scale_fill_gradientn(colours = rev(hm.palette(100))) +
  theme_minimal(base_size = 12) +
  theme(
    # panel.grid.minor.x = element_blank(), 
    # panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 90, vjust = 0.4)
  ) +
  labs(title = "Número de anuncios coincidentes en bases de datos de InsideAirbnb",
       # subtitle = "2015-2018",
       subtitle = "Pisos completos. 2015-2018",
       y = "fechas",
       x = "fechas",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb")
dev.off()

# heat matrix desde 180412-----------
dates.p <- dates
dates.p <- dates.p[-c(1:5)]

heat.matrix.p <- data.frame(matrix(ncol = length(dates.p),nrow = length(dates.p)  ))
names(heat.matrix.p) <- dates.p
row.names(heat.matrix.p) <- dates.p

for (j in 1:length(dates.p)) {
  print(paste("j:",j))
  for (i in 1:length(dates.p)) {
    print(paste("i:",i))
    column.select.i <- paste("d",dates.p[j],sep = "")
    column.select.j <- paste("d",dates.p[i],sep = "")
    print(paste(column.select.i,"interseccion con",column.select.j,":"))
    coinciden <- nrow(listings.total %>% filter((!!sym(column.select.i))== 1 & (!!sym(column.select.j)) == 1) )
    print(coinciden)
    print(paste("mete dato en ","[j:",j,", i:",i,"]",sep = ""))
    heat.matrix.p[j,i] <- coinciden 
    # heat.matrix[i,j] <- coinciden 
  }
}

write.csv(heat.matrix.p, file="temp/heat.matrix.p.csv")

# basic heatmap
image(as.matrix(heat.matrix.p), xlab = 'Matrix rows', ylab = 'Matrix columns', axes = F)

# add id to column
heat.matrix.p$id <- colnames(heat.matrix.p)
# melt, from wide to long format
heat.matrix.m.p <- melt(heat.matrix.p)
# heat.matrix.m <- melt(heat.matrix,variable.name = "sample",value.name = "other",id="id")
ggplot(heat.matrix.m.p , aes(x = id, y = variable, fill = -value)) + geom_tile()

# png(filename=paste("images/airbnb/evolucion/heat-map-coincidencias-madrid-insideairbnb-02p.png", sep = ""),width = 1200,height = 1200)
png(filename=paste("images/airbnb/evolucion/heat-map-coincidencias-madrid-insideairbnb-02p-pisos-completos.png", sep = ""),width = 1200,height = 1200)
ggplot(heat.matrix.m.p , aes(x = id, y = variable, fill = value)) +
  geom_tile() +
  # geom_text(aes(label=value)) +
  coord_equal() +
  scale_fill_gradientn(colours = rev(hm.palette(100))) +
  theme_minimal(base_size = 22) +
  theme(
    # panel.grid.minor.x = element_blank(), 
    # panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 90, vjust = 0.4)
  ) +
  labs(title = "Número de anuncios coincidentes en bases de datos de InsideAirbnb",
       subtitle = "Pisos completos. 2016-2018",
       y = "fechas",
       x = "fechas",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb")
dev.off()

# Anuncios coincidentes en gráficos de línea
png(filename=paste("images/airbnb/evolucion/lineas-coincidencias-madrid-insideairbnb-03.png", sep = ""),width = 1400,height = 600)
ggplot(heat.matrix.m.p , aes(x = id, y = value, group = variable,color=variable)) +
  geom_line() +
  geom_point(size=0.5) +
  geom_text(aes(label=value),size=3,color="#000000")+
  scale_fill_manual(values = getPalette(colourCount)) +
  theme_minimal(base_size = 25) +
  theme(
    # panel.grid.minor.x = element_blank(),
    # panel.grid.major.x = element_blank(),
    legend.position = "right",
    axis.text.x = element_text(angle = 90, vjust = 0.4)
  ) +
  labs(title = "Anuncios de pisos completos coincidentes en bases de datos de InsideAirbnb",
       subtitle = "Cada línea es una base de datos. 2016-2018",
       y = "nº coincidencias",
       x = "fechas",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb")
dev.off()


# heat matrix desde 161107 normalizada-----------
heat.matrix.n <- data.frame(matrix(ncol = length(dates.p),nrow = length(dates.p)  ))
names(heat.matrix.n) <- dates.p
row.names(heat.matrix.n) <- dates.p

for (j in 1:length(dates.p)) {
  print(paste("j:",j))
  for (i in 1:length(dates.p)) {
    print("---------")
    print(paste("i:",i))
    column.select.i <- paste("d",dates.p[j],sep = "")
    column.select.j <- paste("d",dates.p[i],sep = "")
    print(paste(column.select.i,"interseccion con",column.select.j,":"))
    coinciden <- nrow(listings.total %>% filter((!!sym(column.select.i))== 1 & (!!sym(column.select.j)) == 1) )
    total <- nrow( listings.total %>% filter((!!sym(column.select.i))== 1) )
    percent <- round(coinciden/total*100, digits = 0)
    print(paste("elementos que coinciden ",coinciden," divididos por ",total," (",column.select.i,")",sep=""))
    print(paste("sale:",percent,"%"))
    # print(coinciden,total,percent)
    print(paste("mete dato en ","[j:",j,", i:",i,"]",sep = ""))
    heat.matrix.n[j,i] <- percent
  }
}

write.csv(heat.matrix.n, file="temp/heat.matrix.n.csv")

# basic heatmap
image(as.matrix(heat.matrix.n), xlab = 'Matrix rows', ylab = 'Matrix columns', axes = F)

# add id to column
heat.matrix.n$id <- colnames(heat.matrix.n)
# melt, from wide to long format
heat.matrix.n.m.p <- melt(heat.matrix.n)
# heat.matrix.m <- melt(heat.matrix,variable.name = "sample",value.name = "other",id="id")
ggplot(heat.matrix.n.m.p , aes(x = id, y = variable, fill = value)) + geom_tile()

# png(filename=paste("images/airbnb/evolucion/heat-map-coincidencias-madrid-insideairbnb-02-normalizada-text.png", sep = ""),width = 1200,height = 1200)
png(filename=paste("images/airbnb/evolucion/heat-map-coincidencias-madrid-insideairbnb-02-normalizada-text-pisos-completos.png", sep = ""),width = 1200,height = 1200)
ggplot(heat.matrix.n.m.p , aes(x = id, y = variable, fill = value)) +
  geom_tile() +
  geom_text(aes(label=value),size=6) +
  coord_equal() +
  scale_fill_gradientn(colours = rev(hm.palette(100))) +
  theme_minimal(base_size = 28) +
  theme(
    # panel.grid.minor.x = element_blank(), 
    # panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 90, vjust = 0.4)
  ) +
  labs(title = "Porcentaje de anuncios coincidentes en bases de datos de InsideAirbnb",
       subtitle = "Pisos completos. 2016-2018",
       y = "fechas",
       x = "fechas",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb")
dev.off()

# Plots coincidencias normalizadas

# fechas sin parsear -----
# png(filename=paste("images/airbnb/evolucion/lineas-coincidencias-madrid-insideairbnb-01-normalizada.png", sep = ""),width = 1400,height = 600)
# ggplot(heat.matrix.n.m.p , aes(x = id, y = value, group = variable,color=variable)) +
#   geom_line() +
#   geom_point(size=0.5) +
#   scale_fill_manual(values = getPalette(colourCount)) +
#   theme_minimal(base_size = 25) +
#   theme(
#     # panel.grid.minor.x = element_blank(), 
#     # panel.grid.major.x = element_blank(),
#     legend.position = "right",
#     axis.text.x = element_text(angle = 90, vjust = 0.4)
#   ) +
#   labs(title = "Porcentaje de anuncios coincidentes en bases de datos de InsideAirbnb",
#        subtitle = "Cada línea es una base de datos. 2016-2018",
#        y = "% coincidencia",
#        x = "fechas",
#        caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb")
# dev.off()

# converts to date -----
heat.matrix.n.m.p$date <- as.Date(heat.matrix.n.m.p$variable, "%Y-%m-%d")
# heat.matrix.n.m.p$id <- as.factor(heat.matrix.n.m.p$id)

# png(filename=paste("images/airbnb/evolucion/lineas-coincidencias-madrid-insideairbnb-02-normalizada.png", sep = ""),width = 1400,height = 600)
png(filename=paste("images/airbnb/evolucion/lineas-coincidencias-madrid-insideairbnb-02-normalizad-pisos-completos.png", sep = ""),width = 1400,height = 600)
# heat.matrix.n.m.p %>% filter(id=="180514") %>%
# ggplot(aes(x = date, y = value)) +
ggplot(heat.matrix.n.m.p, aes(x = date, y = value, group = id,color=variable)) +
  geom_line() +
  geom_point(size=1) +
  scale_fill_manual(values = getPalette(colourCount)) +
  scale_x_date(date_breaks = "1 month",date_labels = "%m-%Y") +
  # geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  # annotate("text",x=as.Date("2018-05-31"),y=40,label="acuerdo",color="#000000",size=4) +
  theme_minimal(base_size = 25) +
  theme(
    panel.grid.minor.x = element_blank(),
    # panel.grid.major.x = element_blank(),
    legend.position = "right",
    axis.text.x = element_text(angle = 90, vjust = 0.4)
  ) +
  labs(title = "Porcentaje de anuncios coincidentes en bases de datos de InsideAirbnb",
       subtitle = "Pisos completos. Cada línea es una base de datos. 2016-2018",
       y = "% coincidencia",
       x = "fechas",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb")
dev.off()

# Líneas: coincidencia en % de anuncios de pisos completos entre bases de datos de Airbnb --------------------
png(filename=paste("images/airbnb/evolucion/lineas-coincidencias-madrid-insideairbnb-03-normalizad-pisos-completos.png", sep = ""),width = 1000,height = 500)
ggplot() + 
  # lines
  geom_line(data=heat.matrix.n.m.p, aes(x = date, y = value, group = id),color="#bbbbbb") +
  # line destacada
  # geom_line(data=filter(heat.matrix.n.m.p,id=="180514"), aes(x = date, y = value, group =id),color="#ddbbbb",size=2) +
  # line destacada
  # geom_line(data=filter(heat.matrix.n.m.p,id=="180818"), aes(x = date, y = value, group = id),color="#bbddbb",size=2) +
  # points
  geom_point(data=heat.matrix.n.m.p, aes(x = date, y = value), size=1,color="#bbbbbb") +
  # destaca puntos mayo
  # geom_point(data=filter(heat.matrix.n.m.p,id=="180514"), aes(x = date, y = value), size=2,color="#bb9999") +
  # mete % en cada punto de mayo
  # geom_text(data=filter(heat.matrix.n.m.p,id=="180514" & date > as.Date("2018-01-01") & date < as.Date("2018-09-01")),
  #           aes(x = date+10, y = value+3,label=paste(value,"%",sep="")),size=5,family = "Roboto Condensed")+
  # fechas de scrapings
  geom_text(data=filter(heat.matrix.n.m.p, value ==100 ),
            aes(x = date, y = 41,label=date),
            size=5,family = "Roboto Condensed",angle = 90,color ="#333333" )+
  # colors
  scale_fill_manual(values = getPalette(colourCount)) +
  # scale
  scale_x_date(date_breaks = "2 month",date_labels = "%m-%Y") +
  # anotations
  # geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  # annotate("text",x=as.Date("2018-05-26"),y=57,label="acuerdo",color="#000000",size=5,hjust = 1,family = "Roboto Condensed") +
  # annotate("text",x=as.Date("2018-06-1"),y=87,label="El 29% desapareció",color="#000000",size=5,hjust=0,family = "Roboto Condensed") +
  # nota 71%
  # annotate(geom = "text", x = as.Date("2018-01-1"), y = 55, label = "El 71% de los anuncios de mayo seguía en junio", 
  #          family = "Roboto Condensed", hjust = 1,size=6) +
  # geom_curve(aes(x = as.Date("2018-01-1"), y = 55, xend = as.Date("2018-06-6"), yend = 70.5), 
  #            color="#333333", data =df,  curvature = 0.2, arrow = arrow(length = unit(0.03, "npc"))
  #            ) +
  # nota fecha de scraping
  # annotate(geom = "text", x = as.Date("2018-01-1"), y = 103, label = "Fecha del scraping de mayo 2018", 
  #          family = "Roboto Condensed", hjust = 1,size=6) +
  # geom_curve(aes(x = as.Date("2018-01-1"), y = 100, xend = as.Date("2018-05-10"), yend = 100), 
  #            color="#333333", data =df,  curvature = 0.2, arrow = arrow(length = unit(0.03, "npc"))
  # ) +
  # theme
  theme_minimal(base_family = "Roboto Condensed",base_size = 25) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major = element_line(size=0.6),
    panel.grid.major.x = element_blank(),
    # panel.grid.major.x = element_blank(),
    legend.position = "right",
    axis.text.x = element_blank()
    # axis.text.x = element_text(angle = 90, vjust = 0.4)
  ) +
  labs(title = "Porcentaje de pisos completos de Airbnb entre scrapings de InsideAirbnb",
       subtitle = "Cada línea es un scraping. 2016-2018. Madrid",
       y = "% coincidencia entre scrapings",
       x = "fecha",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb")
dev.off()

# Explicación 01 -------
heat.matrix.m.p$date <- as.Date( paste(20,as.character(heat.matrix.n.m.p$variable),sep=""), "%Y%m%d")

png(filename=paste("images/airbnb/evolucion/lineas-coincidencias-madrid-insideairbnb-03-normalizad-pisos-completos_01.png", sep = ""),width = 1000,height = 500)
ggplot(heat.matrix.m.p , aes(x = date, y = value, group = id),fill="#bbbbbb") +
  # geom_line() +
  # geom_point(size=0.5) +
  # geom_text(aes(label=value),size=3,color="#000000") +
  # geom_line(heat.matrix.m.p , aes(x = id, y = value, group = variable)) +
  # line destacada
  geom_line(data=filter(heat.matrix.m.p,id=="180514"), aes(x = date, y = value, group =id),color="#ddbbbb",size=2) +
  # line destacada
  # geom_line(data=filter(heat.matrix.n.m.p,id=="180818"), aes(x = date, y = value, group = id),color="#bbddbb",size=2) +
  # points
  # geom_point(data=heat.matrix.n.m.p, aes(x = date, y = value), size=1,color="#bbbbbb") +
  # destaca puntos mayo
  geom_point(data=filter(heat.matrix.m.p,id=="180514"), aes(x = date, y = value), size=2,color="#bb9999") +
  # mete n en cada punto de mayo
  geom_text( data=filter(heat.matrix.m.p, id=="180514"),
            aes(x = date, 
                y = value+100,
                label=value,
                family = "Roboto Condensed"),size=4 )+
  # fechas de scrapings
  # geom_text(data=filter(heat.matrix.n.m.p, value ==100 ),
            # aes(x = date, y = 41,label=date),
            # size=5,family = "Roboto Condensed",angle = 90,color ="#333333" )+
  # colors
  scale_fill_manual(values = getPalette(colourCount)) +
  # scale
  # scale_x_date(date_breaks = "2 month",date_labels = "%m-%Y") +
  # anotations
  # geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  # annotate("text",x=as.Date("2018-05-26"),y=57,label="acuerdo",color="#000000",size=5,hjust = 1,family = "Roboto Condensed") +
  # annotate("text",x=as.Date("2018-06-1"),y=87,label="El 29% desapareció",color="#000000",size=5,hjust=0,family = "Roboto Condensed") +
  # nota 71%
  # annotate(geom = "text", x = as.Date("2018-01-1"), y = 55, label = "El 71% de los anuncios de mayo seguía en junio", 
  #          family = "Roboto Condensed", hjust = 1,size=6) +
  # geom_curve(aes(x = as.Date("2018-01-1"), y = 55, xend = as.Date("2018-06-6"), yend = 70.5), 
  #            color="#333333", data =df,  curvature = 0.2, arrow = arrow(length = unit(0.03, "npc"))
  # ) +
  # nota fecha de scraping
  # annotate(geom = "text", x = as.Date("2018-01-1"), y = 103, label = "Fecha del scraping de mayo 2018", 
  #          family = "Roboto Condensed", hjust = 1,size=6) +
  # geom_curve(aes(x = as.Date("2018-01-1"), y = 100, xend = as.Date("2018-05-10"), yend = 100), 
  #            color="#333333", data =df,  curvature = 0.2, arrow = arrow(length = unit(0.03, "npc"))
  # ) +
  # theme
  theme_minimal(base_family = "Roboto Condensed",base_size = 25) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major = element_line(size=0.6),
    panel.grid.major.x = element_blank(),
    # panel.grid.major.x = element_blank(),
    legend.position = "right",
    axis.text.x = element_blank()
    # axis.text.x = element_text(angle = 90, vjust = 0.4)
  ) +
  labs(title = "Número de pisos completos coincidentes de Airbnb entre scrapings de InsideAirbnb",
       subtitle = "Mayo 2018. Madrid",
       y = "% coincidencia entre scrapings",
       x = "fecha",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb")
dev.off()

# Explicación 02: Líneas: coincidencia en % de anuncios de pisos completos entre bases de datos de Airbnb explicación--------------------
png(filename=paste("images/airbnb/evolucion/lineas-coincidencias-madrid-insideairbnb-03-normalizad-pisos-completos_02.png", sep = ""),width = 1000,height = 500)
ggplot() + 
  # lines
  # geom_line(data=heat.matrix.n.m.p, aes(x = date, y = value, group = id),color="#bbbbbb") +
  # line destacada
  geom_line(data=filter(heat.matrix.n.m.p,id=="180514"), aes(x = date, y = value, group =id),color="#ddbbbb",size=2) +
  # line destacada
  # geom_line(data=filter(heat.matrix.n.m.p,id=="180818"), aes(x = date, y = value, group = id),color="#bbddbb",size=2) +
  # points
  # geom_point(data=heat.matrix.n.m.p, aes(x = date, y = value), size=1,color="#bbbbbb") +
  # destaca puntos mayo
  geom_point(data=filter(heat.matrix.n.m.p,id=="180514"), aes(x = date, y = value), size=2,color="#bb9999") +
  # mete % en cada punto de mayo
  geom_text(data=filter(heat.matrix.n.m.p,id=="180514" & date > as.Date("2018-01-01") & date < as.Date("2018-09-01")),
            aes(x = date+10, y = value+3,label=paste(value,"%",sep="")),size=5,family = "Roboto Condensed")+
  # fechas de scrapings
  geom_text(data=filter(heat.matrix.n.m.p, value ==100 ),
            aes(x = date, y = 41,label=date),
            size=5,family = "Roboto Condensed",angle = 90,color ="#333333" )+
  # colors
  scale_fill_manual(values = getPalette(colourCount)) +
  # scale
  scale_x_date(date_breaks = "2 month",date_labels = "%m-%Y") +
  # anotations
  geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  # annotate("text",x=as.Date("2018-05-26"),y=57,label="acuerdo",color="#000000",size=5,hjust = 1,family = "Roboto Condensed") +
  # annotate("text",x=as.Date("2018-06-1"),y=87,label="El 29% desapareció",color="#000000",size=5,hjust=0,family = "Roboto Condensed") +
  # nota 71%
  annotate(geom = "text", x = as.Date("2018-01-1"), y = 55, label = "El 71% de los anuncios de mayo seguía en junio", 
           family = "Roboto Condensed", hjust = 1,size=6) +
  geom_curve(aes(x = as.Date("2018-01-1"), y = 55, xend = as.Date("2018-06-6"), yend = 70.5), 
             color="#333333", data =df,  curvature = 0.2, arrow = arrow(length = unit(0.03, "npc"))
  ) +
  # nota fecha de scraping
  annotate(geom = "text", x = as.Date("2018-01-1"), y = 103, label = "Fecha del scraping de mayo 2018", 
           family = "Roboto Condensed", hjust = 1,size=6) +
  geom_curve(aes(x = as.Date("2018-01-1"), y = 100, xend = as.Date("2018-05-10"), yend = 100), 
             color="#333333", data =df,  curvature = 0.2, arrow = arrow(length = unit(0.03, "npc"))
  ) +
  # theme
  theme_minimal(base_family = "Roboto Condensed",base_size = 25) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major = element_line(size=0.6),
    panel.grid.major.x = element_blank(),
    # panel.grid.major.x = element_blank(),
    legend.position = "right",
    axis.text.x = element_blank()
    # axis.text.x = element_text(angle = 90, vjust = 0.4)
  ) +
  labs(title = "Porcentaje de pisos completos de Airbnb entre scrapings de InsideAirbnb",
       subtitle = "Mayo 2018. Madrid",
       y = "% coincidencia entre scrapings",
       x = "fecha",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb")
dev.off()

