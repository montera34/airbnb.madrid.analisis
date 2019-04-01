
dates <- c("2019-03-08","2019-02-06","2019-01-14","2018-12-10","2018-11-07","2018-10-10","2018-09-11","2018-08-14","2018-07-10","2018-05-14","2018-04-12",
           "2018-01-17","2017-04-08","2017-03-06","2015-10-02","2015-09-04","2015-07-17")
# files_data <- c("listings.csv.gz", "calendar.csv.gz", "reviews.csv.gz")
files_vis <- c("listings.csv","reviews.csv")

for (i in 4:length(dates)) {
# for (i in 2:3) {
  dir.create(paste("data/original/airbnb/",dates[i],sep=""))
  dir.create(paste("data/original/airbnb/",dates[i],"/data",sep=""))
  dir.create(paste("data/original/airbnb/",dates[i],"/visualizations/",sep=""))
  for (j in 1:length(files_vis)) {
  download.file(paste("http://data.insideairbnb.com/spain/comunidad-de-madrid/madrid/",dates[i],"/visualisations/",files_vis[j],sep=""),
              destfile=paste("data/original/airbnb/",dates[i],"/visualizations/",files_vis[j],sep=""),
                             method = "wget")
  }
  # for (k in 1:length(files_data)) {
  # download.file(paste("http://data.insideairbnb.com/spain/comunidad-de-madrid/madrid/",dates[i],"/data/",files_data[k],sep=""),
  #               destfile=paste("data/original/airbnb/",dates[i],"/data/",files_data[j],sep=""),
  #               method = "wget")
  # }
}


