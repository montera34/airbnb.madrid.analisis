# Script to download InsideAirbnb data

# List dates
dates <- c("2019-03-08","2019-02-06","2019-01-14","2018-12-10","2018-11-07","2018-10-10","2018-09-11","2018-08-14","2018-07-10","2018-05-14","2018-04-12",
           "2018-01-17","2017-04-08","2017-03-06","2015-10-02","2015-09-04","2015-07-17")

# List files to download from each day
# Big files (complete data)
files_data <- c("listings.csv.gz", "calendar.csv.gz", "reviews.csv.gz")
# Simplified data
# files_vis <- c("listings.csv","reviews.csv")

# Loop through all the dates
for (i in 1:length(dates)) {
  print(i)
  
  # Run the first time to create the directories
  # dir.create(paste("data/original/airbnb/",dates[i],sep=""))
  # dir.create(paste("data/original/airbnb/",dates[i],"/data",sep=""))
  # dir.create(paste("data/original/airbnb/",dates[i],"/visualizations/",sep=""))
  
  # To download big files (uncomment if needed)
  # for (j in 1:length(files_vis)) {
  # download.file(paste("http://data.insideairbnb.com/spain/comunidad-de-madrid/madrid/",dates[i],"/visualisations/",files_vis[j],sep=""),
  #             destfile=paste("data/original/airbnb/",dates[i],"/visualizations/",files_vis[j],sep=""),
  #                            method = "wget")
  # }
  
  # To download simplfied data
  for (k in 1:length(files_data)) {
  download.file(paste("http://data.insideairbnb.com/spain/comunidad-de-madrid/madrid/",dates[i],"/data/",files_data[k],sep=""),
                destfile=paste("data/original/airbnb/",dates[i],"/data/",files_data[k],sep=""),
                method = "wget")
  }
}


