require(dplyr)
require(tidyr)
require(reshape2)

## Get the lon lat bounding rectangle for australia.
get_aust_bounds_lon_lat <- function() {
  australiaBox <- list(left=109.07, bottom=-50.96, top=6.23, right=189.40)
  australiaBox
}

## Read the stations file
read_stations <- function(stationsFile) {
  sdata <- read.csv(stationsFile, sep=" ", stringsAsFactors = FALSE, 
                    check.names=FALSE, row.names=NULL)
  sdata$name <- sapply(1:nrow(sdata), function(i) {
    row <- sdata[i,]
    text <- row[,5:ncol(sdata)]
    if (i < nrow(sdata) && TRUE %in% is.na(sdata[i+1,])) {
      row2 <- sdata[i+1,]
      row2 <- row2[,!is.na(row2)]
      text <- c(text, row2)
    }
    trimws(stringr::str_flatten(text, collapse=" "))
  })
  subset <- sdata[,1:4]
  colnames(subset) <- c("station", "lat", "lon", "elevation")
  subset$name <- sdata$name
  sdata <- subset  
  sdata <- sdata[order(sdata$name),]
  sdata <- sdata[complete.cases(sdata),]
  sdata
}

read_precip_file <- function(baseDir, file, filesep="/", colsep=" ") {
  MISSING_VAL <- 99999.9
  matches <- stringr::str_match(file, "\\.(\\d\\d\\d\\d\\d\\d+)\\.")
  
  match <- if (length(matches) > 1) {
    matches[2]
  } else "UNK"
  
  file <- paste(baseDir, file, sep=filesep)
  
  #df <- read.csv(pipe(paste("zcat", file)), skip=1, sep="[[:space:]]+", stringsAsFactors = FALSE, row.names = NULL)
  rows <- readLines(pipe(paste("zcat", file)))
  rows <- strsplit(rows, "[[:space:]]+")
  rows <- rows[2:length(rows)]
  dates <- sapply(rows, function(row) row[1])
  precip <- sapply(rows, function(row) row[2])
  df <- data.frame(date=dates, precip=as.numeric(precip))
  
  station <- rep(match, nrow(df))
  df$station <- station
  df$date <- as.POSIXct(strptime(df$date,"%Y%m%d"))
  df <- df[df$precip < MISSING_VAL,]
  df
}

## Load the readings from the stations data.
read_data <- function(baseDir, filesep="/", colsep=" ", dateCol="date", names=c("station", "date", "precip")) {
  MISSING_VAL <- 99999.9
  files <- list.files(baseDir)
  data <- lapply(files, function(file) read_precip_file(baseDir,file))
  data1 <- data[[1]]
  
  for(i in 2:length(data)) {
    data1 <- rbind(data1, data[[i]])  
  }
  
  data1
}

# create a wide table format, if there are multiple examples per date for a single station those multiple examples
# will be aggrgated as the mean
mergeStations <- function(stations, data) {
  allData <- data %>% inner_join(stations, by="station")
  allWide <- dcast(allData, date ~ station, value.var = "precip", fun.aggregate=mean)
  list(
    allData=allData,
    allWide=allWide
  )
}

slidingMatrix <- function(sequence, p=1) {
  sequence <- sequence[!is.nan(sequence)]
  # note n' = n - p + 1
  n1 <- length(sequence)
  # new number of rows
  n2 <- length(sequence) - p + 1
  nextSeq <- sapply(1:n2, function(i) {
    k <- i
    if (k > p) {
      k <- k - p + 1
    }
    sequence[k]
  })
  m <- matrix(nextSeq, nrow=n2, ncol=p, byrow=TRUE)
  as.matrix(m)
}

