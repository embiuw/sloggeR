#' \code{readSL} Read data from single SensorLogger observation directory
#'
#' @param subDir name of the subdirectory containing data from a single observation instance
#' @return Returns a list with all sensor data, except for the voice recording
#' Also includes an added data frame with all relevant extracted sightings sensor information
#' @details To be added
#' @family Sightings survey data processing
#' @seealso \code{\link{getSightings}} to read all observations in a main directory,
#'   \code{\link{annotateSightings}} to manually extract information from voice recordings,
#'   \code{\link{estDist}} to estimate distance to sightings,
#' @author Martin Biuw
#' @example
#' sl <- readLSL()
#' @export


readSL <- function(subDir='2021-12-14_12-20-38') {
  options(warn=-1)
  unzip(paste(subDir, '.zip', sep=''), exdir=subDir)
  file.list <- dir(subDir)
  csv.list <- file.list[grep('csv', file.list)]

  sensor.list <- list()

  for(i in 1:length(csv.list)) {
    csv.file <- paste(subDir, '/', csv.list[i], sep='')
    sensor.list[[i]] <- try(read.csv(csv.file), silent=T)
  }
  names(sensor.list) <- gsub('.csv', '', csv.list)
  options(warn=0)
  sensor.list <- sensor.list[c(match('Metadata', names(sensor.list)),
                               setdiff(c(1:length(sensor.list)),
                                       match('Metadata', names(sensor.list))))]


  t.digits <- max(nchar(round(1000/as.numeric(unlist(strsplit(sensor.list$Metadata$sampleRateMs, '|', fixed=T))))))
  options(digits.secs=t.digits)
  for(i in 2:length(sensor.list)) {
    if(class(sensor.list[[i]])!='try-error') {
      sensor.list[[i]]$time <- as.POSIXct(sensor.list[[i]]$time/1000000000,
                                          origin='1970-01-01 00:00:00',
                                          tz='UTC')
    }
  }

  if(class(sensor.list$Magnetometer)!='try-error') {
    sensor.list$Magnetometer$bearing <- -atan2(sensor.list$Magnetometer$x, sensor.list$Magnetometer$y)*(180/pi)
    sensor.list$Magnetometer$bearing[which(sensor.list$Magnetometer$bearing<0)] <- sensor.list$Magnetometer$bearing[which(sensor.list$Magnetometer$bearing<0)] + 360
  }

  if(class(sensor.list$Magnetometer)!='try-error' & class(sensor.list$Location)!='try-error') {
    angle <- sensor.list$Magnetometer$bearing-mean(sensor.list$Location$bearing[-1])
    angle[which(angle>180)] <- angle[which(angle>180)]-360
    angle <- data.frame(time=sensor.list$Magnetometer$time,
                        angle=angle)
  } else {
    angle <- NA
  }

  if(class(sensor.list$Orientation)!='try-error') {
    pitch <- data.frame(time=sensor.list$Orientation$time, pitch=sensor.list$Orientation$pitch)
  }

  sensor.list$sighting <- list(angle=angle,
                               pitch=pitch)
  unlink(subDir, recursive = TRUE)
  sensor.list
}


#' \code{getSightings} Read all observations in a main directory
#'
#' @param mainDir name of the  main directory containing all observation data from a cruise
#' @return Returns a list with the sightings component from all observations
#' @details To be added
#' @family Sightings survey data processing
#' @seealso \code{\link{readSL}} to read single observation data in a subdirectory,
#'   \code{\link{annotateSightings}} to manually extract information from voice recordings,
#'   \code{\link{estDist}} to estimate distance to sightings,
#' @author Martin Biuw
#' @example
#' To be added
#' @export

getSightings <- function(mainDir='C:/Users/a5406/Documents/OneOcean/SensorLogger') {
  require(progress)
  oDir <- getwd()
  setwd(mainDir)
  subDirs <- dir()

  sightings <- list()
  pb <- progress_bar$new(total=length(subDirs))
  for(i in 1:length(subDirs)) {
    pb$tick()
    unzip(subDirs[i], exdir=gsub('.zip', '', subDirs[i]))
    sl <- try(readSL(gsub('.zip', '', subDirs[i])), silent=T)
    fs::dir_delete(gsub('.zip', '', subDirs[i]))
    if(class(sl)!='try-error') {
      sightings[[i]] <- sl$sighting
    } else {
      sightings[[i]] <- sl
    }
  }
  names(sightings) <- gsub('.zip', '', subDirs)
  sightings
}

#' \code{annotateSightings} Extract information from voice recording
#'
#' @param mainDir name of the  main directory containing all observation data from a cruise
#' @return To be added
#' @details To be added
#' @family Sightings survey data processing
#' @seealso \code{\link{readSL}} to read single observation data in a subdirectory,
#'   \code{\link{getSightings}} to read all observations in a main directory,
#'   \code{\link{estDist}} to estimate distance to sightings,
#' @author Martin Biuw
#' @example
#' To be added
#' @export
#'
annotateSightings <- function(mainDir='C:/Users/a5406/Documents/OneOcean/SensorLogger') {
  require(av)
  oDir <- getwd()
  setwd(mainDir)
  subDirs <- dir()

  for(i in 1:length(subDirs)) {
    exDir <- gsub('.zip', '', subDirs[i])
    unzip(subDirs[i], exdir=exDir)
    av_spectrogram_video(audio=paste('./', exDir, '/Microphone.m4a', sep=''),
                        output=paste('./', exDir, '/Recording.mp4', sep=''))
    utils::browseURL(paste('./', exDir, '/Microphone.m4a', sep=''))

    setwd(paste('./', exDir, sep=''))
    dir.create('./www')
    file.copy('Microphone.m4a', './www/Microphone.m4a')
  }
}
