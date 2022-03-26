readSL <- function(subDir='2021-12-14_12-20-38') {
  options(warn=-1)
  unzip(paste('./SensorLogger/', subDir, '.zip', sep=''), exdir=paste('./SensorLogger/', subDir, sep=''))
  file.list <- dir(paste('./SensorLogger/', subDir, sep=''))
  csv.list <- file.list[grep('csv', file.list)]
  
  sensor.list <- list()
  
  for(i in 1:length(csv.list)) {
    csv.file <- paste('./SensorLogger/', subDir, '/', csv.list[i], sep='')
    sensor.list[[i]] <- read.csv(csv.file)   
  }
  names(sensor.list) <- gsub('.csv', '', csv.list)
  options(warn=0)
  sensor.list <- sensor.list[c(match('Metadata', names(sensor.list)),
                               setdiff(c(1:length(sensor.list)), 
                                       match('Metadata', names(sensor.list))))]
  
  
  t.digits <- max(nchar(round(1000/as.numeric(unlist(strsplit(sensor.list$Metadata$sampleRateMs, '|', fixed=T))))))
  options(digits.secs=t.digits)    
  for(i in 2:length(sensor.list)) {
    sensor.list[[i]]$time <- as.POSIXct(sensor.list[[i]]$time/1000000000, 
                                        origin='1970-01-01 00:00:00', 
                                        tz='UTC')
  }
  
  sensor.list$Magnetometer$bearing <- -atan2(sensor.list$Magnetometer$x, sensor.list$Magnetometer$y)*(180/pi)
  sensor.list$Magnetometer$bearing[which(sensor.list$Magnetometer$bearing<0)] <- 
    sensor.list$Magnetometer$bearing[which(sensor.list$Magnetometer$bearing<0)] + 360
  
  angle <- sensor.list$Magnetometer$bearing-mean(sensor.list$Location$bearing[-1])
  angle[which(angle>180)] <- angle[which(angle>180)]-360
  pitch <- sensor.list$Orientation$pitch
  sensor.list$sighting <- list(angle=data.frame(time=sensor.list$Magnetometer$time,
                                                angle=angle), 
                               pitch=data.frame(time=sensor.list$Orientation$time,
                                                pitch=pitch))
  unlink(paste('./SensorLogger/', subDir, sep=''), recursive = TRUE)
  sensor.list
}



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
