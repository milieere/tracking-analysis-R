#Load the data and clean them, for each replicate (discard first frame etc)
loadCleanData <- function(dir, short, middle, streamend){
  
  library(plyr)
  library(dplyr)
  library(tidyr)
  
  list <- list.files(dir)
  
  dataset <- data.frame()
  
  for (i in 1:length(list)){
    if (length(list)==1){
      temp_file <- read.csv(file=list[i], header=TRUE, sep=',', na.strings='NA', stringsAsFactors = FALSE)
      temp_file <- cleanData(temp_file, short, middle, streamend)
      dataset <- temp_file
    } else {
      temp_file <- read.csv(file=list[i], header=TRUE, sep=',', na.strings='NA', stringsAsFactors = FALSE)
      temp_file$rep_no <- c(rep(substring(list[i], 1, 5), nrow(temp_file)))
      temp_file <- cleanData(temp_file, short, middle, streamend)
      dataset <- bind_rows(dataset, temp_file)
    }
  }
  return(dataset)
}


cleanData <- function(results, short, middle, streamend) {
  library(plyr)
  library(dplyr)
  library(tidyr)
  #Clean the data. 1. Discard tracks from first and last frame
  removeTracks <- subset(results$TRACK_ID, results$FRAME == 0)
  #removeTracks <- append(removeTracks, subset(results$TRACK_ID, results$FRAME == results$FRAME[nrow(results)]))
  filtered <- results[!(results$TRACK_ID %in% removeTracks),]
  #Get rid of None track ID
  filtered <- filtered %>% filter(filtered$TRACK_ID != 'None')
  
  #Select tracks present in frame 1-40
  tracks_1_40 <- subset(filtered$TRACK_ID, between(filtered$FRAME,1,40))
  filtered <- filtered[(filtered$TRACK_ID %in% tracks_1_40),]
  
  #Order by track and frame
  filtered <- filtered[
    order( filtered[,3], filtered[,9] ),
  ]
  #Put the rownames into order
  rownames(filtered) <- 1 : length(rownames(filtered))
  #Get length of tracks starting at frame 60
  counts_1fps <- count(filtered %>% filter(FRAME > streamend) %>% group_by(TRACK_ID, rep_no))
  counts_20fps <- count(filtered %>% filter(FRAME < streamend) %>% group_by(TRACK_ID, rep_no))
  counts_both <- counts_1fps %>% full_join(counts_20fps, by=c("TRACK_ID","rep_no"))
  counts_both <- counts_both %>% rename(onefps = n.x)
  counts_both <- counts_both %>% rename(twentyfps = n.y)
  counts_both[is.na(counts_both)] = 0
  counts_both$twentyfps <- counts_both$twentyfps*0.05
  counts_both$duration_sec <-  counts_both$twentyfps + counts_both$onefps
  
  #JOins the counts so the data
  filtered <- filtered %>% full_join(counts_both, by=c("TRACK_ID","rep_no"))
  
  #Computes lengths of tracks
  #filtered <- ddply(filtered,.(TRACK_ID),transform,length_frames=length(SNR))
  
  #Adds category for events
  filtered$length_category <- mapply(function(x) if (x <= short) 'short' else if (x <= middle) 'middle' else 'long', filtered$duration_sec)
  
  #Normalize the mean intensity values
  filtered <- filtered %>% group_by(TRACK_ID, rep_no) %>% dplyr::mutate(maximum=max(MEAN_INTENSITY))
  filtered <- filtered %>% group_by(TRACK_ID, rep_no) %>% dplyr::mutate(timepoint=1:n())
  filtered$normalized_mean_int <- filtered$MEAN_INTENSITY / filtered$maximum

  return(filtered)
}


#Plots distributions etc
plotDistr <- function(clean, short, middle){
  library(ggplot2)
  library(ggpubr)
  
  summary <- clean %>% 
    group_by(TRACK_ID, rep_no) %>% 
    summarize(mean_intensity = mean(MEAN_INTENSITY), mean_norm_intensity = mean(normalized_mean_int), sd = sd(normalized_mean_int), length=mean(duration_sec))
  
  #Categorize short and long
  fun1 <- function(x) if (x <= short) 'short' else if (x <= middle) 'middle' else 'long'
  summary$size <- mapply(fun1, summary$length)
  
  print(summary)
  
  #Distribution on mean intensities to check where approx cut off the extreme high values
  norm_intensity <- ggplot(summary, aes(x = mean_norm_intensity)) + geom_density(aes(fill = size), alpha = 0.4) + geom_vline(aes(xintercept = mean(mean_norm_intensity)), 
                                                                                                                   linetype = "dashed", size = 0.6)
  
  intensity <- ggplot(summary, aes(x = mean_intensity)) + geom_density(aes(fill = size), alpha = 0.4) + geom_vline(aes(xintercept = mean(mean_intensity)), 
                                                                                                                            linetype = "dashed", size = 0.6)
  
  duration <- ggplot(summary, aes(x = length)) + geom_density(aes(fill = rep_no), alpha = 0.4) +
    geom_vline(aes(xintercept = mean(length)), 
               linetype = "dashed", size = 0.6)
  
  #Correlation of mean int and length
  library("ggpubr")
  
  #cor <- ggscatter(summary, x = "mean_intensity", y = "length",  
                   #add = "reg.line", conf.int = TRUE, 
                   #cor.coef = TRUE, cor.method = "pearson",
                   #xlab = "MEAN_INTENSITY", ylab = "length_frames")
  
  
  plots <- ggarrange(intensity, norm_intensity, duration, ncol=2, nrow=2)
  return(plots)
}

plotNormMean <- function(data){
  library(dplyr)
  library(ggplot2)
  library(ggpubr)
  
  summary <- data %>% 
    filter(FRAME < 60) %>%
    group_by(length_category, FRAME) %>% 
    summarize(mean_intensity = mean(MEAN_INTENSITY), sd= sd(MEAN_INTENSITY))
  
  ribbonplot <- ggplot(summary, aes(FRAME, mean_intensity, group = length_category, color = length_category)) +
    geom_line() +
    geom_point() +
    geom_ribbon(aes(ymax = mean_intensity + sd, ymin = mean_intensity - sd),
                alpha = 0.15,
                fill = "grey70",
                colour=NA)
  
  summary1 <- data %>% 
    filter(FRAME < 60) %>%
    group_by(length_category, timepoint) %>% 
    summarize(mean_intensity = mean(MEAN_INTENSITY), sd= sd(MEAN_INTENSITY))
  
  ribbonplot1 <- ggplot(summary1, aes(timepoint, mean_intensity, group = length_category, color = length_category)) +
    geom_line() +
    geom_point() +
    geom_ribbon(aes(ymax = mean_intensity + sd, ymin = mean_intensity - sd),
                alpha = 0.15,
                fill = "grey70",
                colour=NA)
  
  plots <- ggarrange(ribbonplot, ribbonplot1, ncol = 2)
  return(plots)
}


plotData <- function(data){
  library(ggplot2)
  library(ggpubr)
  library(plyr)
  
  #Annotate tre tracks short <12 frames and long >12 frames
  counts_frames_tracks <- ddply(data, .(data$TRACK_ID), nrow)
  names(counts_frames_tracks) <- c("TRACK_ID", "LENGTH_FRAMES")
  fun1 <- function(x) if (x <= 12) 'short' else 'long'
  counts_frames_tracks$size <- mapply(fun1, counts_frames_tracks$LENGTH_FRAMES)
  
  #Plot the data
  distr_events <- ggplot(counts_frames_tracks, mapping = aes(x = LENGTH_FRAMES, fill = size, color=000000)) + geom_histogram(bins = 30) + scale_x_continuous(breaks = seq(0, 50, 5))
  intensity <- ggplot(data, mapping = aes(x = FRAME, y = MEAN_INTENSITY, color = TRACK_ID) ) + geom_line() + theme(legend.position = "none") 
  timeline <- ggplot(data, mapping = aes(x = FRAME, y = TRACK_ID, color = TRACK_ID) ) + geom_point() + theme(legend.position = "none") 
  plots <- ggarrange(timeline, distr_events, intensity, labels = c("Timeline", "Distribution of events", "Mean intensity"), font.label = list(size = 10, color = "black", family = NULL), ncol=2, nrow=2)
  return(plots)
}


#PCA
makePCA <- function(data, vars) {
  library(dplyr)
  
  data_pca <- list()
  
  for (var in 1:length(vars)){
    varname <- vars[var]
    column <- data[ , grepl( varname , names( data ) ) ]
    print(column)
    data_pca[[varname]] <- column
  }
  
  data_pca <- data.frame(Reduce(cbind, data_pca))
  colnames(data_pca) <- vars
  return(data_pca)
}
  
  data_pca <- tibble(TRACK_ID = data$TRACK_ID, QUALITY = data$QUALITY, NORM_MEAN_INT = data$normalized_mean_int, REP_NO = data$rep_no, LENGTH_CAT = data$length_category, TIMEPOINT = data$timepoint, SNR = data$SNR, CONTRAST = data$CONTRAST, EST_DIAMETER = data$ESTIMATED_DIAMETER)
  data_pca <- data_pca %>% filter(TIMEPOINT < 20)
  data.pca <- prcomp(data_pca[,c(2,3,7:9)], center = TRUE,scale. = TRUE)
  summary(data.pca)
  
  library(ggbiplot)
  pca <- ggbiplot(data.pca,ellipse=TRUE, groups=data_pca$TIMEPOINT)
  return(pca)
}

#' Calculates the Mean Squared Displacement for a trajectory
#'  
#' @param sx x axis positions along the trajectory
#' @param sy y axis positions along the trajectory
#' @param until how many points should be included in the Mean Squared Displacement curve
#' 
#' @return A numeric vector containing the values of the MSD
#' 
#' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
computeMSD <- function(sx,sy,until=4)
{
  msd.t <- rep(0,until)
  for (dt in 1:until)
  {
    displacement.x <- as.vector(na.omit(sx[(1+dt):length(sx)]) - sx[1:(length(sx)-dt)])
    displacement.y <- as.vector(na.omit(sy[(1+dt):length(sy)]) - sy[1:(length(sy)-dt)])
    sqrdispl <- (displacement.x^2 + displacement.y^2)
    msd.t[dt] <- mean(sqrdispl)
  }
  return(msd.t)
}



#' Get the names of the tracks
#' @param file results file from TrackMate in csv
#' 
#' @return a vector containing the track ids
getTrackNames <- function(file){
  return(tracknames <- c(file$TRACK_ID))
}


#' Get start and stop positions for MSD calculations
#' @param tracknames takes track names extracted with previous function
#' 
#' @return a list containing two vectors, one with start and other with end positions

getTrackIndices <- function(tracknames){
  indices <- c()
  ntracknames <- c(seq(1, length(tracknames), 1))
  var <- 0
  for (i in seq_along(ntracknames)){
    if (var != tracknames[i]){
      print(tracknames[i])
      index <- ntracknames[i]
      print(ntracknames[i])
      indices <- append(indices, index)
    }
    var <- tracknames[i]
  }
  start <- indices
  end <- indices[-1]-1
  return(list(start, end))
}

indices <- getTrackIndices(tracknames)

#' @param start takes start values from a list returned by getTrackIndices, i.e. list[[1]]
#' @param end takes end values from a list returned by getTrackIndices, i.e. list[[2]]
#' @param x takes the X coordinates from the results file i.e. filtered$POSITION_X
#' @param y takes the Y coordinates from the results file i.e. filtered$POSITION_Y
#' @param tracknames is taking the vector of the track names extracted by getTrackNames function
#' 
computeMSDs <- function(start, end, x, y, tracknames, fps, filename) {
  msds <- list()
  for (i in 1:length(end)){
    startpos <- start[i]
    endpos <- end[i]
    msd <- computeMSD(x[startpos:endpos], y[startpos:endpos], until = (endpos-startpos))
    track_id <- rep(tracknames[startpos], length(msd))
    rep_no <- rep(filename$rep_no[startpos], length(msd))
    timepoint <- seq(1, length(msd))
    data <- data.frame(msd, track_id, timepoint, length, length_category, rep_no)
    msds[[i]] <- data
  }
  return(msds)
}

#' Wrapper of all previous functions, returns dataframe with all msds and information needed for plotting

calcMSD <- function(filename, fps){
  tracknames <- getTrackNames(filename)
  startEnd <- getTrackIndices(tracknames)
  msds <- computeMSDs(startEnd[[1]], startEnd[[2]], filename$POSITION_X, filename$POSITION_Y, tracknames, fps, filename)
  library(dplyr)
  msd_calc <- bind_rows(msds, .id = "column_label")
  
  return(msd_calc)
}

cleanMsd <- function(msds, data, filter_length){
  data_length <- data.frame(TRACK_ID = data$TRACK_ID, length_cat = data$length_category, rep_no = data$rep_no)
  data_length <- data_length %>% dplyr::rename(track_id = TRACK_ID)
  msds_joined <- msds %>%inner_join(data_length, by=c('rep_no', 'track_id'))
  msds_joined <- msds_joined[-6]
  msds_joined <- distinct(msds_joined)
  msds_joined <- msds_joined %>% filter(length > filter_length)
  return(msds_joined)
}

data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  return(data_sum)
}

plotMsd <- function(msds){
  intensity <- ggplot(msds, mapping = aes(x = timepoint, y = msd, color = length_category.x, fill=TRACK_ID:location) ) + geom_line() + geom_point() + theme(legend.position = "none") 
  intensity
  
  
  summaryMsd <- data_summary(msds, varname="msd", 
                             groupnames=c('length_category.x', 'timepoint'))
  print(summaryMsd)
  
  ribbonplot <- ggplot(summaryMsd, aes(timepoint, mean, group = length_category.x, color = length_category.x)) +
    geom_line() +
    geom_point() +
    geom_ribbon(aes(ymax = mean + sd, ymin = mean - sd),
                alpha = 0.4,
                fill = "grey70",
                colour=NA)
  
  plots <- ggarrange(intensity, ribbonplot, nrow=2)
  return(plots)
}
