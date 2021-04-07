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
    length <- rep(endpos-startpos, length(msd))
    length_category <- mapply(function(x) if (x <= fps*4) 'short' else if (x <= fps*20) 'middle' else 'long', length)
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
  intensity <- ggplot(msds, mapping = aes(x = timepoint, y = msd, color = length_category, fill=track_id) ) + geom_line() + geom_point() + theme(legend.position = "none") 
  intensity
  
  
  summaryMsd <- data_summary(msds, varname="msd", 
                             groupnames=c('length_category', 'timepoint'))
  
  ribbonplot <- ggplot(summaryMsd, aes(timepoint, mean, group = length_category, color = length_category)) +
    geom_line() +
    geom_point() +
    geom_ribbon(aes(ymax = mean + sd, ymin = mean - sd),
                alpha = 0.4,
                fill = "grey70",
                colour=NA)
  
  plots <- ggarrange(intensity, ribbonplot, nrow=2)
  return(plots)
}

msds_final <- calcMSD(data, 1)
plots_msd <- plotMsd(msds_final)


