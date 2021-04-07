library(ggplot2)
library(dplyr)

msds_plot <- ggplot(msd_calc, aes(x=timepoint, y=msd, color=track_id)) + geom_point() + geom_line()

#Calculate MSDs for all tracks individually
#Get the names of the tracks
getTrackNames <- function(file){
  return(tracknames <- c(file$TRACK_ID))
}

#' Get start and stop positions for MSD calculations
#' @param tracknames takes track names extracted with previous function

getTrackIndices <- function(tracknames){
  indices <- c()
  var <- 0
  for (track in tracknames){
    if (var != track){
      indexes <- c(which(tracknames == track))
      indices <- append(indices, indexes[1])
      start <- indices
      start[2] <- start[2]-1
      end <- indices
      end <- end[-1]
      end[seq(1, length(end), 2)] <- end[seq(1, length(end), 2)]+1
      end[seq(2, length(end), 2)] <- end[seq(2, length(end), 2)]-1
    }
    var <- track
  }
  return(start, end)
}

#RUn the function
indexes <- getTrackIndices(tracknames)

#Split the vector in two for start and end positions
nth_element <- function(vector, starting_position, n) { 
  vector[seq(starting_position, length(vector), n)] 
}

nth_element(indexes, 1, 2)*2
end <- nth_element(indexes, 2, 2)
end <- end-1

start1 <- end+1
end1 <- nth_element(start, 2, 1)

x <- filtered$POSITION_X
y <- filtered$POSITION_Y
trackid <- filtered$TRACK_ID

msds <- list()

#Function to cpmute MSDs for the subsets
computeMSDs <- function(start, end, x, y, trackid) {
  for (i in 1:length(end)){
    startpos <- start[i]
    endpos <- end[i]
    msd <- computeMSD(x[startpos:endpos], y[startpos:endpos], until = (endpos-startpos))
    track_id <- rep(trackid[startpos], length(msd))
    timepoint <- seq(1, length(msd))
    length <- rep(endpos-startpos, length(msd))
    length_category <- mapply(fun1, length)
    data <- data.frame(msd, track_id, timepoint, length, length_category)
    msds[[i]] <- data
  }
  return(msds)
}


#Execute function for both to get all tracks
msds <- computeMSDs(start, end, x, y, trackid)
msds2 <- computeMSDs(start1, end1, x, y, trackid)

#Join 
msd_calc <- bind_rows(msds, .id = "column_label")
msd_calc1 <- bind_rows(msds2, .id = "column_label")
msd_calc <- bind_rows(msd_calc, msd_calc1)

#Plot
msds_plot <- ggplot(msd_calc, aes(x=timepoint, y=msd, color=length_category, fill=track_id)) + geom_point() + geom_line()
msds_plot
