#' Cut a SpatialLinesDateFrame into a smaller segments
#'
#' @param l a SpatialLinesDataFrame.
#' @param id_l a character string. IDs Column name of SpatialLinesDataFrame.
#'
#' @return a SpatialLinesDataFrame
#'
#' @import sp
#' @importFrom methods slot
#'
#' @export
#'
#' @examples
cut_sldf <- function(l, id_l){
  #extract all lines coords
  coords <- lapply(methods::slot(l, "lines"), function(x) lapply(methods::slot(x, "Lines"), function(y) methods::slot(y, "coords")))
  #convert to data.frame
  coords.df <- lapply(coords, function(x) as.data.frame(x))
  #create Line in each coords list with coords pair
  Line.list <- lapply(coords.df, function(x) {
    newlist <- list()
    #create Line with row n coords and row n+1 coords. So last row is only use with row n-1
    for(i in 1:(nrow(x) - 1)){
      #coords pair
      linecoords <- rbind(c(x[i, "X1"],x[i, "X2"]),c(x[i+1, "X1"],x[i+1, "X2"]))
      #create Line with coords
      newline <- sp::Line(linecoords)
      #add Line to list
      newlist <- c(newlist, newline)
    }
    return(newlist)
  })

  #create Lines in each Line list with unique Line
  Lines.list <- lapply(Line.list, function(x){
    newlist <- list()
    for(i in 1:length(x)){
      #create Lines with one Line
      newLines <- sp::Lines(x[[i]], ID = i)
      #add Lines to list
      newlist <- c(newlist, newLines)
    }
    return(newlist)
  })
  #create vector with length of each Lines list
  n_lines <- unlist(lapply(Lines.list, length))
  #repeate original ID with length of each Lines list
  rep_id <- rep(l@data[, id_l], n_lines)
  #create unique list with all Lines
  Lines.all <- unlist(Lines.list)
  #create unique ID for each Lines = original ID + __i__ where i is an integer
  for(i in 1:length(Lines.all)){
    methods::slot(Lines.all[[i]], "ID") <- paste0(rep_id[i], "__", methods::slot(Lines.all[[i]], "ID"), "__")
  }
  #create SpatialLines
  sl <- sp::SpatialLines(Lines.all)
  #create data.frame which contains originals IDs
  #df <- as.data.frame(rep_id, stringsAsFactors = F)
  df <- as.data.frame(sapply(Lines.all, function(x) methods::slot(x, "ID")))
  #rename column with original column ID
  colnames(df)[1] <- id_l
  #create SpatialLinesDataFrame
  sldf <- sp::SpatialLinesDataFrame(sl, data = df, match.ID = F)
  #return result
  return(sldf)
}
