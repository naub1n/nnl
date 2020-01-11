#' Create points along SpatialLinesDataFrame
#'
#' @param l a SpatialLinesDataFrame
#' @param id_l default NULL. A character string. If NULL, row.names is use. If set, it define column name used.
#' @param id_p default "ID_PTS". A character string. If set, it define column name of unique points IDs.
#' @param step defautl 5. An integer. Define the distance between two points along the line in meters.
#' @param ncores default NULL. An integer. If NULL all CPU cores - 1 are used. If set, define number of CPU cores used.
#' @param p default TRUE. Logical. Define if function use multithreading.
#'
#' @return a SpatialPointsDataFrame
#'
#' @importFrom methods slot
#' @import parallel
#' @import doParallel
#' @import sp
#'
#' @export
#'
#' @examples
#'
#' # return lol
create_pts <- function(l, id_l = "ID_LINES", id_p = "ID_PTS", step = 5, ncores = NULL, p = T){
  if(p) {
    #explicite %dopar% function
    `%dopar%` <- foreach::`%dopar%`
    #if ncores is null, all cores are used
    if(is.null(ncores)) ncores <- parallel::detectCores() - 1
    #creation of CoreCluster
    cl <- parallel::makeCluster(ncores)
    #Stop connection if exit function
    on.exit(parallel::stopCluster(cl))
    #register the parallel backend
    doParallel::registerDoParallel(cl)
    #split geometry with cores number
    #split_l <- split.data.frame(l, 1:ncores)
    split_l <- split(l, rep(1:ncores, length.out = nrow(l), each = ceiling(nrow(l)/ncores)))
    #explicite i
    i <- integer()
    #points creation
    lst_p <- foreach::foreach (splited = split_l) %dopar% {
      sapply(methods::slot(splited,"lines"), function(x) sp::spsample(x, n = (sp::LinesLength(x) / step), type = "regular"))
    }
    #stop connections
    #parallel::stopCluster(cl)
    #merge result
    lst_p <- do.call(c, lst_p)
  } else {
    lst_p <- sapply(methods::slot(l,"lines"), function(x) sp::spsample(x, n = (sp::LinesLength(x) / step), type = "regular"))
  }
  #define default IDs for each line
  if(id_l == "ID_LINES") l$ID_LINES <- paste0("Line_",row.names(l))
  #Create ID vector for each SpatiaLine
  ids <- l[[id_l]]
  #Count points for each element (SpatialPoint) in SpatialPoint List (number of element = number of lines in the SpatialLinesDataFrame)
  npts <- sapply(lst_p, function(i) ifelse(is.null(i),0,nrow(i@coords)))
  #Repeat IDs of SpatialLine as many times as there are points on this SpatialLine
  pt_id <- rep(ids, npts)
  #Delete Null elements fo SpatialPoints list (Very important here)
  lst_p.filtered <- Filter(Negate(is.null),lst_p)
  #optimize memory
  rm(lst_p)
  gc()
  #Fuse list of SpatialPoints
  lst_p.merged <- do.call('rbind', lst_p.filtered)
  #optimize memory
  rm(lst_p.filtered)
  gc()
  #Create SpatialPointsDataFrame
  pts_linear <- sp::SpatialPointsDataFrame(lst_p.merged, data=data.frame(ID_LINEAR = pt_id, stringsAsFactors = F))
  #optimize memory
  rm(lst_p.merged)
  gc()
  #rename Lines column name
  colnames(pts_linear@data)[grep("ID_LINEAR", colnames(pts_linear@data))] <- id_l
  #Create an ID for each point
  pts_linear@data$ID_PT <- (seq.int(nrow(pts_linear)))
  #rename Points column name
  colnames(pts_linear@data)[grep("ID_PT", colnames(pts_linear@data))] <- id_p
  #return SpatialPointsDataFrame
  return(pts_linear)
}
