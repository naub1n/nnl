#' Find nearest point between two SpatialPointsDataFrame.
#'
#' @param pts_A a SpatialPointsDataFrame. Reference points.
#' @param pts_B a SpatialPointsDataFrame. Searched points.
#' @param id_p_A a character string. Indicates the IDs column name of pts_A.
#' @param id_p_B a character string. Default "ID_PTS_B". Indicates the IDs column name of pts_B.
#' @param ncores an integer. Number of CPU cores used. If NULL, all cores - 1 are used.
#' @param p a boolean. Define if function use multithreading. Default = TRUE.
#'
#' @return a data.table
#'
#' @import doParallel
#' @import SearchTrees
#' @importFrom sp coordinates
#' @importFrom data.table data.table
#'
#' @export
#'
#' @examples
#' #test
find_np <- function(pts_A, pts_B, id_p_A, id_p_B = "ID_PTS_B", ncores = NULL, p = T){
  if(p) {
    #explicite %dopar% function
    `%dopar%` <- foreach::`%dopar%`
    #if ncores is null, all cores are used
    if(is.null(ncores)) ncores <- parallel::detectCores() - 1
    #creation of CoreCluster
    cl <- parallel::makeCluster(ncores)
    #register the parallel backend
    doParallel::registerDoParallel(cl)
    #stop connections if exit function (error or not)
    on.exit(parallel::stopCluster(cl))
    #split geometry with cores number
    #split_p <- split.data.frame(pts_A, 1:ncores)
    split_p <- split(pts_A, rep(1:ncores, length.out = nrow(pts_A), each = ceiling(nrow(pts_A)/ncores)))
    #explicite i
    i <- integer()
    #find nearest point
    np <- foreach::foreach (i = 1:length(split_p)) %dopar% {
      #QuadTree must be declared in worker!
      tree <- SearchTrees::createTree(sp::coordinates(pts_B),dataType = "point")
      st <- SearchTrees::knnLookup(tree, newdat=sp::coordinates(split_p[[i]]), k=1)
      #convert in data frame
      st.df <- as.data.frame(st)
      #add IDs of points A
      st.df[, id_p_A] <- split_p[[i]]@data[, id_p_A]
      return(st.df)
    }
    #merge result
    np.df <- do.call(rbind, np)
  } else {
    tree <- SearchTrees::createTree(sp::coordinates(pts_B),dataType = "point")
    np <- SearchTrees::knnLookup(tree, newdat=sp::coordinates(pts_A), k=1)
    #convert in data frame
    np.df <- as.data.frame(np)
    #add IDs of points A
    np.df[, id_p_A] <- pts_A@data[, id_p_A]
  }
  #rename first column
  colnames(np.df)[1] <- id_p_B
  #convert in data table
  np.dt <- data.table::data.table(np.df, key = c(id_p_B, id_p_A))
  #return result
  return(np.dt)
}
