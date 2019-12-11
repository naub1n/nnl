#' Complete and unique function to extrapolate a SpatialLinesDataFrame A to a SpatialLinesDataFrame B
#'
#' @param l_A a SpatialLinesDataFrame. Reference lines.
#' @param l_B a SpatialLinesDataFrame. Searched lines.
#' @param id_l_A a character string. IDs Column name of reference SpatialLinesDataFrame (Lines A)
#' @param id_l_B a character string. IDs Column name of reference SpatialLinesDataFrame (Lines B)
#' @param id_p_A a character string. Indicates the IDs column name of points created to lines A. Default : "ID_PTS_A"
#' @param id_p_B a character string. Indicates the IDs column name of points created to lines B. Default : "ID_PTS_B"
#' @param step an integer. Define the distance between two points along the line in meters. Defautl = 5.
#' @param rate an integer. Between 0 and 100. Define lower limit to select line B. Rate between number of points B on Lines A and totalpoints on Line B.
#' @param p a boolean. Define if function use multithreading. Default = TRUE.
#' @param ncores an integer. Number of CPU cores used. If NULL, all cores - 1 are used.
#'
#' @return a SpatialLinesDataFrame.
#'
#' @export
#'
#' @examples
nnl <- function(l_A, l_B, id_l_A, id_l_B, id_p_A = "ID_PTS_A", id_p_B = "ID_PTS_B", step = 5, rate = 45, p = T, ncores = NULL){
  #create points along lines A
  points_A <- create_pts(l = l_A, id_l = id_l_A, id_p = id_p_A, step = step, p = p, ncores = ncores)
  #create points along lines B
  points_B <- create_pts(l = l_B, id_l = id_l_B, id_p = id_p_B, step = step, p = p, ncores = ncores)
  #find nearest points
  np_points_A <- find_np(pts_A = points_A, pts_B = points_B , id_p_A = id_p_A, id_p_B = id_p_B, p = p, ncores = ncores)
  #select nearest lines
  np_step1 <- nnl_step1(pts_A = points_A,
                        pts_B = points_B,
                        fnp_A = np_points_A,
                        id_p_A = id_p_A,
                        id_p_B = id_p_B,
                        id_l_A = id_l_A,
                        id_l_B = id_l_B,
                        rate = rate)
  #find discontinuities
  np_step2 <- nnl_step2(l_B = l_B, r_s1_A = np_step1, id_l_B = id_l_B,id_l_A = id_l_A)
  #select only columns with IDs of lines A and B on step 1
  nnl_s1 <- np_step1[, c(id_l_A, id_l_B)]
  #Idem on step 2
  nnl_s2 <- np_step2[, c(id_l_A, id_l_B)]
  #fuse dataframes
  nnl_full <- rbind(nnl_s1, nnl_s2)
  #Select nearest lines in lines B
  nnl_sldf <- l_B[l_B@data[, id_l_B] %in% nnl_full[, id_l_B],]
  #add lines A IDs to nearest lines B
  nnl_sldf <- merge(nnl_sldf, nnl_full, by = id_l_B, all.x = T)
  #return Spatial
  return(nnl_sldf)
}
