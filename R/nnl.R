#' Complete and unique function to extrapolate a SpatialLinesDataFrame A to a SpatialLinesDataFrame B
#'
#' @param l_A a SpatialLinesDataFrame. Reference lines.
#' @param l_B a SpatialLinesDataFrame. Searched lines.
#' @param id_l_A a character string. IDs Column name of reference SpatialLinesDataFrame (Lines A)
#' @param id_l_B a character string. IDs Column name of searched SpatialLinesDataFrame (Lines B)
#' @param id_p_A a character string. Indicates the IDs column name of points created to lines A. Default : "ID_PTS_A"
#' @param id_p_B a character string. Indicates the IDs column name of points created to lines B. Default : "ID_PTS_B"
#' @param bfr_width an integer. Buffer distance.
#' @param step an integer. Define the distance between two points along the line in meters. Defautl = 5.
#' @param rate an integer. Between 0 and 100. Define lower limit to select line B. Rate between number of points B on Lines A and totalpoints on Line B.
#' @param cut logical. Define if function cut SpatialLinesDataFrame B to a smaller entities.
#' @param p logical. Define if function use multithreading. Default = TRUE.
#' @param ncores an integer. Number of CPU cores used. If NULL, all cores - 1 are used.
#' @param verbose logical. View info for each step.
#'
#' @return a SpatialLinesDataFrame.
#'
#' @importFrom raster compareCRS
#' @importFrom raster intersect
#' @importFrom raster buffer
#'
#' @export
#'
#' @examples
#' \dontrun{
#' url_bresle <- paste0("https://api.sandre.eaufrance.fr/",
#'                      "coursdeau/v1/amont/G01-0400?pk_vid=aade75889c86a2471576396546bb9c85")
#' river_bresle <- rgdal::readOGR(url_bresle)
#'
#' url_example <- "https://raw.githubusercontent.com/naub1n/nnl/master/example/example.geojson"
#' example <- rgdal::readOGR(url_example)
#'
#' river_bresle <- sp::spTransform(river_bresle, sp::CRS("+init=epsg:2154"))
#'
#' extrapolate_example <- nnl(l_A = example,
#'                            l_B = river_bresle,
#'                            id_l_A = "ID_EXAMPLE",
#'                            id_l_B = "CdEntiteHydrographique",
#'                            cut = T)
#'
#' plot(river_bresle, col = "blue")
#' plot(example, col = "green", add = T)
#' plot(extrapolate_example, col = "red", add = T )
#' }
nnl <- function(l_A, l_B, id_l_A, id_l_B, id_p_A = "ID_PTS_A", id_p_B = "ID_PTS_B", bfr_width = 100, step = 5, rate = 45, cut = F, p = T, ncores = NULL, verbose = F){
  #check memory
  #check_mem_usage()
  #test CRS comparaison
  if(verbose) writeLines("CRS Comparaison")
  if(!raster::compareCRS(l_A, l_B)) stop("SpatialPointsDataFrames have not the same CRS")
  #test projection info. objects should be in planar coordinates (cf sp::sample)
  if(verbose) writeLines("Projection test")
  if(!sp::is.projected(l_A) || !sp::is.projected(l_B)) stop("Objects should be in planar coordinates")
  #cut l_B
  if(cut) {
    if(verbose) writeLines("Cut l_B")
    l_B <- cut_sldf(l = l_B, id_l = id_l_B)
  }
  #check memory
  #check_mem_usage()
  #reduce number of lines B by intersecting lines A buffer
  if(verbose) writeLines("Filter l_B")
  l_B <- filter_sldf(l_A = l_A, l_B = l_B, id_l_B = id_l_B, bfr_width = bfr_width)
  #check memory
  #check_mem_usage()
  #create points along lines A
  if(verbose) writeLines("Create points to l_A")
  points_A <- create_pts(l = l_A, id_l = id_l_A, id_p = id_p_A, step = step, p = p, ncores = ncores)
  if(verbose) writeLines(paste(nrow(points_A), "points created"))
  #check memory
  #check_mem_usage()
  #create points along lines B
  if(verbose) writeLines("Create points to l_B")
  points_B <- create_pts(l = l_B, id_l = id_l_B, id_p = id_p_B, step = step, p = p, ncores = ncores)
  if(verbose) writeLines(paste(nrow(points_B), "points created"))
  #check memory
  #check_mem_usage()
  #find nearest points
  if(verbose) writeLines("Find nearest points")
  np_points_A <- find_np(pts_A = points_A, pts_B = points_B , id_p_A = id_p_A, id_p_B = id_p_B, p = p, ncores = ncores)
  if(verbose) writeLines(paste(nrow(points_B), "points attached"))
  #check memory
  #check_mem_usage()
  #select nearest lines
  if(verbose) writeLines("Select nearest lines : Step 1")
  np_step1 <- nnl_step1(pts_A = points_A,
                        pts_B = points_B,
                        fnp_A = np_points_A,
                        id_p_A = id_p_A,
                        id_p_B = id_p_B,
                        id_l_A = id_l_A,
                        id_l_B = id_l_B,
                        rate = rate)
  if(verbose) writeLines(paste(nrow(np_step1), "lines found"))
  #find discontinuities
  if(verbose) writeLines("Select nearest lines : Step 2")
  np_step2 <- nnl_step2(l_B = l_B, r_s1_A = np_step1, id_l_B = id_l_B, id_l_A = id_l_A)
  if(verbose) writeLines(paste(nrow(np_step2), "lines found"))
  #select only columns with IDs of lines A and B on step 1
  if(verbose) writeLines("Aggregate result")
  nnl_s1 <- np_step1[, c(id_l_A, id_l_B)]
  #Idem on step 2
  nnl_s2 <- np_step2[, c(id_l_A, id_l_B)]
  #fuse dataframes
  nnl_full <- rbind(nnl_s1, nnl_s2)
  #Select nearest lines in lines B
  if(verbose) writeLines("Select nearest l_B")
  nnl_sldf <- l_B[l_B@data[, id_l_B] %in% nnl_full[, id_l_B],]
  #add lines A IDs to nearest lines B
  if(verbose) writeLines("Add IDs")
  nnl_sldf <- merge(nnl_sldf, nnl_full, by = id_l_B, all.x = T)
  #calculate Line A total length
  if(verbose) writeLines("Compare length result with l_A length")
  length_l_A <- sum(sapply(methods::slot(l_A,"lines"), function(x) sp::LinesLength(x)))
  #calculate nnl result total length
  length_nnl <- sum(sapply(methods::slot(nnl_sldf,"lines"), function(x) sp::LinesLength(x)))
  #calculate difference
  diff_length <- round(abs((length_l_A-length_nnl)/length_l_A)*100)
  #Warning if difference is too hight
  if(diff_length > 20) warning(paste(paste0("Difference between SpatialLine A length and SpatialLine nnl result length is upper than 20% : ", diff_length, "%"),
                                     "@lines length of SpatialLine B should be smaller than @lines length of SpatialLine A",
                                     "Try to use cut=TRUE in nnl() function, it transform SpatialLine B to a smaller segments", sep = "\n"))
  #return Spatial
  if(verbose) writeLines("End")
  return(nnl_sldf)
}
