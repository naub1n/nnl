#' Find near neighbors lines. First step which can contains discontinuities.
#'
#' @param pts_A a SpatialPointsDataFrame. Reference points.
#' @param pts_B a SpatialPointsDataFrame. Searched points.
#' @param fnp_A a data.table. Result of find_np().
#' @param id_p_A a character string. Indicates the IDs column name of pts_A.
#' @param id_p_B a character string. Indicates the IDs column name of pts_B.
#' @param id_l_A a character string. IDs Column name of reference SpatialLinesDataFrame (Lines A)
#' @param id_l_B a character string. IDs Column name of searched SpatialLinesDataFrame (Lines A)
#' @param rate an integer. Between 0 and 100. Define lower limit to select line B. Rate between number of points B on Lines A and totalpoints on Line B.
#'
#' @return a data.frame. Contains near neighbors lines
#'
#' @importFrom plyr ddply
#' @importFrom plyr summarise
#'
#' @export
#'
#' @examples
nnl_step1 <- function(pts_A, pts_B, fnp_A, id_p_A, id_p_B, id_l_A, id_l_B, rate = 45){
  #add IDs of lines A
  np_A_info <- base::merge(fnp_A, pts_A@data, by = id_p_A)
  #add IDs of lines B
  np_A_info <- base::merge(np_A_info, pts_B@data, by = id_p_B)
  #count unique points B IDs for each pair of LinesA/LinesB
  np_A_final <- plyr::ddply(np_A_info,c(id_l_A, id_l_B),
                                 .fun = function(x, colname) plyr::summarise(x, NB_PTS_B_TO_A = length(unique(x[,colname]))),
                                 colname = id_p_B)
  #count points on line A
  nb_pts_A <- data.frame(table(pts_A@data[,id_l_A]))
  #raname columns names
  colnames(nb_pts_A) <- c(id_l_A, "NB_PTS_A")
  #count points on line B
  nb_pts_B <- data.frame(table(pts_B@data[,id_l_B]))
  #raname columns names
  colnames(nb_pts_B) <- c(id_l_B, "NB_PTS_B")
  #add total number of points on line A
  np_A_final <- merge(np_A_final,nb_pts_A, by = id_l_A)
  #add total number of points on line B
  np_A_final <- merge(np_A_final,nb_pts_B, by = id_l_B)
  #calculate rate between number of points B on Lines A and totalpoints on Line B
  np_A_final$RATE <- round((np_A_final$NB_PTS_B_TO_A / np_A_final$NB_PTS_B) * 100)
  #selection only pair with rate higher than 'rate' parameter
  np_A_final <- np_A_final[np_A_final$RATE >= rate,]
  #add TRUE information for each row
  np_A_final$SELECT_STEP1 <- TRUE
  #Detect multi lines A on B
  d_multi <- plyr::ddply(np_A_final, id_l_B, .fun = function(x) plyr::summarise(x,
                                                                                nb_l_A = length(x[, id_l_A]),
                                                                                max_rate = max(x[, "RATE"]),
                                                                                min_rate = min(x[, "RATE"]),
                                                                                sd_rate = stats::sd(x[, "RATE"])))
  #select only lines B with multi lines A
  d_multi <- d_multi[d_multi$nb_l_A > 1, ]
  #If multi lines A are found
  if(nrow(d_multi) > 0){
    #for each row, define SELECT_STEP1 to False where sd_rate = 0 (impossible to shoose a line A) and where rate is not the max rate found.
    np_A_final[np_A_final[,id_l_B] %in% d_multi[, id_l_B], ]$SELECT_STEP1 <- apply(np_A_final[np_A_final[,id_l_B] %in% d_multi[, id_l_B], ], 1, function(x){
      if(d_multi[d_multi[, id_l_B] == x[id_l_B], "sd_rate"] == 0){
        return(FALSE)
      } else if(d_multi[d_multi[, id_l_B] == x[id_l_B], "max_rate"] == x["RATE"]){
        return(TRUE)
      } else {
        return(FALSE)
      }
    })
  }

  #select only row with SELECT_STEP1 == TRUE
  np_A_final <- np_A_final[np_A_final$SELECT_STEP1 == TRUE,]
  #return result
  return(np_A_final)
}
