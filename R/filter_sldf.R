#' Title
#'
#' @param l_A a SpatialLinesDataFrame. Reference lines.
#' @param l_B a SpatialLinesDataFrame. Searched lines.
#' @param id_l_B a character string. IDs Column name of searched SpatialLinesDataFrame (Lines B)
#' @param bfr_width an integer. Buffer distance.
#'
#' @return a SpatialLinesDataFrame.
#'
#' @import sf
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
filter_sldf <- function(l_A, l_B, id_l_B, bfr_width){
  #convert SpatialLinesDataFrame to sf
  sf_A <- sf::st_as_sf(l_A)
  sf_B <- sf::st_as_sf(l_B)
  #create buffer
  b = sf::st_buffer(sf_A, dist = bfr_width)
  #create IDs with row number
  sf_B.m = dplyr::mutate(.data = sf_B, id_sf_B = dplyr::row_number() )
  #filter sf_B with sf_B IDs which intersect buffer
  sf_B.f = dplyr::filter(.data = sf_B.m, .data[["id_sf_B"]] %in% unlist( sf::st_intersects(b, sf_B) ))
  #filter l_B with sf_B
  l_B.filtered <- l_B[l_B@data[["ID"]] %in% sf_B.f[["ID"]],]
  #return result
  return(l_B.filtered)
}
