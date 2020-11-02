#' Check memory usage. Stop programm if memory usage is to hight.
#'
#' @param limit numeric. Define minimum free memory in MB
#'
#' @export
#'
#' @examples
#' check_mem_usage()
check_mem_usage <- function(limit = 2000){
  #check free memory
  if(.Platform$OS.type == "unix") {
    mem_free <- as.numeric(gsub(",",".",system("free | grep Mem | awk '{print $7 / 1000}'", intern = TRUE)))
  } else {

  }
  #Stop programm if lot of memory is used
  if(mem_free > limit) {
    writeLines("Check memory usage : OK")
  } else {
    stop("Memory usage is to hight")
  }
}
