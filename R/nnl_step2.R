#' Find discontinuities in nnl_step1() result
#'
#' @param l_B a SpatialLinesDataFrame. Searched lines.
#' @param r_s1_A a data.frame. Result of nnl_step1().
#' @param id_l_A a character string. IDs Column name of reference SpatialLinesDataFrame (Lines A)
#' @param id_l_B a character string. IDs Column name of searched SpatialLinesDataFrame (Lines B)
#'
#' @return a data.frame. Contains discontinuities.
#'
#' @importFrom stats sd
#' @import dplyr
#'
#' @export
#'
#' @examples
nnl_step2 <- function(l_B, r_s1_A, id_l_A, id_l_B){
  #extract points coordinates of each line
  lst_coord <- lapply(slot(l_B, "lines"), function(x) lapply(slot(x, "Lines"), function(y) slot(y, "coords")))
  #create IDs vector
  nodes <- subset(l_B@data, select = id_l_B)
  #extract extremities coordinates of each lines
  nodes$X_MIN <- as.numeric(lapply(lst_coord, function(x) x[[1]][1,1]))
  nodes$Y_MIN <- as.numeric(lapply(lst_coord, function(x) x[[1]][1,2]))
  nodes$X_MAX <- as.numeric(lapply(lst_coord, function(x) x[[length(x)]][nrow(x[[length(x)]]),1]))
  nodes$Y_MAX <- as.numeric(lapply(lst_coord, function(x) x[[length(x)]][nrow(x[[length(x)]]),2]))
  #create dataframe with lower extremities
  nodes_min <- subset(nodes, select=c(id_l_B, "X_MIN", "Y_MIN"))
  #create dataframe with upper extremities
  nodes_max <- subset(nodes,select=c(id_l_B, "X_MAX", "Y_MAX"))
  #rename columns
  colnames(nodes_min)[2:3]<-c("X","Y")
  colnames(nodes_max)[2:3]<-c("X","Y")
  #merge dataframes
  nodes.full <- rbind(nodes_min,nodes_max)
  #add ID for each node
  nodes.full$ID_NODE <- seq.int(nrow(nodes.full))
  #Jointure permettant de distinguer les tronçons identifiés sur le DP en phase 1
  nodes.nnlstep1 <- merge(nodes.full, r_s1_A, by = id_l_B, all.x = T, all.y = T)
  #Remplace les valeurs nulles du champ DP_phase1 par "non" // ATTENTION : as.character devant lapply, sinon le champs est de type list et ça bug dans les calcul de somme
  nodes.nnlstep1$SELECT_STEP1 <- as.character(lapply(nodes.nnlstep1$SELECT_STEP1,function(x) ifelse (is.na(x),FALSE,x)))
  #Toilettage table jointure
  nodes.nnlstep1 <- subset(nodes.nnlstep1, select=c(id_l_A, id_l_B, "ID_NODE","X","Y","SELECT_STEP1"))
  #merge nodes with themselves to find neighbour of each line
  RQT1 <- merge(nodes.nnlstep1, nodes.nnlstep1, by=c("X","Y"))
  #exclude pairs with the same line ID
  RQT1 <- subset(RQT1, RQT1$ID.x != RQT1$ID.y)
  #count number of lines selected in nnl_step1 for each node
  RQT2 <- dplyr::group_by(.data = RQT1, .data[[paste0(id_l_A,".x")]], .data[[paste0(id_l_B,".x")]], .data[["ID_NODE.x"]], .data[["SELECT_STEP1.x"]])
  RQT2 <- dplyr::summarise(.data = RQT2, NB_LINE_SELECT_STEP1 = sum(.data[["SELECT_STEP1.y"]] == TRUE))

  RQT3 <- dplyr::group_by(.data = RQT2, .data[[paste0(id_l_A,".x")]], .data[[paste0(id_l_B,".x")]], .data[["SELECT_STEP1.x"]])
  RQT3 <- dplyr::summarise(.data = RQT3,
                            #number of nodes
                            NB_NODES = length(.data[["ID_NODE.x"]]),
                            #standard deviation of number of points for each extremity
                            SD_LINES_SELECTED = stats::sd(.data[["NB_LINE_SELECT_STEP1"]]),
                            #Mean of number of points for each extremity
                            MEAN_LINES_SELECTED = mean(.data[["NB_LINE_SELECT_STEP1"]]))
  # create classes
  RQT3$classe<-ifelse(RQT3$NB_NODES == 2 & RQT3$MEAN_LINES_SELECTED == 0.5 & RQT3$SD_LINES_SELECTED == stats::sd(c(1,2)),"1.0",
                      ifelse(RQT3$NB_NODES == 2 & RQT3$MEAN_LINES_SELECTED == 1 & RQT3$SD_LINES_SELECTED == 0,"1.1",
                             ifelse(RQT3$NB_NODES == 2 & RQT3$MEAN_LINES_SELECTED == 1 & RQT3$SD_LINES_SELECTED == 1,"2.0",
                                    ifelse (RQT3$NB_NODES == 2 & RQT3$MEAN_LINES_SELECTED == 1.5 & RQT3$SD_LINES_SELECTED == stats::sd(c(1,2)),"2.1",
                                            ifelse (RQT3$NB_NODES == 2 & RQT3$MEAN_LINES_SELECTED == 2 & RQT3$SD_LINES_SELECTED == 0,"2.2",
                                                    "OTHER")))))
  #identify discontinuities
  RQT3$Disc<-ifelse(
    #lines not selected at origin
    RQT3$SELECT_STEP1.x == FALSE &
      #wich have one line at each extremity (so 2 lines)
      RQT3$NB_NODES == 2 &
      #where there are as many lines at each extremity
      RQT3$SD_LINES_SELECTED == 0 &
      #to select only in case where there are 1 line selected at extremity and 1 line selected at other extremity
      RQT3$MEAN_LINES_SELECTED == 1
    ,TRUE,FALSE)
  #select only discontinuities
  disc <- subset(RQT3, RQT3$Disc == TRUE)
  #rename columns
  colnames(disc)[grep(paste0(id_l_A,".x"), colnames(disc))] <- id_l_A
  colnames(disc)[grep(paste0(id_l_B,".x"), colnames(disc))] <- id_l_B
  #find line A ID for each line B
  disc[, id_l_A] <- apply(disc, 1, function(x) {
    id <- unique(RQT1[RQT1[, paste0(id_l_B, ".x")] == x[id_l_B] & RQT1$SELECT_STEP1.y == TRUE , "CdMasseDEa.y"])
    #return NA if many IDs are found
    id.f <- if(length(id) == 1) id else NA
    return(id.f)
  })
  #return result
  return(disc)
}
