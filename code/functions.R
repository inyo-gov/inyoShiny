

#' Revert to Single Parcel Name
#'
#' @param x A data frame containing parcel data.
#' @return A data frame with single parcel names.
#' @export
#' @examples
#' mult_to_single_parcel_name(x)
mult_to_single_parcel_name <- function(x) {
  # ABD012
  x$Parcel[x$Parcel == "BLK029"] <- "ABD012"
  x$Parcel[x$Parcel == "ABD012_BLK029"] <- "ABD012"
  x$Parcel[x$Parcel == "ABD012/BLK029"] <- "ABD012"

  # BIS025
  x$Parcel[x$Parcel == "BIS025_FSL157"] <- "BIS025"

  # BIS026
  x$Parcel[x$Parcel == "BIS026_FSL156"] <- "BIS026"

  # BIS055
  x$Parcel[x$Parcel == "FSL214"] <- "BIS055"
  x$Parcel[x$Parcel == "BIS055_FSL214"] <- "BIS055"
  x$Parcel[x$Parcel == "BIS055/FSL214"] <- "BIS055"

  # BLK002
  x$Parcel[x$Parcel == "TIN061"] <- "BLK002"
  x$Parcel[x$Parcel == "BLK002_TIN061"] <- "BLK002"
  x$Parcel[x$Parcel == "TIN061_BLK002"] <- "BLK002"

  # FSL048
  x$Parcel[x$Parcel == "LAW109"] <- "FSL048"
  x$Parcel[x$Parcel == "LAW109_FSL048"] <- "FSL048"

  # FSP004
  x$Parcel[x$Parcel == "BGP188"] <- "FSP004"
  x$Parcel[x$Parcel == "FSP004_BGP188"] <- "FSP004"
  x$Parcel[x$Parcel == "FSP004_BPG188"] <- "FSP004"
  x$Parcel[x$Parcel == "FSP004/BGP188"] <- "FSP004"

  # FSP006
  x$Parcel[x$Parcel == "BGP182"] <- "FSP006"
  x$Parcel[x$Parcel == "FSP006_BGP182"] <- "FSP006"
  x$Parcel[x$Parcel == "FSP006/BGP182"] <- "FSP006"

  # IND024
  x$Parcel[x$Parcel == "BLK103"] <- "IND024"
  x$Parcel[x$Parcel == "IND024_BLK103"] <- "IND024"
  x$Parcel[x$Parcel == "IND024/BLK103"] <- "IND024"

  # IND139
  x$Parcel[x$Parcel == "MAN005"] <- "IND139"
  x$Parcel[x$Parcel == "IND139_MAN005"] <- "IND139"
  x$Parcel[x$Parcel == "IND139/MAN005"] <- "IND139"

  # IND163
  x$Parcel[x$Parcel == "BEE017"] <- "IND163"
  x$Parcel[x$Parcel == "IND163_BEE017"] <- "IND163"

  # LAW108
  x$Parcel[x$Parcel == "FSL047"] <- "LAW108"
  x$Parcel[x$Parcel == "LAW108_FSL047"] <- "LAW108"
  x$Parcel[x$Parcel == "LAW108/FSL047"] <- "LAW108"

  # LAW137
  x$Parcel[x$Parcel == "PLC210"] <- "LAW137"
  x$Parcel[x$Parcel == "LAW137_PLC210"] <- "LAW137"
  x$Parcel[x$Parcel == "LAW137/PLC210"] <- "LAW137"

  # MAN006
  x$Parcel[x$Parcel == "MAN006_IND229"] <- "MAN006"
  x$Parcel[x$Parcel == "MAN006/IND229"] <- "MAN006"
  x$Parcel[x$Parcel == "IND229"] <- "MAN006"

  # TIN028
  x$Parcel[x$Parcel == "FSP019"] <- "TIN028"
  x$Parcel[x$Parcel == "FSP022"] <- "TIN028"
  x$Parcel[x$Parcel == "TIN028_FSP022_FSP019"] <- "TIN028"
  x$Parcel[x$Parcel == "TIN028_FSP019_FSP022"] <- "TIN028"

  return(x)
}
