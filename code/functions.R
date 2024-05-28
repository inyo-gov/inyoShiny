

# revert to single parcels name
#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
mult_to_single_parcel_name <- function(x){
  x$Parcel[x$Parcel=="TIN028_FSP022_FSP019"]<-"TIN028"
  x$Parcel[x$Parcel=="TIN028_FSP019_FSP022"]<-"TIN028"
  x$Parcel[x$Parcel=="MAN006_IND229"]<-"MAN006"
  x$Parcel[x$Parcel=="LAW137_PLC210"]<-"LAW137"
  x$Parcel[x$Parcel=="LAW108_FSL047"]<-"LAW108"
  x$Parcel[x$Parcel=="LAW109_FSL048"]<-"FSL048"
  x$Parcel[x$Parcel=="IND163_BEE017"]<-"IND163"
  x$Parcel[x$Parcel=="IND139_MAN005"]<-"IND139"
  x$Parcel[x$Parcel=="IND024_BLK103"]<-"IND024"
  x$Parcel[x$Parcel=="FSP004_BGP188"]<-"FSP004"
  x$Parcel[x$Parcel=="FSP006_BGP182"]<-"FSP006"
  x$Parcel[x$Parcel=="ABD012_BLK029"]<-"ABD012"
  x$Parcel[x$Parcel=="BLK002_TIN061"]<-"BLK002"
  x$Parcel[x$Parcel=="TIN061_BLK002"]<-"BLK002"
  x$Parcel[x$Parcel=="BIS055_FSL214"]<-"BIS055"
  x$Parcel[x$Parcel=="BIS025_FSL157"]<-"BIS025"
  x$Parcel[x$Parcel=="BIS026_FSL156"]<-"BIS026"
  x$Parcel[x$Parcel=="BLK002_TIN061"]<-"BLK002"


return(x)
}



