# written for SimTarget function
# identify people with clinically detected cancer at index date

get.pid.with.crc <- function(adenoma,index.date=specs$date){
  unique(subset(adenoma, date.clindet <= index.date,select=p.id, drop=TRUE))
}
