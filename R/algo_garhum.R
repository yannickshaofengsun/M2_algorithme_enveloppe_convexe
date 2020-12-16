setwd("/Users/Yannick/Desktop/M2Data_science/algo/M2_algorithme-enveloppe-convexe")
source("R/vect_cal.R")

x = array(c(1,2,3,4,5,7,7,2,3,4,7,1), dim = c(6,2))

z = array(c(1,2,3,4,1,3,7,2,7,7,7,1), dim = c(6,2))


graham <- function(v) {
  ini = graham_ini(v)
  old_angle = vect_angle(v)
  sorte_angle = mmergesort(old_angle)

}

ord = match(vect_angle(x), mmergesort(vect_angle(x)))

for (i in 1:length(ord)){
  if
}
