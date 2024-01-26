# Tests d'équivalence entre l'approche FST et lp
if(!dir.exists("R_henderson_theorem/data"))
    dir.create("R_henderson_theorem/data")
library(rjd3filters)
validitySign <- function(coef, p = 2){
    updn <- c(0, diff(sign(coef)))
    ix <- which(updn != 0)
    length(ix) <= p
}
# Création d'une grille
resolution = 201
data <- expand.grid(smoothness.weight = c(seq(0,1,length.out = resolution),1-1/1000,1-1/2000,1-1/3000,1-1/4000,1-1/5000),
                    timeliness.weight = c(seq(0,1,length.out = resolution),1-1/1000,1-1/2000,1-1/3000,1-1/4000,1-1/5000)
)
data$fidelity.weight <- 1 - (data$smoothness.weight + data$timeliness.weight)
data <- data[(data$fidelity.weight<=1) & (data$fidelity.weight>=0),]

null_value = 10^-10
for (h in c(6, 11)) {
  resultat_complet <- lapply(0:3, function(p){
    print(sprintf("p=%i",p))
    res <- lapply(0:h, function(q){
      print(sprintf("q=%i",q))
      mapply(function(x,y){
        tryCatch({
          fst <- rjd3filters::fst_filter(h,q,smoothness.weight = x,timeliness.weight = y,
                                         pdegree = p,
                                         smoothness.degree = 3)
          coef <- coef(fst)
          if((p==0)&&(sum(coef*seq(-h,length(coef) -h-1, by = 1))<null_value)){
            p <- 1
          }
          if((p==1)&&(sum(coef*seq(-h,length(coef) -h-1, by = 1)^2)<null_value)){
            p <- p+1
          }
          if((p==2)&&(sum(coef*seq(-h,length(coef) -h-1, by = 1)^3)<null_value)){
            p <- p+1
          }
          validitySign(coef, p = p+1)
        }, error = function(e){
          TRUE
        })
      }, data$smoothness.weight, data$timeliness.weight)
    })
    names(res) <- sprintf("q=%i",0:h)
    res
  })
  names(resultat_complet) <- sprintf("p=%i",0:3)
  poids_non_equiv <- lapply(resultat_complet, function(p_f){
    lapply(p_f, function(q_f){
      data[!q_f,]  
    })
  })
  saveRDS(data,
          file = "R_henderson_theorem/data/poids.RDS")
  saveRDS(resultat_complet,
          file = sprintf("R_henderson_theorem/data/resultats_h%i.RDS",h))
  saveRDS(poids_non_equiv,
          file = sprintf("R_henderson_theorem/data/poids_non_equiv_h%i.RDS",h))
  
}

