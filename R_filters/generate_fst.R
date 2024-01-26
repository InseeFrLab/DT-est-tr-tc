## Créer les poids/diagnostiques associés à la méthode FST pour h=6 et h=11
if(!dir.exists("R_filters/fst"))
  dir.create("R_filters/fst")
library(rjd3filters)
h_list <- c(9, 13, 23)
h_list <- (h_list-1)/2
h <- h_list[2]
all_fst_weights <- function(lags=6, leads=0, pdegree=2, smoothness.degree=3, 
                            timeliness.passband=pi/6, timeliness.antiphase=T,
                            resolution=101){
  data <- expand.grid(smoothness.weight = c(seq(0,1,length.out = resolution),1-1/1000,1-1/2000,1-1/3000,1-1/4000,1-1/5000),
                      timeliness.weight = c(seq(0,1,length.out = resolution),1-1/1000,1-1/2000,1-1/3000,1-1/4000,1-1/5000)
  )
  data$fidelity.weight <- 1 - (data$smoothness.weight + data$timeliness.weight)
  data <- data[(data$fidelity.weight<=1) & data$fidelity.weight>=0,]
  
  resultat <- mapply(function(x,y){
    tryCatch({
      if(leads == lags){ #Henderson filters
        return(lp_filter(horizon = lags)[,sprintf("q=%i", leads)])
      }
      filter <- fst_filter(lags = lags, leads = leads, pdegree=pdegree, 
                           smoothness.weight=x, smoothness.degree=smoothness.degree,
                           timeliness.weight=y,
                           timeliness.passband=timeliness.passband,
                           timeliness.antiphase=timeliness.antiphase)
      
      filter
    }, error = function(e) moving_average(NA))
  }, data$smoothness.weight, data$timeliness.weight,
  SIMPLIFY = FALSE)
  list(weights = data, results = resultat)
}
for (h in c(6,11)){
  all_q <- 0:h
  all_stats <- lapply(0:3, function(degree){
    res = lapply(all_q, function(q){
      print(sprintf("q=%i, pdegree = %i", q, degree))
      all_fst_weights(leads = q, pdegree = degree,
                      resolution = 21,
                      lags = h)
    })
    weights = res[[1]]$weights
    all_coefs = lapply(seq_len(nrow(weights)),function(i){
      coefs <- do.call(cbind,lapply(res, function(x){
        x$results[[i]]
      }))
      colnames(coefs) <- sprintf("q=%i", 0:(h))
      coefs <- coefs[,seq(ncol(coefs), 1)]
      coefs
    })
    total <- list(weights = weights, coefs = all_coefs)
    saveRDS(total, sprintf("R_filters/fst/fst_pdegree%i_h%i.RDS",
                           degree,h))
  })
}

for(degree in 0:3){
  h6 <- readRDS(sprintf("R_filters/fst/fst_pdegree%i_h%i.RDS",
                        degree, 6))
  h11 <- readRDS(sprintf("R_filters/fst/fst_pdegree%i_h%i.RDS",
                         degree, 11))
  weights = h6$weights
  total <- lapply(seq_along(h6$coefs), function(i){
    res = list(h6$coefs[[i]], h11$coefs[[i]])
    names(res) <- c("h=6","h=11")
    res
  })
  w_to_keep <- !sapply(total, function(x) any(sapply(x, function(y) any(is.na(y)))))
  coefs <- total[w_to_keep]
  coefs <- lapply(coefs, function(x) lapply(x, finite_filters))
  saveRDS(list(weights = weights[w_to_keep,],
               coefs = coefs),
          sprintf("R_filters/fst_pdegree%i.RDS",
                  degree))
}
