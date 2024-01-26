library(AQLThesis)
library(future)
library(rjd3filters)
plan(multisession)
if(!dir.exists("data_simul/byseriescv"))
  dir.create("data_simul/byseriescv")

# Script pour calculer le critère de validation croisée sur les différents filtres pour tester le choix d'une fenêtre optimale
# Le critère est très peu discriminant : on ne retient pas cette méthode pour la sélection de la fenêtre

h_cv <- 18
for(s in list.files("data_simul/byseries",full.names = TRUE)){
  print(s)
  new_f = sprintf("data_simul/byseriescv/%s", basename(s))
  if(!file.exists(new_f)){
    data <- readRDS(s)
    info_fs <- lapply(data, function(x){
      future({sapply(3:h_cv, function(i){
        henderson_coef = lp_filter(horizon = i)[,sprintf("q=%i", i)]
        tmp = cross_validation(x, henderson_coef, lags = -i)^2
        if(all(is.na(tmp)))
          return(NA)
        mean(tmp[-c(1:h_cv, 1:h_cv + length(x) - h_cv)], na.rm=TRUE)
      }
      )
      })
    })
    info <- sapply(info_fs, value)
    rownames(info) <- sprintf("h=%i", 3:h_cv)
    saveRDS(info, new_f)
  }
}



h_cv <- 18
list_coefs <- lapply(3:h_cv, function(i){
  lp_filter(horizon = i,
            kernel = "Henderson",
            endpoints = "LC")
})
names(list_coefs) <- 3:h_cv
cv_asym = function (x, horizon = 6, q=0) {
  if (sum(!is.na(x)) < 2 * horizon + 1) 
    return(NA)
  if (q > horizon)
    return(NA)
  coef_used = list_coefs[[as.character(horizon)]][,sprintf("q=%i", q)]
  sc <- rjd3filters::filter(x, coef_used, horizon)
  
  (x - sc)/(1 - coef_used["t"])
}
for(s in list.files("data_simul/byseries",full.names = TRUE)){
  print(s)
  for(q in 0:6){
    print(q)
    new_f = gsub(".RDS",sprintf("_q%i.RDS", q),
                 sprintf("data_simul/byseriescv/%s", basename(s)))
    
    if(!file.exists(new_f)){
      data <- readRDS(s)
      info_fs <- lapply(data, function(x){
        future({sapply(3:h_cv, function(i){
          if(length(na.omit(x)) <= 2*h_cv +1)
            return(NA)
          tmp = cv_asym(x, i,q)^2
          tmp = tmp[-c(1:h_cv)]
          tmp = tmp[-seq(length(tmp), by = -1,length.out = h_cv)]
          mean(tmp, na.rm=TRUE)
        }
        )
        })
      })
      info <- sapply(info_fs, value)
      rownames(info) <- sprintf("h=%i", 3:h_cv)
      saveRDS(info, new_f)
    }
  }
  
}

info_fs = list()
i = 1
for(s in list.files("data_simul/byseries",full.names = TRUE)){
  print(s)
  for(q in 0:6){
    print(q)
    new_f = gsub(".RDS",sprintf("_q%i.RDS", q),
                 sprintf("data_simul/byseriescv/%s", basename(s)))
    
    data <- readRDS(s)
    info_fs[[i]][[q+1]] <- lapply(data, function(x){
      future({sapply(3:h_cv, function(i){
        if(length(na.omit(x)) <= 2*h_cv +1)
          return(NA)
        tmp = cv_asym(x, i,q)^2
        tmp = tmp[-c(1:h_cv)]
        tmp = tmp[-seq(length(tmp), by = -1,length.out = h_cv)]
        mean(tmp, na.rm=TRUE)
      }
      )
      })
    })
  }
  i <- i+1
}
