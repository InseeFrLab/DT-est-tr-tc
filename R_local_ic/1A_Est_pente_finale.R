library(AQLThesis)
library(future)
library(rjd3filters)
plan(multisession)
if(!dir.exists("data_simul/byseriespente_final"))
  dir.create("data_simul/byseriespente_final")
if(!dir.exists("data_fredm/byseriespente_final_nber_dt"))
  dir.create("data_fredm/byseriespente_final_nber_dt")
if (!file.exists()) {
  source("R_simul/1A_data_creation.R")
}
if(!dir.exists("data_simul/byseries")) # création des données si elles n'existent pas
  source("R_simul/1A_data_creation.R")
if(!dir.exists("data_fredm/byseries")) # création des données si elles n'existent pas
  source("R_fredm/0_download_files.R")

# Dans ce programme, pour paramétrer les méthodes LC et QL :
# 1. On prend les MM symétriques finales d'estimation de la pente et polynôme degré 2
#  (dans gen_MM q = p)
# 2. la variance est estimée à chaque date sur les données connues

X_gen <- function(d = 1, p = 6, q = p){
  sapply(0:d, function(exp) seq(-p, q)^exp)
}

gen_MM <- function(p=6, q=p, d=2){
  k = rjd3filters::get_kernel("Henderson", h = p)
  k
  k = c(rev(k$coef[-1]), k$coef[seq(0,q)+1])
  k
  K = diag(k)
  X = X_gen(d=d, p = p, q = q)
  e1 = e2 = e3 = matrix(0, ncol = 1, nrow = d+1)
  e1[1] = 1
  e2[2] = 1
  e3[3] = 1
  M1 = K %*% X %*% solve(t(X) %*% K %*% X, e1)
  M2 = K %*% X %*% solve(t(X) %*% K %*% X, e2)
  M3 = K %*% X %*% solve(t(X) %*% K %*% X, e3)
  cbind(M1, M2, M3)
}


MM = lapply(3:6, function(h){
  list(pente = list(
    `d=2` = gen_MM(p=h, d=2)[,2],
    `d=3` = gen_MM(p = h, d=3)[,2]),
    `deriv2` = list(
      `d=2` = gen_MM(p=h, d=2)[,3],
      `d=3` = gen_MM(p = h, d=3)[,3]),
       henderson = lp_filter(horizon = h)@sfilter
    )
})
names(MM) <- sprintf("h=%i", 3:6)
s = list.files("data_simul/byseries",full.names = TRUE)[1]
d = 2
h = 6
for(s in list.files("data_simul/byseries",full.names = TRUE)){
  print(s)
  for(h in 3:6){
    new_f = sprintf("data_simul/byseriespente_final/%s_h%i.RDS",
                    gsub(".RDS", "",basename(s)),h)
    print(new_f)
    MM_h = MM[[sprintf("h=%i", h)]]
    if(!file.exists(new_f)){
      data <- readRDS(s)
      last_est = data[[length(data)]]
      pente_d2 = zoo::na.locf(moving_average(MM_h$pente[[sprintf("d=%i",2)]], -h) * last_est)
      pente_d3 = zoo::na.locf(moving_average(MM_h$pente[[sprintf("d=%i",3)]], -h) * last_est)
      courbure_d2 = zoo::na.locf(moving_average(MM_h$deriv2[[sprintf("d=%i",2)]], -h) * last_est)
      courbure_d3 = zoo::na.locf(moving_average(MM_h$deriv2[[sprintf("d=%i",3)]], -h) * last_est)

      info_fs <- lapply(data, function(x){
        future({
          sigma2 <- var_estimator(x, MM_h[["henderson"]])
          list("LC" = list(
            `d=2` = tail(window(pente_d2, end = end(x)), 6),
            `d=3` = tail(window(pente_d3, end = end(x)), 6),
            `sigma2` = sigma2
          ),
          "QL" = list(
            `d=2` = tail(window(courbure_d2, end = end(x)), 6),
            `d=3` = tail(window(courbure_d3, end = end(x)), 6),
            `sigma2` = sigma2
          )
          )
        }
        )
      })
      info <- lapply(info_fs, value)
      saveRDS(info, new_f)
    }
  }
}


for(s in list.files("data_fredm/byseries",full.names = TRUE)){
  print(s)
  for(h in 6){
    new_f = sprintf("data_fredm/byseriespente_final_nber_dt/%s_h%i.RDS", gsub(".RDS", "",basename(s)),h)
    print(new_f)
    MM_h = MM[[sprintf("h=%i", h)]]
    hend_filter = lp_filter(horizon = h)[,h+1]
    if(!file.exists(new_f)){
      data <- readRDS(s)
      last_est = data[[length(data)]]
      
      pente_d2 = zoo::na.locf(moving_average(MM_h$pente[[sprintf("d=%i",2)]], -h) * last_est)
      pente_d3 = zoo::na.locf(moving_average(MM_h$pente[[sprintf("d=%i",3)]], -h) * last_est)
      courbure_d2 = zoo::na.locf(moving_average(MM_h$deriv2[[sprintf("d=%i",2)]], -h) * last_est)
      courbure_d3 = zoo::na.locf(moving_average(MM_h$deriv2[[sprintf("d=%i",3)]], -h) * last_est)
      info_fs <- lapply(data, function(x){
        future({
          sigma2 <- var_estimator(x, MM_h[["henderson"]])
          list("LC" = list(
            `d=2` = tail(window(pente_d2, end = end(x)), 6),
            `d=3` = tail(window(pente_d3, end = end(x)), 6),
            `sigma2` = sigma2
          ),
          "QL" = list(
            `d=2` = tail(window(courbure_d2, end = end(x)), 6),
            `d=3` = tail(window(courbure_d3, end = end(x)), 6),
            `sigma2` = sigma2
          )
          )
        }
        )
      })
      info <- lapply(info_fs, value)
      saveRDS(info, new_f)
    }
  }
}
