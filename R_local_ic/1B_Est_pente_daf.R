library(AQLThesis)
library(future)
library(rjd3filters)
plan(multisession)
if(!dir.exists("data_simul/byseriespente_daf"))
  dir.create("data_simul/byseriespente_daf")
if(!dir.exists("data_fredm/byseriespente_daf_nber_dt"))
  dir.create("data_fredm/byseriespente_daf_nber_dt")
if(!dir.exists("data_simul/byseries")) # création des données si elles n'existent pas
  source("R_simul/1A_data_creation.R")
if(!dir.exists("data_fredm/byseries")) # création des données si elles n'existent pas
  source("R_fredm/0_download_files.R")

# Dans ce programme, pour paramétrer les méthodes LC et QL :
# 1. On prend les MM asymétriques d'estimation de la pente et polynôme degré 2 et on fait une estimation en temps réel
# Pour chaque date on a donc h+1=6+1 moyennes mobiles
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

MM = lapply(0:6, function(q){
  d2 = gen_MM(p=6, q=q, d=2)
  d3 = gen_MM(p=6, q=q, d=3)
  add_0 <- matrix(0, ncol = ncol(d2), nrow = 6-q)
  d2 = rbind(d2, add_0)
  d3 = rbind(d3, add_0)
  list(pente = list(
    `d=2` = d2[,2],
    `d=3` = d3[,2]),
    `deriv2` = list(
      `d=2` = d2[,3],
      `d=3` = d3[,3]))
})
MM = list(pente = list(
  `d=2` = sapply(MM, function(x) x[[1]][[1]]),
  `d=3` = sapply(MM, function(x) x[[1]][[2]])
),
`deriv2` = list(
  `d=2` = sapply(MM, function(x) x[[2]][[1]]),
  `d=3` = sapply(MM, function(x) x[[2]][[2]])
)
)
MM = lapply(MM, function(x){
  lapply(x, finite_filters, first_to_last = TRUE)
})
s = list.files("data_simul/byseries",full.names = TRUE)[1]
d = 2
h=6
for(s in list.files("data_simul/byseries",full.names = TRUE)){
  new_f = sprintf("data_simul/byseriespente_daf/%s.RDS",
                  gsub(".RDS", "",basename(s)))
  print(new_f)
  hend_filter = lp_filter(horizon = h)@sfilter
  if(!file.exists(new_f)){
    data <- readRDS(s)
    info_fs <- lapply(data, function(x){
      future({
        sigma2 <- var_estimator(x, hend_filter)
        list("LC" = list(
          `d=2` = tail(rjd3filters::filterfilter(x, MM$pente$`d=2`),6),
          `d=3` = tail(rjd3filters::filterfilter(x, MM$pente$`d=3`),6),
          `sigma2` = sigma2
        ),
        "QL" = list(
          `d=2` = tail(rjd3filters::filterfilter(x, MM$deriv2$`d=2`),6),
          `d=3` = tail(rjd3filters::filterfilter(x, MM$deriv2$`d=3`),6),
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


for(s in list.files("data_fredm/byseries",full.names = TRUE)){
  new_f = sprintf("data_fredm/byseriespente_daf_nber_dt/%s.RDS", gsub(".RDS", "",basename(s)))
  print(new_f)
  hend_filter = lp_filter(horizon = h)[,h+1]
  if(!FALSE){
    data <- readRDS(s)
    info_fs <- lapply(data, function(x){
      future({
        sigma2 <- var_estimator(x, hend_filter)
        list("LC" = list(
          `d=2` = tail(rjd3filters::filterfilter(x, MM$pente$`d=2`),6),
          `d=3` = tail(rjd3filters::filterfilter(x, MM$pente$`d=3`),6),
          `sigma2` = sigma2
        ),
        "QL" = list(
          `d=2` = tail(rjd3filters::filterfilter(x, MM$deriv2$`d=2`),6),
          `d=3` = tail(rjd3filters::filterfilter(x, MM$deriv2$`d=3`),6),
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
