if(!dir.exists("data_simul"))
  dir.create("data_simul")
if(!dir.exists("data_simul/byseries"))
  dir.create("data_simul/byseries")
if(!dir.exists("data_simul/byseriesinfo"))
  dir.create("data_simul/byseriesinfo")
library(AQLThesis)
set.seed(100)
start = 1960
frequency = 12
time = seq_along(seq(start, 2019+11/12, by = 1/12))
series = list(
  highvariability1 = simulated_tci(time,sigma_nu = 0.08,sigma_e = 0.40,lambda = 72,rho = 0.5),
  highvariability2 = simulated_tci(time,sigma_nu = 0.08,sigma_e = 0.40,lambda = 72,rho = 0.7),
  highvariability3 = simulated_tci(time,sigma_nu = 0.08,sigma_e = 0.40,lambda = 72,rho = 1),
  mediumvariability1 = simulated_tci(time,sigma_nu = 0.08,sigma_e = 0.30,lambda = 72,rho = 1.5),
  mediumvariability2 = simulated_tci(time,sigma_nu = 0.08,sigma_e = 0.30,lambda = 72,rho = 2),
  mediumvariability3 = simulated_tci(time,sigma_nu = 0.08,sigma_e = 0.30,lambda = 72,rho = 3),
  lowvariability1 = simulated_tci(time,sigma_nu = 0.08,sigma_e = 0.20,lambda = 72,rho = 3),
  lowvariability2 = simulated_tci(time,sigma_nu = 0.08,sigma_e = 0.20,lambda = 72,rho = 3.5),
  lowvariability3 = simulated_tci(time,sigma_nu = 0.08,sigma_e = 0.20,lambda = 72,rho = 4)
              )
series = lapply(series,ts, start = start, frequency = frequency)

tp = turning_points(series[[9]][,"cycle"])
first_date = time(series[[9]][,"cycle"])[25]
tp = lapply(tp, function(x)x[x>=first_date])
saveRDS(tp, "data_simul/tp_simul1.RDS")

for(nom_series in names(series)){
  all_series <- lapply(time[-(1:24)], function(i){
    ts(series[[nom_series]][1:i,"tc"], start = start, frequency = frequency)
  })
  names(all_series) <- sapply(all_series, function(x) time(x)[length(x)])
  saveRDS(all_series, file = sprintf("data_simul/byseries/%s.RDS", nom_series))
}


library(future)
plan(multisession)


fs <- list()
i <- 0
s = list.files("data_simul/byseries",full.names = TRUE)[1]
for(s in list.files("data_simul/byseries",full.names = TRUE)){
  i <- i+1
  print(s)
  fs[[i]] <- future({
    data <- readRDS(s)
    info <- lapply(data, function(x){
        x = data[[5]]
      res = rjd3filters::select_trend_filter(x)
      res = c(res[c("length", "icr")],
              rjd3filters::select_trend_filter(x, length = 9)[1], 
              rjd3filters::select_trend_filter(x, length = 23)[1])[c(1,3,2,4)]
      names(res) = c("optimal_length", "icr-9", "icr-13", "icr-23")
      res
    })
    saveRDS(info, sprintf("data_simul/byseriesinfo/%s", basename(s)))
    s
  })
}
vs <- lapply(fs, value)

last_icr <- do.call(rbind, lapply(lapply(list.files("data_simul/byseriesinfo",full.names = TRUE), readRDS),
                                   function(x) x[[length(x)]]))
rownames(last_icr) <- list.files("data_simul/byseriesinfo",full.names = FALSE)
round(last_icr,1)

