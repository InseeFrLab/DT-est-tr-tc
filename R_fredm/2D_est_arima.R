if(!dir.exists("results_fredm"))
  dir.create("results_fredm")
if(!dir.exists("results_fredm/arima"))
  dir.create("results_fredm/arima")
library(rjd3filters)
library(AQLThesis)
library(future)
library(forecast)
library(magrittr)
plan(multisession)

list_series <- list.files("data_fredm/byseries", full.names = TRUE)
s = list_series[1]
fs <- list()
i <- 0
for(s in list_series){
  name_file <- gsub(".RDS$", "", basename(s))
  print(name_file)
  
  
  nom_f_s <- sprintf("results_fredm/arima/%s.RDS",
                     name_file)
  nom_f_s_tp <- 
    sprintf("results_fredm/arima/%s_tp.RDS",
            name_file)

  if(all(file.exists(nom_f_s_tp),
         file.exists(nom_f_s)))
    next;
  
  i <- i+1
  
  fs[[i]] <- future({
    print(s)
    data <- readRDS(s)
    data_info <- readRDS(sub("byseries", "byseriesinfo", s))
    
    series_s <- lapply(names(data), function(nom_d){
      x <- data[[nom_d]]
      l = data_info[[nom_d]]["optimal_length"]
      l = 13
      prevs = auto.arima(x, max.Q = 0, max.D = 0, max.P = 0) %>% 
        forecast(h=(l-1)/2)
      y_prevs = ts(c(x, prevs$mean), start = start(x), frequency = frequency(x))
      window(henderson(y_prevs, musgrave = FALSE,length = l),
             start = start(x), end = end(x))
    })
    names(series_s) <- names(data)
    
    saveRDS(series_s, nom_f_s)
    
    print("turning points")
    tp <- lapply(series_s, turning_points)

    saveRDS(tp, nom_f_s_tp)
    TRUE
  })
}

vs <- lapply(fs, value)
