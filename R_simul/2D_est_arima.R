if(!dir.exists("results_simul"))
  dir.create("results_simul")
if(!dir.exists("results_simul/arima"))
  dir.create("results_simul/arima")
library(rjd3filters)
library(AQLThesis)
library(future)
plan(multisession)

list_series <- list.files("data_simul/byseries", full.names = TRUE)
list_method <- names(arima_f$`h=6`)
s = list_series[1]
method = "phase"
fs <- list()
i <- 0
for(s in list_series){
  i <- i+1
  name_file <- gsub(".RDS$", "", basename(s))
  print(name_file)
  fs[[i]] <- future({
    print(s)
    data <- readRDS(s)
    data_info <- readRDS(sub("byseries", "byseriesinfo", s))
    nom_f_s <- sprintf("results_simul/arima/%s.RDS",
                       name_file)
    nom_f_s_tp <- 
      sprintf("results_simul/arima/%s_tp.RDS",
              name_file)
    
    nom_f_s_rev_fe <- sprintf("results_simul/arima/%s_fe_rev.RDS",
                              name_file)
    nom_f_s_rev_ce <- sprintf("results_simul/arima/%s_ce_rev.RDS",
                              name_file)
    
    if(all(file.exists(nom_f_s_tp),
           file.exists(nom_f_s_rev_fe),
           file.exists(nom_f_s_rev_ce)))
      next;
    
    series_s <- lapply(names(data), function(nom_d){
      x <- data[[nom_d]]
      # l = data_info[[nom_d]]["optimal_length"]
      l = 13
      prevs = auto.arima(x, max.Q = 0, max.D = 0, max.P = 0)
      prevs = forecast(prevs, h=(l-1)/2)
      y_prevs = ts(c(x, prevs$mean), start = start(x), frequency = frequency(x))
      window(henderson(y_prevs, musgrave = FALSE,length = l),
             start = start(x), end = end(x))
    })
    names(series_s) <- names(data)
    
    # saveRDS(series_s, nom_f_s)
    
    print("turning points")
    tp <- lapply(series_s, turning_points)
    
    saveRDS(tp, nom_f_s_tp)
    revisions_firstest <- first_est_revisions(series_s)
    revisions_consest <- consecutive_est_revisions(series_s)
    
    saveRDS(revisions_firstest,
            nom_f_s_rev_fe)
    saveRDS(revisions_consest,
            nom_f_s_rev_ce)
    TRUE
  })
}


vs <- lapply(fs, value)
