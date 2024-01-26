if(!dir.exists("results_fredm"))
  dir.create("results_fredm")
if(!dir.exists("results_fredm/rkhs"))
  dir.create("results_fredm/rkhs")
rkhs_f <- readRDS("R_filters/rkhs_rw_p3.RDS")
library(rjd3filters)
library(AQLThesis)
library(future)
plan(multisession)

list_series <- list.files("data_fredm/byseries", full.names = TRUE)
list_method <- names(rkhs_f$`h=6`)
s = list_series[1]
method = "phase"
fs <- list()
i <- 0
for(method in list_method){
  print(method)
  for(s in list_series){
    name_file <- gsub(".RDS$", "", basename(s))
    print(name_file)
    
    
    nom_f_s <- sprintf("results_fredm/rkhs/%s_%s.RDS",
                       name_file,
                       tolower(method))
    nom_f_s_tp <- 
      sprintf("results_fredm/rkhs/%s_%s_tp.RDS",
              name_file,
              tolower(method))
    
    nom_f_s_rev_fe <- sprintf("results_fredm/rkhs/%s_%s_fe_rev.RDS",
                              name_file,
                              tolower(method))
    nom_f_s_rev_ce <- sprintf("results_fredm/rkhs/%s_%s_ce_rev.RDS",
                              name_file,
                              tolower(method))

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
        coef = rkhs_f[[sprintf("h=%i", (l-1)/2)]][[method]]
        rjd3filters::filterfilter(x, coef)
      })
      names(series_s) <- names(data)
      
      saveRDS(series_s, nom_f_s)
      
      print("turning points")
      tp <- lapply(series_s, turning_points)

      saveRDS(tp, nom_f_s_tp)
      TRUE
    })
  }
}

vs <- lapply(fs, value)

