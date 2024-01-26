if(!dir.exists("results_fredm"))
  dir.create("results_fredm")
if(!dir.exists("results_fredm/fst"))
  dir.create("results_fredm/fst")
library(rjd3filters)
library(AQLThesis)
library(future)
plan(multisession)
list_series <- list.files("data_fredm/byseries", full.names = TRUE)
list_degree <- c(0:3)
list_degree <- 2
s = list_series[1]
q = 0
fs <- list()
j <- 1
reload <- FALSE
w_to_keep = 235

for(degree in list_degree){
  print(degree)
  all_coefs <- readRDS(sprintf("R_filters/fst_pdegree%i.RDS",degree))
  weights = all_coefs$weights[(w_to_keep),]
  all_coefs = all_coefs$coefs[w_to_keep]
  
  for(i in seq_along(all_coefs)){
    print(i)
    for(s in list_series){
      name_file <- gsub(".RDS$", "", basename(s))
      data <- readRDS(s)
      data_info <- readRDS(sub("byseries", "byseriesinfo", s))
      nom_f_s <- sprintf("results_fredm/fst/%s_degree%s_weight%i.RDS",
                         name_file,
                         degree,
                         w_to_keep[i]
      )
      nom_f_s_tp <- sprintf("results_fredm/fst/%s_degree%s_weight%i_tp.RDS",
                            name_file,
                            degree,
                            w_to_keep[i]
      )
      coefs = all_coefs[[i]]
      if(all(file.exists(nom_f_s_tp),
             file.exists(nom_f_s_rev_fe),
             file.exists(nom_f_s_rev_ce)))
        next;
      print(name_file)
      
      reload <- TRUE
      fs[[j]] <- future({
        print(s)
        series_s <- lapply(names(data), function(nom_d){
          x <- data[[nom_d]]
          l = data_info[[nom_d]]["optimal_length"]
          l = 13
          if(isTRUE(all.equal(as.numeric(l),9)))
            return(NULL)
          coef = coefs[[sprintf("h=%i", (l-1)/2)]]
          rjd3filters::filterfilter(x, coef)
        })
        names(series_s) <- names(data)
        
        saveRDS(series_s, nom_f_s)
        
        print("turning points")
        tp <- lapply(series_s, turning_points)
        saveRDS(tp,
                nom_f_s_tp
        )
        
       
        TRUE
      })
      j <- j+1
    }
    if(reload){
      vs <- lapply(fs, value)
      fs <- list()
      j <- 1
    }
  }
}
