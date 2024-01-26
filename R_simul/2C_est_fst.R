if(!dir.exists("results_simul"))
  dir.create("results_simul")
if(!dir.exists("results_simul/fst"))
  dir.create("results_simul/fst")
library(rjd3filters)
library(AQLThesis)
library(future)
plan(multisession)
list_series <- list.files("data_simul/byseries", full.names = TRUE)
list_degree <- c(0:3)

fs <- list()
j <- 1
reload <- FALSE

for(degree in list_degree){
  print(degree)
  all_coefs <- readRDS(sprintf("R_filters/fst_pdegree%i.RDS",degree))
  weights = all_coefs$weights
  all_coefs = all_coefs$coefs
  
  if(!dir.exists(sprintf("results_simul/fst/fst%i",degree)))
    dir.create(sprintf("results_simul/fst/fst%i",degree))
  for(i in seq_along(all_coefs)){
    print(i)
    for(s in list_series){
      name_file <- gsub(".RDS$", "", basename(s))
      data <- readRDS(s)
      data_info <- readRDS(sub("byseries", "byseriesinfo", s))
      nom_f_s <- sprintf("results_simul/fst/fst%i/%s_degree%s_weight%i.RDS",
                         degree,
                         name_file,
                         degree,
                         i
      )
      nom_f_s_tp <- sprintf("results_simul/fst/fst%i/%s_degree%s_weight%i_tp.RDS",
                            degree,
                            name_file,
                            degree,
                            i
      )
      
      nom_f_s_rev_fe <- sprintf("results_simul/fst/fst%i/%s_degree%s_weight%i_fe_rev.RDS",
                                degree,
                                name_file,
                                degree,
                                i
      )
      nom_f_s_rev_ce <- sprintf("results_simul/fst/fst%i/%s_degree%s_weight%i_ce_rev.RDS",
                                degree,
                                name_file,
                                degree,
                                i
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
          l = 13
          coef = coefs[[sprintf("h=%i", (l-1)/2)]]
          rjd3filters::filter(x, coef)
        })
        names(series_s) <- names(data)
        
        # saveRDS(series_s, nom_f_s)
        
        print("turning points")
        tp <- lapply(series_s, turning_points)
        saveRDS(tp,
                nom_f_s_tp
        )
        
        revisions_firstest <- first_est_revisions(series_s)
        revisions_consest <- consecutive_est_revisions(series_s)
        
        
        saveRDS(revisions_firstest, nom_f_s_rev_fe)
        saveRDS(revisions_consest, nom_f_s_rev_ce)
        TRUE
      })
      j <- j+1
    }
    if(reload & j >= 100){
      vs <- lapply(fs, value)
      fs <- list()
      j <- 1
    }
  }
}
