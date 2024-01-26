library(AQLThesis)
if(!dir.exists("results_simul/compile_revisions"))
  dir.create("results_simul/compile_revisions")
if(!dir.exists("results_simul/compile_tp"))
  dir.create("results_simul/compile_tp")
if(!dir.exists("results_simul/compile_tp_norev"))
  dir.create("results_simul/compile_tp_norev")
if(!dir.exists("results_fredm/compile_revisions"))
  dir.create("results_fredm/compile_revisions")
if(!dir.exists("results_fredm/compile_tp"))
  dir.create("results_fredm/compile_tp")
if(!dir.exists("results_fredm/compile_tp_norev"))
  dir.create("results_fredm/compile_tp_norev")

tp = readRDS("data_simul/tp_simul1.RDS")
dir = "localic_final"
for(dir in c("localic_daf", "localic_daf_trunc", "localic_final")){
  
  for(suffix in c("_fe_rev", "_ce_rev")){
    
    all_files <- list.files(sprintf("results_simul/%s/", dir),pattern = suffix,full.names = TRUE)
    
    all_rev <- lapply(seq_along(all_files), function(i){
      print(i)
      f = all_files[i]
      data = readRDS(f)
      data = summary_revisions(data,                    
                               peaks = tp$downturn,
                               troughs = tp$upturn)
      
      full_names <- gsub(sprintf("%s.RDS$", suffix), "", basename(f))
      split <- strsplit(full_names, "_")
      split <- strsplit(full_names, "_")
      series <- sapply(split, `[`, 1)
      method <- sapply(split, `[`, 2)
      if(length(grep("localic_daf", dir)) >0){
        h <- "h6"
        degree <- sapply(split, `[`, 3)
      } else {
        h <- sapply(split, `[`, 3)
        degree <- sapply(split, `[`, 4)
      }
      degree[is.na(degree)] <- "d3"
      
      data$series <- series
      data$kernel <- "henderson"
      data$method <- method
      data$h <- h
      data$degree <- degree
      data
    })
    all_rev = do.call(rbind, all_rev)
    
    saveRDS(all_rev, sprintf("results_simul/compile_revisions/%s%s.RDS",dir, suffix))
  }
  
  all_files <- list.files(sprintf("results_simul/%s/", dir),pattern = "_tp",full.names = TRUE)
  all_tp_lp <- lapply(seq_along(all_files), function(i){
    print(i)
    f = all_files[i]
    compute_time_lag(readRDS(f),
                     peaks = tp$downturn,
                     troughs = tp$upturn)
  })
  
  full_names <- gsub("_tp.RDS$", "", basename(all_files))
  split <- strsplit(full_names, "_")
  series <- sapply(split, `[`, 1)
  method <- sapply(split, `[`, 2)
  if(length(grep("localic_daf", dir)) >0){
    h <- "h6"
    degree <- sapply(split, `[`, 3)
  } else {
    h <- sapply(split, `[`, 3)
    degree <- sapply(split, `[`, 4)
  }
  degree[is.na(degree)] <- "d3"
  all_t <- data.frame(t(sapply(all_tp_lp, function(x) x[["troughs"]][["phaseshift"]])),
                      series, kernel = "henderson", method, h, degree)
  all_p <- data.frame(t(sapply(all_tp_lp, function(x) x[["peaks"]][["phaseshift"]])),
                      series, kernel = "henderson", method, h, degree)
  rownames(all_t) <- rownames(all_p) <- full_names
  
  saveRDS(all_t, sprintf("results_simul/compile_tp/troughs_%s.RDS", dir))
  saveRDS(all_p, sprintf("results_simul/compile_tp/peaks_%s.RDS", dir))
  
  all_tp_lp <- lapply(seq_along(all_files), function(i){
    print(i)
    f = all_files[i]
    compute_time_lag(readRDS(f),
                     peaks = tp$downturn,
                     troughs = tp$upturn,
                     type = "no_revisions")
  })
  
  full_names <- gsub("_tp.RDS$", "", basename(all_files))
  split <- strsplit(full_names, "_")
  series <- sapply(split, `[`, 1)
  method <- sapply(split, `[`, 2)
  if(length(grep("localic_daf", dir)) >0){
    h <- "h6"
    degree <- sapply(split, `[`, 3)
  } else {
    h <- sapply(split, `[`, 3)
    degree <- sapply(split, `[`, 4)
  }
  degree[is.na(degree)] <- "d3"
  
  all_t <- data.frame(t(sapply(all_tp_lp, function(x) x[["troughs"]][["phaseshift"]])),
                      series, kernel = "henderson", method, h, degree)
  all_p <- data.frame(t(sapply(all_tp_lp, function(x) x[["peaks"]][["phaseshift"]])),
                      series, kernel = "henderson", method, h, degree)
  rownames(all_t) <- rownames(all_p) <- full_names
  
  saveRDS(all_t, sprintf("results_simul/compile_tp_norev/troughs_%s.RDS", dir))
  saveRDS(all_p, sprintf("results_simul/compile_tp_norev/peaks_%s.RDS", dir))
}
#####################################################

for(dir in c("localic_daf", "localic_daf_trunc", "localic_final")){
  print(dir)
  all_files <- list.files(sprintf("results_fredm/%s/", dir),pattern = "_tp",full.names = TRUE)
  all_tp_lp <- lapply(seq_along(all_files), function(i){
    print(i)
    f = all_files[i]
    compute_time_lag(readRDS(f),                    
                     peaks = nber_tp_m[,"Peak"],
                     troughs = nber_tp_m[,"Trough"],
                     n_ahead_max = 12)
  })
  
  full_names <- gsub("_tp.RDS$", "", basename(all_files))
  split <- strsplit(full_names, "_")
  series <- sapply(split, `[`, 1)
  method <- sapply(split, `[`, 2)
  if(length(grep("localic_daf", dir)) >0){
    h <- "h6"
    degree <- sapply(split, `[`, 3)
  } else {
    h <- sapply(split, `[`, 3)
    degree <- sapply(split, `[`, 4)
  }
  degree[is.na(degree)] <- "d3"
  all_t <- data.frame(t(sapply(all_tp_lp, function(x) x[["troughs"]][["phaseshift"]])),
                      series, kernel = "henderson", method, h, degree)
  all_p <- data.frame(t(sapply(all_tp_lp, function(x) x[["peaks"]][["phaseshift"]])),
                      series, kernel = "henderson", method, h, degree)
  rownames(all_t) <- rownames(all_p) <- full_names
  
  saveRDS(all_t, sprintf("results_fredm/compile_tp/troughs_%s.RDS", dir))
  saveRDS(all_p, sprintf("results_fredm/compile_tp/peaks_%s.RDS", dir))
  
  all_tp_lp_localic <- lapply(seq_along(all_files), function(i){
    print(i)
    f = all_files[i]
    compute_time_lag(readRDS(f),                    
                     peaks = nber_tp_m[,"Peak"],
                     troughs = nber_tp_m[,"Trough"],
                     type = "no_revisions",
                     n_ahead_max = 12)
  })
  
  full_names <- gsub("_tp.RDS$", "", basename(all_files))
  split <- strsplit(full_names, "_")
  series <- sapply(split, `[`, 1)
  method <- sapply(split, `[`, 2)
  if(length(grep("localic_daf", dir)) >0){
    h <- "h6"
    degree <- sapply(split, `[`, 3)
  } else {
    h <- sapply(split, `[`, 3)
    degree <- sapply(split, `[`, 4)
  }
  degree[is.na(degree)] <- "d3"
  
  all_t <- data.frame(t(sapply(all_tp_lp, function(x) x[["troughs"]][["phaseshift"]])),
                      series, kernel = "henderson", method, h, degree)
  all_p <- data.frame(t(sapply(all_tp_lp, function(x) x[["peaks"]][["phaseshift"]])),
                      series, kernel = "henderson", method, h, degree)
  rownames(all_t) <- rownames(all_p) <- full_names
  
  saveRDS(all_t, sprintf("results_fredm/compile_tp_norev/troughs_%s.RDS", dir))
  saveRDS(all_p, sprintf("results_fredm/compile_tp_norev/peaks_%s.RDS", dir))
}

