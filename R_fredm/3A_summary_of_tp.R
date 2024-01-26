library(AQLThesis)
if(!dir.exists("results_fredm/compile_tp"))
  dir.create("results_fredm/compile_tp")


all_files <- list.files("results_fredm/lp/",pattern = "_tp",full.names = TRUE)

all_tp_lp <- lapply(seq_along(all_files), function(i){
  print(i)
  f = all_files[i]
  compute_time_lag(readRDS(f),
                   peaks = nber_tp_m[,"Peak"],
                   troughs = nber_tp_m[,"Trough"],
                   type = "first_detection",
                   n_ahead_max = 12)
})

full_names <- gsub("_tp.RDS$", "", basename(all_files))
split <- strsplit(full_names, "_")
series <- sapply(split, `[`, 1)
kernel <- sapply(split, `[`, 2)
method <- sapply(split, `[`, 3)
all_t <- data.frame(t(sapply(all_tp_lp, function(x) x[["troughs"]][["phaseshift"]])),
                    series, kernel, method)
all_p <- data.frame(t(sapply(all_tp_lp, function(x) x[["peaks"]][["phaseshift"]])),
                    series, kernel, method)
rownames(all_t) <- rownames(all_p) <- full_names


saveRDS(all_t, "results_fredm/compile_tp/troughs_lp.RDS")
saveRDS(all_p, "results_fredm/compile_tp/peaks_lp.RDS")



all_files <- list.files("results_fredm/rkhs/",pattern = "_tp",full.names = TRUE)

all_tp_rkhs <- lapply(seq_along(all_files), function(i){
  print(i)
  f = all_files[i]
  compute_time_lag(readRDS(f),                    
                   peaks = nber_tp_m[,"Peak"],
                   troughs = nber_tp_m[,"Trough"],
                   type = "first_detection",
                   n_ahead_max = 12)
})

full_names <- gsub("_tp.RDS$", "", basename(all_files))
split <- strsplit(full_names, "_")
series <- sapply(split, `[`, 1)
method <- sapply(split, `[`, 2)
kernel = "henderson"
all_t <- data.frame(t(sapply(all_tp_rkhs, function(x) x[["troughs"]][["phaseshift"]])),
                    series, kernel, method)
all_p <- data.frame(t(sapply(all_tp_rkhs, function(x) x[["peaks"]][["phaseshift"]])),
                    series, kernel, method)
rownames(all_t) <- rownames(all_p) <- full_names

saveRDS(all_t, "results_fredm/compile_tp/troughs_rkhs.RDS")
saveRDS(all_p, "results_fredm/compile_tp/peaks_rkhs.RDS")


all_files <- list.files("results_fredm/arima/",pattern = "_tp",full.names = TRUE)

all_tp_arima <- lapply(seq_along(all_files), function(i){
  print(i)
  f = all_files[i]
  compute_time_lag(readRDS(f),                    
                   peaks = nber_tp_m[,"Peak"],
                   troughs = nber_tp_m[,"Trough"],
                   type = "first_detection",
                   n_ahead_max = 12)
})

full_names <- gsub("_tp.RDS$", "", basename(all_files))
split <- strsplit(full_names, "_")
series <- sapply(split, `[`, 1)
method <- "auto_arima"
kernel = "henderson"
all_t <- data.frame(t(sapply(all_tp_arima, function(x) x[["troughs"]][["phaseshift"]])),
                    series, kernel, method)
all_p <- data.frame(t(sapply(all_tp_arima, function(x) x[["peaks"]][["phaseshift"]])),
                    series, kernel, method)
rownames(all_t) <- rownames(all_p) <- full_names

saveRDS(all_t, "results_fredm/compile_tp/troughs_arima.RDS")
saveRDS(all_p, "results_fredm/compile_tp/peaks_arima.RDS")


all_files <- list.files("results_fredm/fst",
                        pattern = "_tp",full.names = TRUE)
all_tp <- lapply(seq_along(all_files), function(i){
  print(i)
  f = all_files[i]
  compute_time_lag(readRDS(f),                    
                   peaks = nber_tp_m[,"Peak"],
                   troughs = nber_tp_m[,"Trough"],
                   type = "first_detection",
                   n_ahead_max = 12)
})

full_names <- gsub("_tp.RDS$", "", basename(all_files))
split <- strsplit(full_names, "_")
series <- sapply(split, `[`, 1)
weight <- sapply(split, `[`, 3)
degree <- as.numeric(gsub("degree", "", sapply(split, `[`, 2)))
all_t <- data.frame(t(sapply(all_tp, function(x) x[["troughs"]][["phaseshift"]])),
                    series, degree, weight)
all_p <- data.frame(t(sapply(all_tp, function(x) x[["peaks"]][["phaseshift"]])),
                    series, degree, weight)
rownames(all_t) <- rownames(all_p) <- full_names
saveRDS(all_t,
        "results_fredm/compile_tp/troughs_fst.RDS")
saveRDS(all_p,
        "results_fredm/compile_tp/peaks_fst.RDS")
