library(rjd3filters)
# On reprend les poids de Bianconcini, Dagum (2015), A new set of filters for tracking the short-term trend in real time
# f = "Bianconcini, Dagum (2015), A new set of filters for tracking the short-term trend in real time.pdf"
# tabulizer::extract_tables(f,pages = 15)
# tab = tabulizer::extract_tables(f,pages = 15)[[1]]
# tab
# dput(as.numeric(tab[2,2:5]))
# dput(as.numeric(tab[3,2:5]))
# dput(as.numeric(tab[4,2:5]))
# dput(as.numeric(tab[6,2:7]))
# dput(as.numeric(tab[7,2:7]))
# dput(as.numeric(tab[8,2:7]))
# dput(as.numeric(tab[10,2:12]))
# dput(as.numeric(tab[11,2:12]))
# dput(as.numeric(tab[12,2:12]))

optimal_bw <- list("h=4"=list("frf" = c(6.47, 5.21, 4.9, 4.92, 5),
                              "gain" = c(8, 5.67, 4.87, 4.9, 5),
                              "phase" = c(4.01, 4.45, 5.97, 6.93, 5)),
                   "h=6"=list("frf" = c(9.54, 7.88, 7.07, 6.88, 6.87, 6.94, 7),
                              "gain" = c(11.78, 9.24, 7.34, 6.85, 6.84, 6.95, 7),
                              "phase" = c(6.01, 6.01, 7.12, 8.44, 9.46, 10.39, 7)),
                   "h=11"=list("frf" = c(17.32, 15.35, 13.53, 12.47, 12.05, 11.86, 11.77, 11.77, 11.82, 
                                         11.91, 11.98, 7),
                               "gain" = c(21.18, 18.4, 16.07, 13.89, 12.44, 11.9, 11.72, 11.73, 11.83, 
                                          11.92, 11.98, 7),
                               "phase" = c(11.01, 11.01, 11.01, 11.01, 11.41, 13.85, 15.13, 16.21, 17.21, 
                                           18.15, 19.05, 7)))
rkhs <- lapply(optimal_bw, function(h_){
  lapply(h_, function(method){
    h <- length(method)-1
    res <- do.call(cbind, lapply(seq_len(h+1), function(q){
      rkhs_filter(horizon = as.numeric(h), degree = 3,
                  kernel = "Biweight",
                  optimalbw = FALSE,
                  bandwidth = method[q])[,sprintf("q=%i", q-1)]
    }))
    res <- res[,seq(ncol(res), 1)]
    finite_filters(res)
  })
})


saveRDS(rkhs, "R_filters/rkhs_rw_p3.RDS")


