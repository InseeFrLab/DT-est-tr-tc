library(future)
plan(multisession)

if(!dir.exists("data_fredm/byseriesinfo"))
  dir.create("data_fredm/byseriesinfo")

fs <- list()
i <- 0
for(s in list.files("data_fredm/byseries",full.names = TRUE)){
  i <- i+1
  print(s)
  fs[[i]] <- future({
    data <- readRDS(s)
    info <- lapply(data, function(x){
      res = rjd3filters::select_trend_filter(x)
      res = c(res[c("length", "icr")],
              rjd3filters::select_trend_filter(x, length = 9)[1], 
              rjd3filters::select_trend_filter(x, length = 23)[1])[c(1,3,2,4)]
      names(res) = c("optimal_length", "icr-9", "icr-13", "icr-23")
      res
      })
    saveRDS(info, sprintf("data_fredm/byseriesinfo/%s", basename(s)))
    s
  })
}
vs <- lapply(fs, value)
