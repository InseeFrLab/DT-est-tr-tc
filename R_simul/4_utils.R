library(dplyr)
library(AQLThesis)

length_info <- 
  sapply(list.files(path = "data_simul/byseriesinfo",full.names = TRUE), 
         function(f){
           info <- readRDS(f)
           13
         })
length_info <- data.frame(series = gsub(".RDS", "", basename(names(length_info))),
                          length = as.numeric(length_info))

select_var <- function(x){
  x = select_series(x)
  x
}
select_series <- function(x){
  x = merge(x, length_info, all.x = TRUE, all.y = FALSE, by = "series")
  x$variability <- gsub("\\d","", x$series)
  x
}
select_mae <- function(x){
  x %>% 
    dplyr::filter(Group == "total",
           stats == "MAE") %>% 
    select(!c(Group, stats))
}


unique_series_pivot <- function(x){
  to_remove = x %>% group_by(series, name) %>%
    mutate(remove = any(is.na(value))) %>% 
    data.frame()
  x[to_remove$remove,"value"] <- NA
  x
}
