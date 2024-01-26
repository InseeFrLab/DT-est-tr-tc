if(!dir.exists("data"))
  dir.create("data")
if(!dir.exists("data_fredm/csv"))
  dir.create("data_fredm/csv")
if(!dir.exists("data_fredm/rds"))
  dir.create("data_fredm/rds")

if(!dir.exists("data_fredm/rds"))
  dir.create("data_fredm/rds")
library(future)
overwrite <- TRUE

# Téléchargement des bases FRED-MD
download.file("https://s3.amazonaws.com/files.research.stlouisfed.org/fred-md/Historical_FRED-MD.zip",
              "data_fredm/Historical_FRED-MD.zip")
download.file("https://s3.amazonaws.com/files.research.stlouisfed.org/fred-md/FRED_MD.zip",
              "data_fredm/FRED_MD.zip")

list_hist_files <- unzip("data_fredm/Historical_FRED-MD.zip",exdir = "data", overwrite = overwrite)
list_recent_files <- unzip("data_fredm/FRED_MD.zip",exdir = "data", overwrite = overwrite)

# On deplace tous les fichiers .csv sous data_fredm/csv
filesstrings::move_files(grep("\\.csv$", list_hist_files, value = TRUE), "data_fredm/csv/", overwrite = overwrite)
filesstrings::move_files(grep("\\.csv$", list_recent_files, value = TRUE), "data_fredm/csv/",overwrite = overwrite)

# on deplace tous les autres fichiers sous data
filesstrings::move_files(grep("\\.csv$", list_hist_files, invert = TRUE, value = TRUE), "data_fredm/", overwrite = overwrite)
filesstrings::move_files(grep("\\.csv$", list_recent_files, invert = TRUE, value = TRUE), "data_fredm/",overwrite = overwrite)

# On supprime les dossiers inutiles
unlink(unique(dirname(list_hist_files)), recursive = TRUE)
unlink(unique(dirname(list_recent_files)), recursive = TRUE)
file.remove("data_fredm/csv/current.csv")


plan(multisession)

fs <- list()
i <- 0
for(f in list.files(path = "data_fredm/csv", pattern = "\\.csv$",full.names = TRUE)){
  i <- i+1
  
  fs[[i]] <- future({
    print(f)
    data <- AQLThesis::fredmd(file = f, transform = FALSE, log = TRUE)
    saveRDS(data, sub("csv/", "rds/",sub("\\.csv$",".RDS", f), fixed = TRUE))
  })
}
vs <- lapply(fs, value)

## creation data par serie et par date

if(!dir.exists("data_fredm/byseries"))
  dir.create("data_fredm/byseries")
series <- unique(unlist(lapply(list.files("data_fredm/rds",full.names = TRUE),
                               function(x)colnames(readRDS(x)))))
# On ne retient qu'une sous-partie des séries : celles mentionnées par le NBER dans l'analyse des points de retournement
series_to_keep <- c(
    "W875RX1", # Real personal income ex transfer receipts
    "INDPRO", #industrial production
    "PAYEMS", # All Employees: Total nonfarm
    "DPCERA3M086SBEA", #Real personal consumption expenditures 
    "RETAILx", #Retail sales
    "CE16OV" #employment as measured by the household survey
)

full_data <- readRDS("data_fredm/rds/2022-11.RDS")
full_data <- lapply(series_to_keep, function(s) {
    na.omit(na.contiguous(full_data[,s]))
})
names(full_data) <- series_to_keep
full_data <- lapply(full_data, function(data) {
    dates_studied <- time(data)[-(1:27)] 
    data_cut <- lapply(dates_studied, function(end_date) {
        window(data, end = end_date)
    })
    names(data_cut) <- dates_studied
    data_cut
})

lapply(names(full_data), function(s) {
    saveRDS(full_data[[s]], sprintf("data_fredm/byseries/%s.RDS", s))
    TRUE
})
