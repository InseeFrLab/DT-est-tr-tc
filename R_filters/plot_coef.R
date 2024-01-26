library(rjd3filters)
library(patchwork)
library(AQLThesis)

rkhs <- readRDS("R_filters/rkhs_rw_p3.RDS")[["h=6"]]
lp <- lapply(c("LC", "QL", "CQ", "DAF"), function(endpoints){
  lp_filter(
    horizon = 6,
    degree = 3,
    kernel = "Henderson",
    endpoints = endpoints,
    ic = 3.5,
    tweight = 0,
    passband = pi/12
  )
})

w_to_keep <- 235
fst_coefs <- readRDS(sprintf("R_filters/fst_pdegree%i.RDS",2))
fst_coefs <- fst_coefs$coefs[w_to_keep]
fst_coefs <- lapply(fst_coefs, `[[`, "h=6")

all_filters <- c(rkhs,
                 lp,
                 fst_coefs)

names(all_filters) <- c("frf", "gain", "phase",
                        "LC", "QL", "CQ", "DAF", "FST")


options("OutDec" = ",")
for(i in names(all_filters)){
  print(i)
  x <- all_filters[[i]]
  p <- ggplot_coef(x, q = c(0:6)) / (
    ggplot_gain(x, q = c(0:6)) + 
      ggplot2::scale_y_continuous(
        "Gain",
        breaks = seq(0, 1, by = 0.2))+
      ggplot2::guides(colour = "none") + ggplot_phase(x, xlim = c(0, 4/12*pi), q = c(0:6))+
      ggplot2::guides(colour = "none"))
  p
  ggMultisave(sprintf("DT/img/filters_used/%s",tolower(i)),out = c("pdf", "svg", "jpg"),
              width = 8, height = 5,
              plot = p)
}

x <- all_filters[["LC"]]
p <- (ggplot_coef(x, q=0) + ggplot2::guides(colour = "none") )/ (
  ggplot_gain(x, q = 0)  + 
    ggplot2::scale_y_continuous(
      "Gain",
      breaks = seq(0, 1, by = 0.2),
      limits = c(0, NA))+ ggplot2::guides(colour = "none") + 
    ggplot_phase(x, xlim = c(0, pi), q = 0) + ggplot2::guides(colour = "none")
)
p

ggMultisave("DT/img/filters_used/musgrave",out = c("pdf", "svg", "jpg"),
            width = 8, height = 5,
            plot = p)

options("OutDec" = ".")
