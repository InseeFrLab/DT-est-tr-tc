library(dplyr)
library(ggplot2)
library(patchwork)
library(AQLThesis)

length_info <- 
  sapply(list.files("data_fredm/byseriesinfo/",full.names = TRUE), 
         function(f){
           info <- readRDS(f)
           info[["2020"]][1]
         })
length_info <- data.frame(series = gsub("(.*/)|(.RDS.*)", "", 
                                        names(length_info)),
                          length = as.numeric(length_info))

troughs_detected <- t(sapply(list.files("results_fredm/compile_tp_norev",pattern = "troughs",full.names = TRUE), function(f){
  all_t <- readRDS(f)
  !apply(all_t[,1:35],2,\(x) all(is.na(x)))
}))

peaks_detected <- t(sapply(list.files("results_fredm/compile_tp_norev",pattern = "peaks",full.names = TRUE), function(f){
  all_t <- readRDS(f)
  !apply(all_t[,1:34],2,\(x) all(is.na(x)))
}))


selected_tp <- c("X2001.83333333333", "X2009.41666666667", "X2020.25", "X2001.16666666667", 
                 "X2007.91666666667", "X2020.08333333333")
not_selected_tp <- c("X1854.91666666667", "X1858.91666666667", "X1861.41666666667", 
                     "X1867.91666666667", "X1870.91666666667", "X1879.16666666667", 
                     "X1885.33333333333", "X1888.25", "X1891.33333333333", "X1894.41666666667", 
                     "X1897.41666666667", "X1900.91666666667", "X1904.58333333333", 
                     "X1908.41666666667", "X1912", "X1914.91666666667", "X1919.16666666667", 
                     "X1921.5", "X1924.5", "X1927.83333333333", "X1933.16666666667", 
                     "X1938.41666666667", "X1945.75", "X1949.75", "X1954.33333333333", 
                     "X1958.25", "X1961.08333333333", "X1970.83333333333", "X1975.16666666667", 
                     "X1980.5", "X1982.83333333333", "X1991.16666666667", "X1857.41666666667", 
                     "X1860.75", "X1865.25", "X1869.41666666667", "X1873.75", "X1882.16666666667", 
                     "X1887.16666666667", "X1890.5", "X1893", "X1895.91666666667", 
                     "X1899.41666666667", "X1902.66666666667", "X1907.33333333333", 
                     "X1910", "X1913", "X1918.58333333333", "X1920", "X1923.33333333333", 
                     "X1926.75", "X1929.58333333333", "X1937.33333333333", "X1945.08333333333", 
                     "X1948.83333333333", "X1953.5", "X1957.58333333333", "X1960.25", 
                     "X1969.91666666667", "X1973.83333333333", "X1980", "X1981.5", 
                     "X1990.5")
not_selected_tp_grep <- paste(sprintf("(%s)", not_selected_tp), collapse = "|")
select_var <- function(x){
  x = select_series(x)
  x = x[,
        grep(not_selected_tp_grep, colnames(x), invert = TRUE)]
  toNA <- apply(x[,grep("^X", colnames(x))],2, function(x_){
    res = x_ > x$length
    res[is.na(res)] <- FALSE
    res
  } )
  x[,grep("^X", colnames(x))][toNA] <- NA
  x
}
select_series <- function(x){
  # x = x[x$series %in% series_with_tp_ul,]
  x = merge(x, length_info, all.x = TRUE, all.y = FALSE)
  x
}

select_mae <- function(x){
  x %>% 
    dplyr::filter(Group == "total",
                  stats == "MAE") %>% 
    select(!c(Group, stats))
}

extract_est_data <- function(method = "lc", kernel = "henderson", nb_est = 10,
                             series = "RETAILx",
                             tp_date = 2020.25,
                             nb_dates_before = 6,
                             fst_degree = 2){
  sep = "_"
  if(method %in% c("lc","ql", "cq", "daf")){
    dir <- "lp"
    full_name <- sprintf("%s_%s", kernel, method)
  }else{
    if(length(grep("arima", method)) >0){
      dir <- "arima"
      full_name <- sep <- ""
    } else if (length(grep("_localic", method)) >0) {
      if (length(grep("_final", method)) >0) {
        dir <- "localic_final"
        full_name <- gsub("localic_final", "h6_d2", method)
      } else {
        dir <- "localic_daf_trunc"
        full_name <- gsub("localic", "d2", method)
      }
    } else if (length(grep("^weight", method)) > 0 ) {
      dir <- "fst"
      full_name <- sprintf("degree%s_%s", fst_degree, method)
    } else {
      dir <- "rkhs"
      full_name <- method
    }
  }
  file = sprintf("results_fredm/%s/%s%s%s.RDS", dir, series, sep, full_name)
  data <- readRDS(file)
  data <- do.call(ts.union, data)
  colnames(data) <- as.character(zoo::as.yearmon(as.numeric(colnames(data))))
  column_to_keep <- !apply(window(data, start = tp_date),2, \(x) all(is.na(x)))
  data <- data[,column_to_keep]
  data <- data[,1:nb_est]
  last_date_est = zoo::na.trim(data[,ncol(data)], sides = "left")
  last_date <- time(last_date_est)[which(is.na(last_date_est))[1] - 1]
  window(data, start = tp_date - nb_dates_before/frequency(data),
         end = last_date)
}

get_all_tp <- function(dossier = "results_fredm/compile_tp_norev/",
                       fst_weights = "weight235",
                       fst_degree = 2) {
  tp_lp <- merge(readRDS(sprintf("%stroughs_lp.RDS", dossier)),
                 readRDS(sprintf("%speaks_lp.RDS", dossier)),
                 by=c("series","kernel", "method")) %>%
    select_var() %>% 
    dplyr::filter(kernel =="henderson")
  
  tp_rkhs <- 
    merge(readRDS(sprintf("%stroughs_rkhs.RDS", dossier)),
          readRDS(sprintf("%speaks_rkhs.RDS", dossier)),
          by=c("series","kernel", "method")) %>%
    select_var()
  
  tp_arima <- 
    merge(readRDS(sprintf("%stroughs_arima.RDS", dossier)),
          readRDS(sprintf("%speaks_arima.RDS", dossier)),
          by=c("series","kernel", "method")) %>%
    select_var()
  
  tp_lic_final <- 
    merge(readRDS(sprintf("%stroughs_localic_daf_trunc.RDS", dossier)),
          readRDS(sprintf("%speaks_localic_daf_trunc.RDS", dossier)),
          by=c("series","kernel", "method", "degree", "h")) %>%
    dplyr::filter(degree == "d2", h == "h6") %>% 
    select_var() %>% mutate(method = sprintf("%s_localic_final", method)) %>% 
    select(!c(degree, h))
  
  tp_lic_final_daf <- 
    merge(readRDS(sprintf("%stroughs_localic_daf_trunc.RDS", dossier)),
          readRDS(sprintf("%speaks_localic_daf_trunc.RDS", dossier)),
          by=c("series","kernel", "method", "degree", "h")) %>%
    dplyr::filter(degree == "d2", h == "h6") %>% 
    select_var() %>% mutate(method = sprintf("%s_localic", method)) %>% 
    select(!c(degree, h))
  tp_fst <- merge(readRDS(sprintf("%stroughs_fst.RDS", dossier)),
                  readRDS(sprintf("%speaks_fst.RDS", dossier)),
                  by=c("series","degree", "weight")) %>% 
    select_var() %>% 
    dplyr::filter(degree == fst_degree, weight %in% fst_weights) %>% 
    select(!c(degree)) %>% 
    rename(method = weight) %>% 
    mutate(kernel = "henderson")
  all_tp <- 
    rbind(tp_lp, 
          tp_rkhs,
          tp_arima,
          tp_lic_final,
          tp_lic_final_daf,
          tp_fst
    ) %>% 
    mutate(method = factor(method,
                           c("lc", "lc_localic_final", "lc_localic",
                             "ql", "ql_localic_final", "ql_localic",
                             "cq","daf", "frf", "gain", "phase","auto_arima",
                             "weight235"),
                           ordered = TRUE)) %>% 
    arrange(series, method)
  all_tp
}


get_all_prevs <- function(series, tp_keep, nb_est = 10, nb_dates_before = 6,
                          fst_weights = "weight235",
                          fst_degree = 2){
  s <- sprintf("data_fredm/byseries/%s.RDS", series)
  tp_date <- as.numeric(tp_keep)
  data <- readRDS(s)
  data <- data[names(data) >= tp_keep][1:nb_est]
  data_info <- readRDS(sub("byseries", "byseriesinfo", s))
  data_info <- data_info[names(data_info) >= tp_keep][1:nb_est]
  
  data_merge <- do.call(ts.union, data)
  data_merge <- window(data_merge, start = tp_date - nb_dates_before/frequency(data_merge))
  colnames(data_merge) <- as.character(zoo::as.yearmon(as.numeric(names(data))))
  
  arima_prevs <- do.call(ts.union, lapply(data, function(y){
    prevs = forecast::auto.arima(y, max.Q = 0, max.D = 0, max.P = 0) %>% 
      forecast::forecast(6)
    ts(c(tail(y,1), prevs$mean), start = tail(time(y),1),
       frequency = frequency(y))
  }))
  arima_prevs <- list(arima_prevs)
  
  rkhs_f <- readRDS("R_filters/rkhs_rw_p3.RDS")$`h=6`
  prev_imp_rkhs <- 
    lapply(c(rkhs_f), function(coefs){
      do.call(ts.union, lapply(data, function(y){
        prevs = implicit_forecast(x=y, coefs = coefs)
        prevs_a = ts(c(tail(y,1), prevs), start = tail(time(y),1),
                     frequency = frequency(y))
        prevs_a
      }))
    })
  names(prev_imp_rkhs) <- c("frf", "gain", "phase")
  list_method <- c("LC", "QL", "CQ", "DAF")
  kernel = "Henderson"
  prevs_imp_lp <- 
    lapply(list_method, function(method){
      do.call(ts.union, lapply(seq_along(data), function(i){
        y <- data[[i]]
        l = 13
        icr = data_info[[i]][sprintf("icr-%s", l)]
        lp_coef = lp_filter(horizon = (l-1)/2,
                            kernel = kernel,
                            endpoints = method,
                            ic = icr)
        prevs = implicit_forecast(x=y, coefs = lp_coef)
        prevs_a = ts(c(tail(y,1), prevs), start = tail(time(y),1),
                     frequency = frequency(y))
        prevs_a
      }))
    })
  names(prevs_imp_lp) <- tolower(list_method)
  
  
  data_info_lic <- readRDS(sprintf("data_fredm/byseriespente_final_nber/%s_h%i.RDS",
                                   series, 6))
  data_info_lic_daf <- readRDS(sprintf("data_fredm/byseriespente_daf_nber/%s.RDS",
                                       series))   
  list_method_localic <- c("LC", "QL")
  prevs_imp_localic <- lapply(list_method_localic, function(method){
    do.call(ts.union, lapply(seq_along(data), function(i){
      y <- data[[i]]
      d = 2
      data_t = data_info_lic[[names(data)[i]]][[method]]
      ratio = data_t[[sprintf("d=%i", d)]] / sqrt(data_t[["sigma2"]])
      icr = 2/(sqrt(pi) * ratio)
      lp_coef = lp_filter2(ic = icr, method = method, h = 6, kernel = kernel)
      prevs = implicit_forecast(x=y, coefs = lp_coef)
      prevs_a = ts(c(tail(y,1), prevs), start = tail(time(y),1),
                   frequency = frequency(y))
      prevs_a
    }))
  })
  names(prevs_imp_localic) <- sprintf("%s_localic_final",tolower(list_method_localic))                              
  prevs_imp_localic_daf <- lapply(list_method_localic, function(method){
    do.call(ts.union, lapply(seq_along(data), function(i){
      y <- data[[i]]
      d = 2
      data_t = data_info_lic_daf[[names(data)[i]]][[method]]
      ratio = data_t[[sprintf("d=%i", d)]] / sqrt(data_t[["sigma2"]])
      icr = 2/(sqrt(pi) * ratio)
      icr[abs(icr) > 12] <- 12
      lp_coef = lp_filter2(ic = icr, method = method, h = 6, kernel = kernel)
      prevs = implicit_forecast(x=y, coefs = lp_coef)
      prevs_a = ts(c(tail(y,1), prevs), start = tail(time(y),1),
                   frequency = frequency(y))
      prevs_a
    }))
  })
  names(prevs_imp_localic_daf) <- sprintf("%s_localic_daf",tolower(list_method_localic))
  
  fst_weights_num <- as.numeric(gsub("weight", "", fst_weights))
  all_coefs <- readRDS(sprintf("R_filters/fst_pdegree%i.RDS",fst_degree))
  weights <- all_coefs$weights[fst_weights_num, ]
  all_coefs <- all_coefs$coefs[fst_weights_num]
  
  prevs_imp_fst <- lapply(seq_along(all_coefs), function(i_fst){
    l = 13
    coef = all_coefs[[i_fst]][[sprintf("h=%i", (l-1)/2)]]
    do.call(ts.union, lapply(seq_along(data), function(i){
      y <- data[[i]]
      prevs = implicit_forecast(x=y, coefs = coef)
      prevs_a = ts(c(tail(y,1), prevs), start = tail(time(y),1),
                   frequency = frequency(y))
      prevs_a
    }))
  })
  names(prevs_imp_fst) <- sprintf("%s_localic_daf",fst_weights)
  
  
  all_prevs = c(prevs_imp_lp, prev_imp_rkhs, prevs_imp_localic, prevs_imp_localic_daf,
                arima_prevs,
                prevs_imp_fst)
  all_prevs = lapply(all_prevs, function(x){
    colnames(x) <- colnames(data_merge)
    x
  })
  list(data = data_merge, prevs = all_prevs)
}


###############
#### Plots ####
###############

format_data_plot  <- function(data){
  time <- time(data)
  freq <- frequency(data)
  dataGraph <- data.frame(cbind(time, data))
  colnames(dataGraph) <- c("date", colnames(data))
  reshape2::melt(dataGraph, id = "date") %>% na.omit()
}
plot_prevs <- function (data, data_prevs, date_tp =2020.25, titre = NULL, sous_titre = NULL, limits_y = NULL,
                        linetype_prev = "dashed", outDec = ",") {
  x_lab = y_lab= NULL;
  n_xlabel = 6 ;n_ylabel = 4; 
  
  dataGraph <- format_data_plot(data)
  dataGraph_prevs <- format_data_plot(data_prevs)
  data_legend = dataGraph_prevs  %>% 
    group_by(variable) %>%
    dplyr::filter(date == max(date)) %>% data.frame()
  p <- ggplot(data = dataGraph, aes(x = date, y = value, group = variable, 
                                    colour = variable)) +
    geom_vline(xintercept = date_tp, linetype = "dotted") +
    geom_line(linewidth = 0.7) + 
    geom_line(data = dataGraph_prevs, 
              aes(x = date, y = value, group = variable, 
                  colour = variable), 
              linewidth = 0.6, linetype = linetype_prev)+
    labs(title = titre, subtitle = sous_titre, x = x_lab, 
         y = y_lab) + 
    scale_x_continuous(breaks = scales::pretty_breaks(n = n_xlabel), 
                       labels = zoo::as.yearmon) + 
    scale_y_continuous(breaks = scales::pretty_breaks(n = n_ylabel), 
                       labels = function(x) format(x, decimal.mark = outDec)
    ) + 
    coord_cartesian(ylim = limits_y) +
    theme_bw()
  p +
    geom_text(aes(x = date, y = value, label =variable, colour = variable), 
              data = data_legend, hjust = 0,
              # check_overlap = FALSE, nudge_y = -0.01,
              size = 2) +
    theme(legend.position = "none",
          panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank(),
          axis.text.x = element_text(size=8, angle=20,
                                     vjust=1.1, hjust=1),
          axis.text.y = element_text(size=8),
          plot.subtitle = element_text(hjust = 0.5,
                                       size=10,face="italic"))
}
plot_est <- function (data, date_tp =2020.25, titre = NULL, sous_titre = NULL, limits_y = NULL, outDec = ",") {
  x_lab = y_lab= NULL
  n_xlabel = 6 ;n_ylabel = 4; 

  dataGraph <- format_data_plot(data)
  data_legend = dataGraph %>% 
    group_by(variable) %>%
    dplyr::filter(date == max(date)) %>% data.frame()
  p <- ggplot(data = dataGraph) +
    geom_vline(xintercept = date_tp, linetype = "dotted") +
    geom_line(mapping = aes(x = date, y = value, group = variable, 
                            colour = variable), linewidth = 0.7) + 
    labs(title = titre, subtitle = sous_titre, x = x_lab, 
         y = y_lab) + 
    scale_x_continuous(breaks = scales::pretty_breaks(n = n_xlabel), 
                       labels = zoo::as.yearmon) + 
    scale_y_continuous(breaks = scales::pretty_breaks(n = n_ylabel), 
                       labels = function(x) format(x, decimal.mark = outDec)) + 
    coord_cartesian(ylim = limits_y) +
    theme_bw()
  p +
    geom_text(aes(x = date, y = value, label =variable, colour = variable), data = data_legend,
              check_overlap = TRUE, hjust = 0, nudge_x = 0.01,
              size = 2) +
    theme(legend.position = "none",
          panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank(),
          axis.text.x = element_text(size=8, angle=20,
                                     vjust=1.1, hjust=1),
          axis.text.y = element_text(size=8),
          plot.subtitle = element_text(hjust = 0.5,
                                       size=10,face="italic"))
}


get_all_plots <- function(all_tp, all_tp_rev = NULL, series, tp_keep, tp_plot = detected_tp[detected_tp$series == series,paste0("X", tp_keep)], fst_degree = 2,
                          nb_est = 10, nb_dates_before = 6) {
  tp_keep_col <- paste0("X", tp_keep)
  column_to_remove <- grep(tp_keep_col,
                           grep("^X", colnames(all_tp),value = TRUE), 
                           value = TRUE, invert = TRUE)
  all_tp_plot <- 
    all_tp %>% dplyr::filter(series %in% !!series) %>% 
    mutate(title = as.character(recode(method, lc = "LC", ql = "QL", 
                                       cq = "CQ", daf = "DAF",
                                       frf = "$b_{q,\\Gamma}$",
                                       gain = "$b_{q,G}$",
                                       phase = "$b_{q,\\phi}$",
                                       auto_arima = "ARIMA",
                                       weight235 = "FST",
                                       lc_localic_final = "LC param. loc. finale", 
                                       lc_localic = "LC param.locale",
                                       ql_localic_final = "QL param. loc. finale", 
                                       ql_localic = "QL param. locale")),
           tex = method %in% c("frf", "gain", "phase"),
           subtitle = sprintf("Déphasage de %i mois",
                              round(.data[[tp_keep_col]]))
    ) %>% 
    select(!c(!!column_to_remove, kernel, length))
  
  if (!is.null(all_tp_rev)) {
    all_tp_rev_plot <- 
      all_tp_rev %>% dplyr::filter(series %in% !!series) %>% 
      mutate(subtitle = sprintf("\n(1ère détection en %i mois)",
                                round(.data[[tp_keep_col]]))
      )
    all_tp_plot[, "subtitle"] <- 
      paste0(all_tp_plot[, "subtitle"],
             all_tp_rev_plot[, "subtitle"])
    # tp_diff <- all_tp_plot[,tp_keep_col] != all_tp_rev_plot[,tp_keep_col]
    # if(any(tp_diff)) {
    #   
    # }
  }
  
  all_mod_est <- lapply(seq_len(nrow(all_tp_plot)), function(i_row) {
    extract_est_data(method = all_tp_plot[i_row, "method"], 
                     kernel = "henderson",
                     series = all_tp_plot[i_row, "series"],
                     tp_date = as.numeric(tp_plot),
                     nb_est = nb_est, nb_dates_before = nb_dates_before,
                     fst_degree = fst_degree)
  })
  all_range <- range(sapply(all_mod_est, range,na.rm = TRUE))
  plots <- lapply(seq_len(nrow(all_tp_plot)), function(i_row) {
    plot_est(all_mod_est[[i_row]], 
             titre = ifelse(all_tp_plot[i_row, "tex"],
                            latex2exp::TeX(all_tp_plot[i_row, "title"]),
                            all_tp_plot[i_row, "title"]),
             sous_titre = all_tp_plot[i_row, "subtitle"],
             limits_y = all_range,
             date_tp = as.numeric(tp_plot)
    )
  })
  names(plots) <- all_tp_plot$method
  plots
}


get_all_plots_prevs <- function(data_prevs,
                                tp_keep,
                                series,
                                all_tp,
                                all_tp_rev = NULL,
                                tp_plot = detected_tp[detected_tp$series == series,paste0("X", tp_keep)],
                                dossier = "results_fredm/compile_tp_norev/",
                                fst_weights = "weight235",
                                fst_degree = 2) {
  tp_keep_col <- paste0("X", tp_keep)
  column_to_remove <- grep(tp_keep_col,
                           grep("^X", colnames(all_tp),value = TRUE), 
                           value = TRUE, invert = TRUE)
  all_tp_plot <- 
    all_tp %>% 
    dplyr::filter(series == !!series)%>% 
    mutate(title = as.character(recode(method, lc = "LC", ql = "QL", 
                                       cq = "CQ", daf = "DAF",
                                       frf = "$b_{q,\\Gamma}$",
                                       gain = "$b_{q,G}$",
                                       phase = "$b_{q,\\phi}$",
                                       auto_arima = "ARIMA",
                                       weight235 = "FST",
                                       lc_localic_final = "LC param. loc. finale", 
                                       lc_localic = "LC param.locale",
                                       ql_localic_final = "QL param. loc. finale", 
                                       ql_localic = "QL param. locale")),
           tex = method %in% c("frf", "gain", "phase"),
           subtitle = sprintf("Déphasage de %i mois",
                              round(.data[[tp_keep_col]]))
    ) %>% 
    select(!c(!!column_to_remove, kernel, !!tp_keep_col, length))
  if (!is.null(all_tp_rev)) {
    all_tp_rev_plot <- 
      all_tp_rev %>% dplyr::filter(series %in% !!series) %>% 
      mutate(subtitle = sprintf("\n(1ère détection en %i mois)",
                                round(.data[[tp_keep_col]]))
      )
    all_tp_plot[, "subtitle"] <- 
      paste0(all_tp_plot[, "subtitle"],
             all_tp_rev_plot[, "subtitle"])
    # tp_diff <- all_tp_plot[,tp_keep_col] != all_tp_rev_plot[,tp_keep_col]
    # if(any(tp_diff)) {
    #   
    # }
  }
  plots <- lapply(seq_len(nrow(all_tp_plot)), function(i_row) {
    plot_prevs(
      data = data_prevs$data, 
      data_prevs = data_prevs$prevs[[i_row]], 
      titre = ifelse(all_tp_plot[i_row, "tex"],
                     latex2exp::TeX(all_tp_plot[i_row, "title"]),
                     all_tp_plot[i_row, "title"]),
      sous_titre = all_tp_plot[i_row, "subtitle"],
      date_tp = as.numeric(tp_plot)
    )
  })
  names(plots) <- all_tp_plot$method
  plots
}

##############################
##### Implicit forecasts #####
##############################

lp_filter2 <- function(icr, method = "LC", h = 6, kernel = "Henderson"){
  all_coef = lapply(icr, function(ic){
    lp_filter(horizon = h,
              kernel = kernel,
              endpoints = method,
              ic = ic)
  })
  sym = all_coef[[1]]@sfilter
  rfilters = lapply(1:h, function(i){
    q=h -i
    all_coef[[i]][,sprintf("q=%i", q)]
  })
  finite_filters(sym, rfilters = rfilters)
}
