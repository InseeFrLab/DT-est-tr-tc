source("R_fredm/4_utils.R",encoding = "UTF-8")
library(patchwork)

all_tp <- merge(readRDS("results_fredm/compile_tp_norev/troughs_lp.RDS"),
                readRDS("results_fredm/compile_tp_norev/peaks_lp.RDS"),
                by=c("series","kernel", "method")) %>%
  select_var()

all_tp_rkhs <- 
  merge(readRDS("results_fredm/compile_tp_norev/troughs_rkhs.RDS"),
        readRDS("results_fredm/compile_tp_norev/peaks_rkhs.RDS"),
        by=c("series","kernel", "method")) %>%
  select_var()

all_tp_rkhs <- rbind(all_tp %>% mutate(article = "lpp"), 
                     all_tp_rkhs %>% mutate(article = "rkhs"))
all_rev_rkhs_fe <- rbind(all_rev_fe %>% mutate(article = "lpp"), 
                         all_rev_rkhs_fe %>% mutate(article = "rkhs"))
all_rev_rkhs_ce <- rbind(all_rev_ce %>% mutate(article = "lpp"), 
                         all_rev_rkhs_ce %>% mutate(article = "rkhs"))

all_tp_arima <- 
  merge(readRDS("results_fredm/compile_tp_norev/troughs_arima.RDS"),
        readRDS("results_fredm/compile_tp_norev/peaks_arima.RDS"),
        by=c("series","kernel", "method")) %>%
  select_var() %>% mutate(article = "arima")

all_tp_lic <- 
  merge(readRDS("results_fredm/compile_tp_norev/troughs_lp_localic.RDS"),
        readRDS("results_fredm/compile_tp_norev/peaks_lp_localic.RDS"),
        by=c("series","h", "degree", "method")) %>%
  select_var() %>% 
  mutate(article = "localic",
         kernel = "henderson",
         method = paste(method,h,degree,sep="_"))
all_tp_lic <- all_tp_lic[,colnames(all_tp_rkhs)]


all_tp <- all_tp_rkhs %>%
  rbind(all_tp_arima, all_tp_lic) %>% 
  mutate(method = factor(method,levels = c("lc","ql","cq","daf", "frf", "gain", "phase","auto_arima", "lc_h3_d2", "lc_h3_d3", "ql_h3_d3", "lc_h4_d2", "lc_h4_d3", 
                                           "ql_h4_d3", "lc_h5_d2", "lc_h5_d3", "ql_h5_d3", "lc_h6_d2", "lc_h6_d3", 
                                           "ql_h6_d3"),
                         ordered = TRUE),
         kernel = tolower(kernel))
all_rev_fe <- all_rev_rkhs_fe %>% 
  mutate(method = factor(method,levels = c("lc","ql","cq","daf", "frf", "gain", "phase"),
                         ordered = TRUE),
         kernel = tolower(kernel))
all_rev_ce <- all_rev_rkhs_ce %>% 
  mutate(method = factor(method,levels = c("lc","ql","cq","daf", "frf", "gain", "phase"),
                         ordered = TRUE),
         kernel = tolower(kernel))


series <- "RETAILx"
tp_keep = "2020.25"
column_to_remove <- grep(tp_keep, grep("^X", colnames(all_tp),value = TRUE), value = TRUE, invert = TRUE)
all_tp = 
  all_tp %>% dplyr::filter(series %in% !!series) %>% 
  mutate(method2 = recode(method, lc = "Linear-Constant~(LC)",
                          ql = "Quadratic-Linear~(QL)",
                          cq = "Cubic-quadratic~(CQ)", daf = "DAF",
                          frf = "b['q, '] [Gamma]",
                          gain = "b['q, '] [G]",
                          phase = "b['q, '] [phi]",
                          auto_arima = "ARIMA")) %>% 
  mutate(title = sprintf("%s%s",
                         ifelse(kernel == "henderson", as.character(method2), ""),
                         ifelse((kernel != "henderson") & article == "lpp",
                                sprintf("Noyau~%s", kernel), ""
                         )),
         subtitle = sprintf("Déphasage de %i mois",
                            round(X2020.25))) %>%
  select(!c(!!column_to_remove, article))




all_mod <- list(lc_h = list(kernel = "henderson",
                            method = "lc"),
                ql_h = list(kernel = "henderson",
                            method = "ql"),
                cq_h = list(kernel = "henderson",
                            method = "cq"),
                daf_h = list(kernel = "henderson",
                             method = "daf"),
                rkhs_frf = list(kernel = "henderson",
                                method = "frf"),
                rkhs_gain = list(kernel = "henderson",
                                 method = "gain"),
                rkhs_phase = list(kernel = "henderson",
                                  method = "phase"),
                arima = list(kernel = "henderson",
                             method = "auto_arima"),
                lc_h6_d2 = list(kernel = "henderson",
                                method = "lc_h6_d2"),
                lc_h6_d3 = list(kernel = "henderson",
                                method = "lc_h6_d3"),
                ql_h6_d3 = list(kernel = "henderson",
                                method = "ql_h6_d3")
)
all_titles = lapply(all_mod, function(x){
  title = all_tp %>% dplyr::filter(kernel == x$kernel,
                            method == x$method) %>% 
    select(c(title, subtitle))
  # gsub(" ","~", title)
  title
})

all_mod_est <- lapply(all_mod, \(x) do.call(extract_est_data, x))
all_range <- range(sapply(all_mod_est, range,na.rm = TRUE))

plots <- lapply(names(all_mod_est), 
                \(x,...)plot_est(all_mod_est[[x]], 
                                 titre = parse(text = all_titles[[x]][1]),
                                 sous_titre = all_titles[[x]][2],
                                 ...),
                limits_y = all_range)

wrap_plots(plots[1:4])
wrap_plots(plots[(5:8)])
wrap_plots(plots[-(1:8)])



