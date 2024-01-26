# Pour que ce programme puisse tourner, il faut également avoir lancé ceux sous R_local_ic

source("R_simul/4_utils.R",encoding = "UTF-8")
library(ggplot2)
library(scales)

fst_weights <- c("weight235")
all_methods <- c("lp", "rkhs", "arima", "localic_daf_trunc", "localic_final", "fst")

for (crit in c("ce", "fe")) {
  for (method in all_methods) {
    data <- readRDS(sprintf("results_simul/compile_revisions/%s_%s_rev.RDS", method, crit)) %>% 
      select_series() %>% 
      select_mae()
    if (method == "lp")
      data <- data %>% dplyr::filter(kernel == "henderson")
    if (method %in% c("localic_daf_trunc", "localic_final")){
      suff <- ifelse(method == "localic_final", "_final", "")
      data <- data  %>% dplyr::filter(degree == "d2", h == "h6") %>% 
        mutate(method = sprintf("%s_localic%s", method, 
                                suff)) %>% 
        select(!c(degree, h))
    }
    if (method == "fst")
      data <- data %>% 
        dplyr::filter(degree == 2, weight == fst_weights) %>% 
        select(!c(degree, weight)) %>% 
        mutate(kernel = "henderson", method = fst_weights)
    data <- data %>% select(!kernel)
    assign(sprintf("rev_%s_%s", crit, method), data) 
  }
  all_data <- do.call(rbind, mget(sprintf("rev_%s_%s", crit, all_methods))) %>% 
    mutate(method = factor(method,c("lc", "lc_localic_final", "lc_localic",
                                    "ql", "ql_localic_final", "ql_localic",
                                    "cq","daf", "frf", "gain", "phase","auto_arima",
                                    "weight235"),
                           ordered = TRUE),
           variability = factor(variability,
                                levels = c("lowvariability","mediumvariability","highvariability"),
                                ordered = TRUE)) 
  assign(sprintf("all_rev_%s", crit), all_data)
}

x = all_rev_fe
normalise_rev <- function(x, ref = "lc", suff = "^(rev|X)"){
  ref = x[(x$method == "lc"),grep(suff,colnames(x)) ]
  for(m in unique(x$method)){
    if(nrow(x[x$method == m,grep(suff,colnames(x))]) > 0){
      x[x$method == m,grep(suff,colnames(x))] <- 
        x[x$method == m,grep(suff,colnames(x))] / ref
    }
  }
  x
}
summarise_ref <- function(x, normalise = FALSE){
  if(normalise){
    x = x %>% normalise_rev()
    digits = 1
  } else{
    digits = 2
  }
  x %>% 
    group_by(variability, method) %>%
    summarise(across(
      .cols = where(is.numeric), 
      .fns = list(Mean = \(x) round(mean(x),digits)), 
      .names = "{col}"
    )) %>% 
    select(!c(rev.q6:rev.q10, length)) %>% 
    data.frame()
}
rev_tot = rbind(all_rev_fe %>% summarise_ref(),
                all_rev_ce %>% summarise_ref())
rev_rel = rbind(all_rev_fe %>% summarise_ref(normalise = TRUE),
                all_rev_ce %>% summarise_ref(normalise = TRUE))
rev_tot %>% dplyr::filter(variability == "mediumvariability")
rev_rel %>% dplyr::filter(variability == "mediumvariability")
rev_tot %>% dplyr::filter(method %in% c("lc", "gain"))

rev_table <- rev_tot %>% dplyr::filter(variability == "mediumvariability")%>%
  select(!c(variability)) %>% 
  mutate(method = recode(method, lc = "LC", ql = "QL",
                         cq = "CQ", daf = "DAF",
                         frf = "$b_{q,\\Gamma}$",
                         gain = "$b_{q,G}$",
                         phase = "$b_{q,\\varphi}$",
                         auto_arima = "ARIMA",
                         weight235 = "FST",
                         lc_localic_final = "LC param. locale (finale)", 
                         lc_localic = "LC param. locale",
                         ql_localic_final = "QL param. locale (finale)", 
                         ql_localic = "QL param. locale")) %>% 
  rename_at(vars(starts_with("rev")), function(x){
    sprintf("$q=%s$", gsub("rev.q","",x))
  }) %>% 
  rename(`Méthode` = method)
library(kableExtra)
saveRDS(rev_table, file = "DT/data/simulations_revisions.RDS")
title = "Moyenne des écarts relatifs des révisions pour les différents filtres sur les séries à variabilité moyenne."
pack_row_index = c("$MAE_{fe}(q) = \\mathbb E\\\\left[\\\\left|(y_{t|t+q} -  y_{t|last})/y_{t|last}\\\\right|\\\\right]$" = nrow(rev_table) / 2,
                   "$MAE_{ce}(q)=\\mathbb E\\\\left[
\\\\left|(y_{t|t+q} - y_{t|t+q+1})/y_{t|t+q+1}\\\\right|
\\\\right]$" = nrow(rev_table) / 2)
if(is_html){
  names(pack_row_index) <- gsub("\\\\","\\",names(pack_row_index), fixed = TRUE)
}
rev_table  %>%
  kable(format.args = list(digits = 2,
                           decimal.mark = ","),
        align = "c", booktabs = T, row.names = FALSE, 
        escape = FALSE, caption = title) %>%  
  kable_styling(latex_options=c(#"striped",  
    "hold_position")) %>% 
  pack_rows(index = pack_row_index, escape = FALSE)
