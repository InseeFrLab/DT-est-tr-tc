source("R_simul/4_utils.R",encoding = "UTF-8")
library(ggplot2)
library(scales)
library(latex2exp)

tp_lp <- merge(readRDS("results_simul/compile_tp_norev/troughs_lp.RDS"),
                   readRDS("results_simul/compile_tp_norev/peaks_lp.RDS"),
                   by=c("series","kernel", "method")) %>%
  select_var() %>% 
  mutate(method2 = sprintf("%s_%s", kernel, method))
tp_lp$method %>% unique()
apply(!is.na(tp_lp %>% dplyr::filter(!kernel %in% c("uniform", "gaussian"))),2, all)
tp_lp %>% 
  group_by(kernel)
apply(!is.na(tp_lp %>% dplyr::filter(method == "lc")),2, all)
apply(!is.na(tp_lp %>% dplyr::filter(method == "QL")),2, all)
apply(!is.na(tp_lp %>% dplyr::filter(method == "CQ")),2, all)
apply(!is.na(tp_lp %>% dplyr::filter(method == "DAF")),2, all)

# Graphique sur le dephasage
format_table_tp <- function(x){
  x %>% 
    tidyr::pivot_longer(
      cols = starts_with("x"), 
      names_to = "name",
      values_to = "value"
    ) %>% 
    unique_series_pivot() %>% 
    mutate(variability = recode(variability,
                                lowvariability = "Faible variabilité",
                                mediumvariability = "Variabilité moyenne",
                                highvariability = "Forte variabilité")) %>% 
  na.omit()
}
data_tp <- tp_lp %>% 
  format_table_tp()
all_p <- lapply(c("lc", "ql", "cq", "daf"), function(m) {
  ggplot(data_tp %>% 
           dplyr::filter(method == m),aes(x=kernel, y = value))+ 
    geom_boxplot() +
    facet_wrap(vars(variability), ncol = 1) + theme_bw() +
    labs(y="Déphasage", x = NULL,
         title = m) + 
    theme(axis.text.x = element_text( margin = margin(10, 0, 0, 0), vjust = 1))
})
names(all_p) <- c("lc", "ql", "cq", "daf")

all_p$lc
all_p$ql
all_p$cq
all_p$daf
# ggMultisave("DT/img/simulations/phase_shift_simul", 
#             plot = p,
#             width = 10, height = 6)

###################################################
################ With revisions ###################
###################################################

tp_lp <- merge(readRDS("results_simul/compile_tp/troughs_lp.RDS"),
               readRDS("results_simul/compile_tp/peaks_lp.RDS"),
               by=c("series","kernel", "method")) %>%
  select_var() %>% 
  mutate(method2 = sprintf("%s_%s", kernel, method))

data_tp <- tp_lp %>% 
  format_table_tp()
all_p <- lapply(c("lc", "ql", "cq", "daf"), function(m) {
  ggplot(data_tp %>% 
           dplyr::filter(method == m),aes(x=kernel, y = value))+ 
    geom_boxplot() +
    facet_wrap(vars(variability), ncol = 1) + theme_bw() +
    labs(y="Déphasage", x = NULL,
         title = m) + 
    theme(axis.text.x = element_text( margin = margin(10, 0, 0, 0), vjust = 1))
})
names(all_p) <- c("lc", "ql", "cq", "daf")

all_p$lc
all_p$ql
all_p$cq
all_p$daf

ggMultisave("DT/img/simulations/phase_shift_simul_rev", 
            plot = p,
            width = 10, height = 6)
