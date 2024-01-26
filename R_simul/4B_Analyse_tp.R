# Pour que ce programme puisse tourner, il faut également avoir lancé ceux sous R_local_ic

source("R_simul/4_utils.R",encoding = "UTF-8")
library(ggplot2)
library(scales)
library(latex2exp)

tp_lp <- merge(readRDS("results_simul/compile_tp_norev/troughs_lp.RDS"),
                   readRDS("results_simul/compile_tp_norev/peaks_lp.RDS"),
                   by=c("series","kernel", "method")) %>%
  select_var()

tp_rkhs <- 
  merge(readRDS("results_simul/compile_tp_norev/troughs_rkhs.RDS"),
        readRDS("results_simul/compile_tp_norev/peaks_rkhs.RDS"),
        by=c("series","kernel", "method")) %>%
  select_var()
tp_arima <- 
  merge(readRDS("results_simul/compile_tp_norev/troughs_arima.RDS"),
        readRDS("results_simul/compile_tp_norev/peaks_arima.RDS"),
        by=c("series","kernel", "method")) %>%
  select_var()

tp_lic_final <- merge(readRDS("results_simul/compile_tp_norev/troughs_localic_final.RDS"),
                          readRDS("results_simul/compile_tp_norev/peaks_localic_final.RDS"),
                          by=c("series", "kernel", "h", "degree", "method"))  %>%
  dplyr::filter(degree == "d2", h == "h6") %>% 
  select_var() %>% mutate(method = sprintf("%s_localic_final", method)) %>% 
  select(!c(degree, h))
tp_lic_daf_trunc <- merge(readRDS("results_simul/compile_tp_norev/troughs_localic_daf_trunc.RDS"),
                              readRDS("results_simul/compile_tp_norev/peaks_localic_daf_trunc.RDS"),
                              by=c("series", "kernel", "h", "degree", "method")) %>%
  dplyr::filter(degree == "d2", h == "h6") %>% 
  select_var() %>% mutate(method = sprintf("%s_localic", method)) %>% 
  select(!c(degree, h))

fst_weights <- c("weight235")
tp_fst <- merge(readRDS("results_simul/compile_tp_norev/troughs_fst.RDS"),
                    readRDS("results_simul/compile_tp_norev/peaks_fst.RDS"),
                    by=c("series","degree", "weight")) %>% 
  select_var() %>% 
  dplyr::filter(degree == 2, weight %in% fst_weights) %>% 
  select(!c(degree)) %>% 
  rename(method = weight) %>% 
  mutate(kernel = "henderson")


all_tp <- rbind(tp_lp,
                tp_rkhs,
                tp_arima,
                tp_lic_final,
                tp_lic_daf_trunc,
                tp_fst) %>% 
  mutate(method = factor(method,c("lc", "lc_localic_final", "lc_localic",
                                  "ql", "ql_localic_final", "ql_localic",
                                  "cq","daf", "frf", "gain", "phase","auto_arima",
                                  "weight235"),
                         ordered = TRUE),
         variability = factor(variability,
                              levels = c("lowvariability","mediumvariability","highvariability"),
                              ordered = TRUE),
         kernel = tolower(kernel))


# Graphique sur le dephasage
format_table_tp <- function(x){
  x %>% 
    tidyr::pivot_longer(
      cols = starts_with("x"), 
      names_to = "name",
      values_to = "value"
    )%>% dplyr::filter(kernel == "henderson") %>% 
    unique_series_pivot() %>% 
    mutate(variability = recode(variability,
                                lowvariability = "Faible variabilité",
                                mediumvariability = "Variabilité moyenne",
                                highvariability = "Forte variabilité")) %>% 
    na.omit()
}
data_tp <- all_tp %>% format_table_tp()


legende <- c(lc = "LC", ql = "QL", 
             cq = "CQ", daf = "DAF",
             frf = TeX("$b_{q,\\Gamma}$"),
             gain = TeX("$b_{q,G}$"),
             phase = TeX("$b_{q,\\phi}$"),
             auto_arima = "ARIMA",
             weight235 = "FST",
             lc_localic_final = "LC param.\nloc. finale", 
             lc_localic = "LC param.\nlocale",
             ql_localic_final = "QL param.\nloc. finale", 
             ql_localic = "QL param.\nlocale")
p <- ggplot(data_tp ,aes(x=method, y = value))+ 
  geom_boxplot() +
  facet_wrap(vars(variability), ncol = 1) + theme_bw() +
  labs(y="Déphasage", x = NULL) +
  scale_x_discrete(labels = legende) + 
  theme(axis.text.x = element_text( margin = margin(10, 0, 0, 0), vjust = 1))
p


ggMultisave("DT/img/simulations/phase_shift_simul", 
            plot = p,
            width = 10, height = 6)

###################################################
################ With revisions ###################
###################################################

all_tp_lp <- merge(readRDS("results_simul/compile_tp/troughs_lp.RDS"),
                   readRDS("results_simul/compile_tp/peaks_lp.RDS"),
                   by=c("series","kernel", "method")) %>%
  select_var()

all_tp_rkhs <- 
  merge(readRDS("results_simul/compile_tp/troughs_rkhs.RDS"),
        readRDS("results_simul/compile_tp/peaks_rkhs.RDS"),
        by=c("series","kernel", "method")) %>%
  select_var()
all_tp_arima <- 
  merge(readRDS("results_simul/compile_tp/troughs_arima.RDS"),
        readRDS("results_simul/compile_tp/peaks_arima.RDS"),
        by=c("series","kernel", "method")) %>%
  select_var()

all_tp_lic_final <- merge(readRDS("results_simul/compile_tp/troughs_localic_final.RDS"),
                          readRDS("results_simul/compile_tp/peaks_localic_final.RDS"),
                          by=c("series", "kernel", "h", "degree", "method"))  %>%
  dplyr::filter(degree == "d2", h == "h6") %>% 
  select_var() %>% mutate(method = sprintf("%s_localic_final", method)) %>% 
  select(!c(degree, h))
all_tp_lic_daf_trunc <- merge(readRDS("results_simul/compile_tp/troughs_localic_daf_trunc.RDS"),
                              readRDS("results_simul/compile_tp/peaks_localic_daf_trunc.RDS"),
                              by=c("series", "kernel", "h", "degree", "method")) %>%
  dplyr::filter(degree == "d2", h == "h6") %>% 
  select_var() %>% mutate(method = sprintf("%s_localic", method)) %>% 
  select(!c(degree, h))

fst_weights <- c("weight235")
all_tp_fst <- merge(readRDS("results_simul/compile_tp/troughs_fst.RDS"),
                    readRDS("results_simul/compile_tp/peaks_fst.RDS"),
                    by=c("series","degree", "weight")) %>% 
  select_var() %>% 
  dplyr::filter(degree == 2, weight %in% fst_weights) %>% 
  select(!c(degree)) %>% 
  rename(method = weight) %>% 
  mutate(kernel = "henderson")


all_tp <- rbind(all_tp_lp,
                all_tp_rkhs,
                all_tp_arima,
                all_tp_lic_final,
                all_tp_lic_daf_trunc,
                all_tp_fst) %>% 
  mutate(method = factor(method,c("lc", "lc_localic_final", "lc_localic",
                                  "ql", "ql_localic_final", "ql_localic",
                                  "cq","daf", "frf", "gain", "phase","auto_arima",
                                  "weight235"),
                         ordered = TRUE),
         variability = factor(variability,
                              levels = c("lowvariability","mediumvariability","highvariability"),
                              ordered = TRUE),
         kernel = tolower(kernel))

data_tp <- all_tp %>% format_table_tp() 

legende <- c(lc = "LC", ql = "QL", 
             cq = "CQ", daf = "DAF",
             frf = TeX("$b_{q,\\Gamma}$"),
             gain = TeX("$b_{q,G}$"),
             phase = TeX("$b_{q,\\phi}$"),
             auto_arima = "ARIMA",
             weight235 = "FST",
             lc_localic_final = "LC param.\nloc. finale", 
             lc_localic = "LC param.\nlocale",
             ql_localic_final = "QL param.\nloc. finale", 
             ql_localic = "QL param.\nlocale")

p <- ggplot(data_tp ,aes(x=method, y = value))+ 
  geom_boxplot() +
  facet_wrap(vars(variability), ncol = 1) + theme_bw() +
  labs(y="Déphasage", x = NULL) +
  scale_x_discrete(labels = legende) + 
  theme(axis.text.x = element_text( margin = margin(10, 0, 0, 0), vjust = 1))
p


ggMultisave("DT/img/simulations/phase_shift_simul_rev", 
            plot = p,
            width = 10, height = 6)
