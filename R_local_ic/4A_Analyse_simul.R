source("R_simul/4_utils.R",encoding = "UTF-8")
library(ggplot2)
library(scales)

all_tp <- merge(readRDS("results_simul/compile_tp_norev/troughs_lp.RDS"),
                readRDS("results_simul/compile_tp_norev/peaks_lp.RDS"),
                by=c("series","kernel", "method")) %>%
  select_var()
all_rev_fe <- readRDS("results_simul/compile_revisions/lp_fe_rev.RDS") %>% 
  select_series() %>% 
  select_mae()
all_rev_ce <- readRDS("results_simul/compile_revisions/lp_ce_rev.RDS") %>% 
  select_series() %>% 
  select_mae()

all_tp_rkhs <- 
  merge(readRDS("results_simul/compile_tp_norev/troughs_rkhs.RDS"),
        readRDS("results_simul/compile_tp_norev/peaks_rkhs.RDS"),
        by=c("series","kernel", "method")) %>%
  select_var()
all_rev_rkhs_fe <- readRDS("results_simul/compile_revisions/rkhs_fe_rev.RDS") %>% 
  select_series() %>% 
  select_mae()
all_rev_rkhs_ce <- readRDS("results_simul/compile_revisions/rkhs_ce_rev.RDS") %>% 
  select_series() %>% 
  select_mae()
all_tp_rkhs <- rbind(all_tp %>% mutate(article = "lpp"), 
                     all_tp_rkhs %>% mutate(article = "rkhs"))
all_rev_rkhs_fe <- rbind(all_rev_fe %>% mutate(article = "lpp"), 
                         all_rev_rkhs_fe %>% mutate(article = "rkhs"))
all_rev_rkhs_ce <- rbind(all_rev_ce %>% mutate(article = "lpp"), 
                         all_rev_rkhs_ce %>% mutate(article = "rkhs"))

all_tp_arima <- 
  merge(readRDS("results_simul/compile_tp_norev/troughs_arima.RDS"),
        readRDS("results_simul/compile_tp_norev/peaks_arima.RDS"),
        by=c("series","kernel", "method")) %>%
  select_var()
all_rev_arima_fe <- readRDS("results_simul/compile_revisions/arima_fe_rev.RDS") %>% 
  select_series() %>% 
  select_mae()
all_rev_arima_ce <- readRDS("results_simul/compile_revisions/arima_ce_rev.RDS") %>% 
  select_series() %>% 
  select_mae()


all_tp_lic <- merge(readRDS("results_simul/compile_tp_norev/troughs_localic_lp.RDS"),
                readRDS("results_simul/compile_tp_norev/peaks_localic_lp.RDS"),
                by=c("series", "kernel", "h", "degree", "method")) %>%
  select_var()%>% 
  mutate(variability = factor(variability,
                              levels = c("lowvariability","mediumvariability","highvariability"),
                              ordered = TRUE))
all_tp_lic_final <- merge(readRDS("results_simul/compile_tp_norev/troughs_localic_final.RDS"),
                    readRDS("results_simul/compile_tp_norev/peaks_localic_final.RDS"),
                    by=c("series", "kernel", "h", "degree", "method")) %>%
  select_var()%>% 
  mutate(variability = factor(variability,
                              levels = c("lowvariability","mediumvariability","highvariability"),
                              ordered = TRUE))
all_tp_lic_daf <- merge(readRDS("results_simul/compile_tp_norev/troughs_localic_daf.RDS"),
                    readRDS("results_simul/compile_tp_norev/peaks_localic_daf.RDS"),
                    by=c("series", "kernel", "h", "degree", "method")) %>%
  select_var()%>% 
  mutate(variability = factor(variability,
                              levels = c("lowvariability","mediumvariability","highvariability"),
                              ordered = TRUE))
all_tp_lic_daf_trunc <- merge(readRDS("results_simul/compile_tp_norev/troughs_localic_daf_trunc.RDS"),
                        readRDS("results_simul/compile_tp_norev/peaks_localic_daf_trunc.RDS"),
                        by=c("series", "kernel", "h", "degree", "method")) %>%
  select_var() %>%
  mutate(variability = factor(variability,
                              levels = c("lowvariability","mediumvariability","highvariability"),
                              ordered = TRUE))
all_rev_fe_lic <- readRDS("results_simul/compile_revisions/localic_daf_fe_rev.RDS") %>% 
  select_series() %>% 
  select_mae()
all_rev_ce_lic <- readRDS("results_simul/compile_revisions/localic_daf_ce_rev.RDS") %>% 
  select_series() %>% 
  select_mae()

all_tp <- rbind(all_tp_rkhs, 
                all_tp_arima %>% mutate(article = "arima")) %>% 
  mutate(method = factor(method,levels = c("lc","ql","cq","daf", "frf", "gain", "phase", "auto_arima"),
                         ordered = TRUE),
         variability = factor(variability,
                              levels = c("lowvariability","mediumvariability","highvariability"),
                              ordered = TRUE),
         kernel = tolower(kernel))
all_rev_fe <- rbind(all_rev_rkhs_fe, 
                    all_rev_arima_fe %>% mutate(article = "rkhs"),
                    all_rev_fe_lic %>% dplyr::filter(h == "h6", degree == "d3") %>% 
                      mutate(article = "local ic", method = paste0(method, "_local_ic"))%>% 
                      select(!c(h, degree))) %>% 
  mutate(method = factor(method,levels = c("lc", "lc_local_ic","ql", "ql_local_ic","cq","daf", "frf", "gain", "phase", "auto_arima"),
                         ordered = TRUE),
         variability = factor(variability,
                              levels = c("lowvariability","mediumvariability","highvariability"),
                              ordered = TRUE),
         kernel = tolower(kernel))
all_rev_ce <- rbind(all_rev_rkhs_ce, 
                    all_rev_arima_ce %>% mutate(article = "rkhs"),
                    all_rev_ce_lic %>% dplyr::filter(h == "h6", degree == "d3") %>% 
                      mutate(article = "local ic", method = paste0(method, "_local_ic"))%>% 
                      select(!c(h, degree))) %>% 
  mutate(method = factor(method,levels = c("lc", "lc_local_ic","ql", "ql_local_ic","cq","daf", "frf", "gain", "phase", "auto_arima"),
                         ordered = TRUE),
         variability = factor(variability,
                              levels = c("lowvariability","mediumvariability","highvariability"),
                              ordered = TRUE),
         kernel = tolower(kernel))

normalise_rev <- function(x, ref = "lc", suff = "^(rev|X)"){
  # x <- all_rev_ce
  ref = x[(x$method == "lc") & (x$kernel == "henderson"),grep(suff,colnames(x)) ]
  for(k in unique(x$kernel)){
    for(m in unique(x$method)){
      if(nrow(x[x$method == m & x$kernel == k,grep(suff,colnames(x))]) > 0){
        
        x[x$method == m & x$kernel == k,grep(suff,colnames(x))] <- 
          x[x$method == m& x$kernel == k,grep(suff,colnames(x))] / ref
      }
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
    group_by(variability, kernel, method) %>%
    summarise(across(
      .cols = where(is.numeric), 
      .fns = list(Mean = \(x) round(mean(x),digits)), 
      .names = "{col}"
    )) %>% 
    select(!c(rev.q6:rev.q10, length)) %>% 
    data.frame() %>%  dplyr::filter(kernel == "henderson")  %>% 
    select(!c(kernel))
}
rev_tot = rbind(all_rev_fe %>% summarise_ref(),
                all_rev_ce %>% summarise_ref())
# rev_rel = rbind(all_rev_fe %>% summarise_ref(normalise = TRUE),
#                 all_rev_ce %>% summarise_ref(normalise = TRUE))
rev_tot %>% dplyr::filter(variability == "mediumvariability")
# rev_rel %>% dplyr::filter(variability == "mediumvariability")
all_rev_fe %>% summarise_ref() %>% 
  dplyr::filter(method %in% c("lc", "ql", "lc_local_ic", "ql_local_ic"))

all_rev_ce %>% summarise_ref() %>% 
  dplyr::filter(method %in% c("lc", "ql", "lc_local_ic", "ql_local_ic"))


### Déphasage Henderson 
all_tp %>%
  dplyr::filter(kernel %in% c("henderson")) %>% 
  unique_series() %>%
  group_by(kernel, method, variability) %>%
  summarise_if(is.numeric, \(x) sum(!is.na(x))) %>%
  select (-length) %>% 
  mutate(sum = rowSums(across(where(is.numeric))))  %>%
  select(!starts_with("X")) %>% 
  dplyr::filter(method == "lc") %>% 
  data.frame %>% 
  tail(3)


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
data_tp <- all_tp %>% format_table_tp() %>% 
  mutate(method = recode(method, lc = "LC", ql = "QL",
                         cq = "CQ", daf = "DAF",
                         frf = "b['q, '] [Gamma]",
                         gain = "b['q, '] [G]",
                         phase = "b['q, '] [phi]",
                         auto_arima = "ARIMA"))
data_tp_lic <- all_tp_lic  %>% 
  mutate(method = paste(method, degree,h, sep = "_")) %>%  format_table_tp()
data_tp_lic_final <- all_tp_lic_final %>% 
  mutate(method = paste(method, degree,h, sep = "_")) %>% format_table_tp()
data_tp_lic_daf <- all_tp_lic_daf %>% 
  mutate(method = paste(method, degree,h, sep = "_")) %>% format_table_tp()
data_tp_lic_daf_trunc <- all_tp_lic_daf_trunc %>%
  mutate(method = paste(method, degree,h, sep = "_")) %>% format_table_tp()

data_tp_compil = rbind(data_tp_lic %>% 
                         dplyr::filter(h == "h6") %>% 
                         select(method, value, variability),
                       data_tp %>% 
                         select(method, value, variability))
data_tp_compil_daf = rbind(data_tp_lic_daf %>% 
                         dplyr::filter(h == "h6") %>% 
                         select(method, value, variability),
                       data_tp %>% 
                         select(method, value, variability))
data_tp_compil_daf_trunc = rbind(data_tp_lic_daf_trunc %>%
                             dplyr::filter(h == "h6") %>%
                             select(method, value, variability),
                           data_tp %>%
                             select(method, value, variability))
data_tp_compil_final = rbind(data_tp_lic_final %>% 
                         dplyr::filter(h == "h6") %>% 
                         select(method, value, variability),
                       data_tp %>% 
                         select(method, value, variability))

data_tp_compil_daf_trunc = rbind(data_tp_lic_daf_trunc %>%
                                   dplyr::filter(h == "h6") %>%
                                   select(method, value, variability),
                                 data_tp %>%
                                   select(method, value, variability))
data_tp_compest = rbind(data_tp_lic_final %>% 
                               dplyr::filter(h == "h6") %>% 
                               select(method, value, variability) %>% 
                               mutate(method = paste0(method, "_final")),
                             data_tp_lic_daf_trunc %>%
                               dplyr::filter(h == "h6") %>%
                               select(method, value, variability))
data_tp_rel <- all_tp %>% 
  normalise_rev() %>% 
  tidyr::pivot_longer(
    cols = starts_with("x"), 
    names_to = "name",
    values_to = "value"
  )%>% dplyr::filter(kernel == "henderson") %>% 
  unique_series_pivot() %>% 
  mutate(method = recode(method, lc = "LC", ql = "QL",
                         cq = "CQ", daf = "DAF",
                         frf = "b['q, '] [gamma]",
                         gain = "b['q, '] [G]",
                         phase = "b['q, '] [phi]",
                         auto_arima = "ARIMA"),
         variability = recode(variability,
                              lowvariability = "Faible variabilité",
                              mediumvariability = "Variabilité moyenne",
                              highvariability = "Forte variabilité")) %>% 
  na.omit()
data_tp=data_tp[data_tp$value<=30,]
p = ggplot(data_tp ,aes(x=method, y = value))+ 
  geom_boxplot() +
  facet_wrap(vars(variability), ncol = 1) + theme_bw() +
  labs(y="Déphasage", x = NULL) +# geom_jitter(width = 0.2, alpha = 0.1) +
  scale_x_discrete(labels = label_parse())
p 

# Conclusion garder h6
ggplot(data_tp_lic ,aes(x=method, y = value))+ 
  geom_boxplot() +
  facet_wrap(vars(variability), ncol = 1) + theme_bw() +
  labs(y="Déphasage", x = NULL,title = "IC connu en h-6") +# geom_jitter(width = 0.2, alpha = 0.1) +
  scale_x_discrete(labels = label_parse())

# Conclusion : garder h6 aussi
ggplot(data_tp_lic_final ,aes(x=method, y = value))+ 
  geom_boxplot() +
  facet_wrap(vars(variability), ncol = 1) + theme_bw() +
  labs(y="Déphasage", x = NULL, title = "IC final estimé") +# geom_jitter(width = 0.2, alpha = 0.1) +
  scale_x_discrete(labels = label_parse())


ggplot(data_tp_lic_daf ,aes(x=method, y = value))+ 
  geom_boxplot() +
  facet_wrap(vars(variability), ncol = 1) + theme_bw()  +
  labs(y="Déphasage", x = NULL, title = "IC estimé par DAF") +# geom_jitter(width = 0.2, alpha = 0.1) +
  scale_x_discrete(labels = label_parse())

ggplot(data_tp_compil ,aes(x=method, y = value))+ 
  geom_boxplot() +
  facet_wrap(vars(variability), ncol = 1) + theme_bw()  +
  labs(y="Déphasage", x = NULL) +# geom_jitter(width = 0.2, alpha = 0.1) +
  scale_x_discrete(labels = label_parse())

ggplot(data_tp_compil_final ,aes(x=method, y = value))+ 
  geom_boxplot() +
  facet_wrap(vars(variability), ncol = 1) +theme_bw()  +
  labs(y="Déphasage", x = NULL) +# geom_jitter(width = 0.2, alpha = 0.1) +
  scale_x_discrete(labels = label_parse())

ggplot(data_tp_compest,
       aes(x=method, y = value))+ 
  geom_boxplot() +
  facet_wrap(vars(variability), ncol = 1) + theme_bw()  +
  labs(y="Déphasage", x = NULL, title = "IC estimé par DAF") +# geom_jitter(width = 0.2, alpha = 0.1) +
  scale_x_discrete(labels = label_parse())

ggplot(data_tp_compil_daf ,aes(x=method, y = value))+ 
  geom_boxplot() +
  facet_wrap(vars(variability), ncol = 1) + theme_bw()  +
  labs(y="Déphasage", x = NULL, title = "IC DAF") +# geom_jitter(width = 0.2, alpha = 0.1) +
  scale_x_discrete(labels = label_parse())
ggplot(data_tp_compil_daf_trunc ,aes(x=method, y = value))+ 
  geom_boxplot() +
  facet_wrap(vars(variability), ncol = 1) + theme_bw()  +
  labs(y="Déphasage", x = NULL, title = "IC DAF trunc") +# geom_jitter(width = 0.2, alpha = 0.1) +
  scale_x_discrete(labels = label_parse())


p2 <- ggplot(data_tp %>% dplyr::filter(variability == "Variabilité moyenne") ,aes(x=method, y = value))+ 
  geom_boxplot() +
  facet_wrap(vars(variability), ncol = 1) + theme_bw() +
  labs(y="Déphasage", x = NULL) +# geom_jitter(width = 0.2, alpha = 0.1) +
  scale_x_discrete(labels = label_parse())

ggsave("Slides/img/simulations/phase_shift_simul.pdf", 
       plot = p2,
       width = 8,height = 5)


data_tp_rel=data_tp_rel[data_tp_rel$value<=8,]

ggplot(data_tp_rel ,aes(x=method, y = value))+ 
  geom_boxplot() +
  facet_wrap(vars(variability), ncol = 1) + theme_bw() +
  labs(y="Déphasage", x = NULL) +# geom_jitter(width = 0.2, alpha = 0.1) +
  scale_x_discrete(labels = label_parse())

## Révisions
all_tp %>%
  dplyr::filter(method == "lc",
         !kernel %in% c("gaussian",
                        "parabolic", 
                        "uniform")) %>% 
  unique_series() %>%
  group_by(kernel, method, variability) %>%
  summarise_if(is.numeric, \(x) sum(!is.na(x))) %>%
  select (-length) %>% 
  mutate(sum = rowSums(across(where(is.numeric))))  %>%
  select(!starts_with("X")) %>% 
  dplyr::filter(method == "lc") %>% 
  data.frame %>% 
  tail(3)
all_tp %>%
  dplyr::filter(kernel %in% c("henderson")) %>% 
  unique_series() %>%
  group_by(kernel, method, variability) %>%
  summarise_if(is.numeric, \(x) sum(!is.na(x))) %>%
  select (-length) %>% 
  mutate(sum = rowSums(across(where(is.numeric))))  %>%
  select(!starts_with("X")) %>% 
  dplyr::filter(method == "lc") %>% 
  data.frame %>% 
  tail(3)
all_tp %>%
  dplyr::filter(method == "lc") %>% 
  # unique_series() %>%
  group_by(kernel, method, variability) %>%
  summarise_if(is.numeric, \(x) sum(!is.na(x))) %>%
  select (-length) %>% 
  mutate(sum = rowSums(across(where(is.numeric))))  %>%
  select(!starts_with("X")) %>%
  dplyr::filter(method == "lc") %>% 
  group_by(variability) %>% 
  mutate(toto = paste(kernel[sum == max(sum)], collapse = " - ")) %>% 
  data.frame
unique(all_tp$series)
ggplot(pivot_data %>% 
         dplyr::filter(method == "lc"),aes(x=kernel, y = value))+ 
  geom_boxplot() +
  facet_wrap(vars(variability), ncol = 1) + theme_bw() +
  labs(title="Dephasage")

all_tp %>% 
  dplyr::filter(method == "lc")
