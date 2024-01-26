source("R_simul/4_utils.R",encoding = "UTF-8")

format_fst <- function(x){
  weights_ordered = sprintf("weight%i", sort(unique(as.numeric(gsub("weight", "", x$weight)))))
  x <- x %>% 
    mutate(weight = factor(weight, 
                           levels = weights_ordered,
                           ordered = TRUE))
  if("variability" %in% colnames(x)){
    x <- x %>% 
      mutate(variability = factor(variability,
                                  levels = c("lowvariability","mediumvariability","highvariability"),
                                  ordered = TRUE))
  }
  x
}

all_tp <- merge(readRDS("results_simul/compile_tp_norev/troughs_fst.RDS"),
                readRDS("results_simul/compile_tp_norev/peaks_fst.RDS"),
                by=c("series","degree", "weight")) %>% 
  select_var() %>% 
  format_fst()

min_tp <- all_tp %>% 
  tidyr::pivot_longer(
    cols = starts_with("x"), 
    names_to = "date_tp",
    values_to = "phase_shift"
  ) %>% group_by(variability, weight, degree) %>%
  mutate(
    moy = mean(phase_shift,na.rm = TRUE),
    med = median(phase_shift, na.rm = TRUE)
  ) %>% 
  select(!c(series, length, date_tp, phase_shift)) %>% 
  unique()
min_tp_moy <- min_tp %>% 
  group_by(variability) %>% 
  dplyr::filter(moy == min(moy)) 
min_tp_moy$weight %>% unique() %>% as.character()%>% dput()
all_rev_fe <- select_series(readRDS("results_simul/compile_revisions/fst_fe_rev.RDS")) %>% 
  format_fst() %>% 
  select_mae()
all_rev_ce <- select_series(readRDS("results_simul/compile_revisions/fst_ce_rev.RDS")) %>% 
  format_fst() %>% 
  select_mae() 

all_pond <- do.call(rbind,
                    lapply(0:3, function(degree){
                      data = readRDS(sprintf("R_filters/fst_pdegree%i.RDS", degree))$weights
                      data$degree = degree
                      data$weight = sprintf("weight%i", seq_len(nrow(data)))
                      data
                    }))
min_tp_moy %>% left_join(all_pond)
min_tp_moy %>% left_join(all_rev_fe %>% select(c(variability,degree,weight, rev.q0, series)),
                        by = c("weight", "degree", "variability"))
