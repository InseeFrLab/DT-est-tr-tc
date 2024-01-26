# Pour que ce programme puisse tourner, il faut également avoir lancé ceux sous R_local_ic
source("R_fredm/4_utils.R",encoding = "UTF-8")
library(patchwork)

outDec <- ","
fst_weights = "weight235"
fst_degree = 2
all_tp <- get_all_tp(dossier = "results_fredm/compile_tp_norev/",
                     fst_weights = "weight235",
                     fst_degree = 2)
all_tp_rev <- get_all_tp(dossier = "results_fredm/compile_tp/",
                     fst_weights = "weight235",
                     fst_degree = 2)
detected_tp <- readRDS("results_fredm/compile_tp_norev/detected_tp_lp.RDS")

AQLThesis::fred_md_description %>% 
  dplyr::filter(fred %in% c(series = "CE16OV", "DPCERA3M086SBEA", 
                     "INDPRO", "PAYEMS", 
                     "RETAILx", "W875RX1")) %>% 
  arrange(fred)




data_y <- readRDS("data_fredm/byseries/CE16OV.RDS")[["2020"]]
data_p <- ts.union(data_y, henderson(data_y, length = 13, musgrave = FALSE))
data_p <- window(data_p, start = 2000, end = 2004)
p <- forecast::autoplot(data_p) + 
  geom_vline(xintercept = 2001+1/12, linetype = "dotted") +
  theme_bw() +
  theme(legend.background = element_rect(fill = alpha('gray99', 0.4),
                                         colour = "gray80", linetype = "solid"),
        legend.justification = c(0,1),
        legend.position = c(0,1),
        legend.key = element_blank(),
        legend.title = element_blank()) +
  labs(x = NULL, y = NULL)  +
  scale_color_discrete(labels =c("Série initiale","Série lissée")) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6), 
                     labels = zoo::as.yearmon) + 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 4), 
                     labels = function(x) format(x, decimal.mark = outDec))
p
ggMultisave("DT/img/nber/ce16ov", 
            plot = p,
            width = 8, height = 2.5)


all_plots <- get_all_plots(all_tp,
                           all_tp_rev,
                           series = "CE16OV", tp_keep ="2001.16666666667",
                           fst_degree = fst_degree,
                           nb_est = 8)
wrap_plots(all_plots[(1:8)],ncol = 4)
wrap_plots(all_plots[-(1:8)],ncol = 3)
all_p <- (
  wrap_plots(plots[(1:8)],ncol = 4)/
    wrap_plots(plots[-(1:8)],ncol = 3)
)&theme(plot.title = element_text(hjust = 0.5))
all_p

ggMultisave("DT/img/nber/ce16ov_fev2001", 
            plot =all_p,
            width = 8, height = 10)
# ggMultisave("DT/img/nber/ce16ov_fev2001_lp", 
#             plot = wrap_plots(all_plots[(1:8)], ncol = 4),
#             width = 8, height = 5)
# ggMultisave("DT/img/nber/ce16ov_fev2001_autres", 
#             plot = wrap_plots(all_plots[-(1:8)], ncol = 3),
#             width = 8, height = 5)

data <- readRDS("data_fredm/byseries/RETAILx.RDS")[["2020"]]
plot(data)
all_plots <- get_all_plots(all_tp,
                           series = "RETAILx", tp_keep ="2007.91666666667",
                           fst_degree = fst_degree,
                           nb_est = 8)
wrap_plots(all_plots[(1:8)], ncol = 4)
wrap_plots(all_plots[-(1:8)],ncol = 3)
ggMultisave("DT/img/nber/retailx_nov2007_lp", 
            plot = wrap_plots(all_plots[(1:8)], ncol = 4),
            width = 8, height = 5)
ggMultisave("DT/img/nber/retailx_nov2007_autres", 
            plot = wrap_plots(all_plots[-(1:8)], ncol = 3),
            width = 8, height = 5)

##########################################################################
all_plots <- get_all_plots(all_tp,
                           series = "CE16OV", tp_keep ="2007.91666666667",
                           fst_degree = fst_degree)
wrap_plots(all_plots[-6],ncol = 4)
all_plots <- get_all_plots(all_tp,
                           series = "CE16OV", tp_keep ="2001.83333333333",
                           fst_degree = fst_degree)
wrap_plots(all_plots[-6],ncol = 4)


all_plots <- get_all_plots(all_tp,
                           series = "RETAILx", tp_keep ="2007.91666666667",
                           fst_degree = fst_degree)
wrap_plots(all_plots[-6],ncol = 4)


all_plots <- get_all_plots(all_tp,
                           series = "INDPRO", tp_keep ="2001.83333333333",
                           fst_degree = fst_degree)
wrap_plots(all_plots[-6],ncol = 4)


all_plots <- get_all_plots(all_tp,
                           series = "PAYEMS", tp_keep ="2001.16666666667",
                           fst_degree = fst_degree)
wrap_plots(all_plots[-6],ncol = 4)
all_plots <- get_all_plots(all_tp,
                           series = "PAYEMS", tp_keep ="2007.91666666667",
                           fst_degree = fst_degree)
wrap_plots(all_plots[-6],ncol = 4)
all_plots <- get_all_plots(all_tp,
                           series = "PAYEMS", tp_keep ="2020.08333333333",
                           fst_degree = fst_degree)
wrap_plots(all_plots[-6],ncol = 4)
all_plots <- get_all_plots(all_tp,
                           series = "PAYEMS", tp_keep ="2020.25",
                           fst_degree = fst_degree)
wrap_plots(all_plots[-6],ncol = 4)






all_plots <- get_all_plots(all_tp,
                           series = "W875RX1", tp_keep ="2001.16666666667",
                           fst_degree = fst_degree)
wrap_plots(all_plots[-6],ncol = 4)
all_plots <- get_all_plots(all_tp,
                           series = "W875RX1", tp_keep ="2007.91666666667",
                           fst_degree = fst_degree)
wrap_plots(all_plots[-6],ncol = 4)
all_plots <- get_all_plots(all_tp,
                           series = "W875RX1", tp_keep ="2020.08333333333",
                           fst_degree = fst_degree)
wrap_plots(all_plots[-6],ncol = 4)
all_plots <- get_all_plots(all_tp,
                           series = "W875RX1", tp_keep ="2001.83333333333",
                           fst_degree = fst_degree)
wrap_plots(all_plots[-6],ncol = 4)
all_plots <- get_all_plots(all_tp,
                           series = "W875RX1", tp_keep ="2020.25",
                           fst_degree = fst_degree)
wrap_plots(all_plots[-6],ncol = 4)





##########################################################################
all_plots <- get_all_plots(all_tp,
                           series = "CE16OV", tp_keep ="2001.16666666667",
                           fst_degree = fst_degree)
wrap_plots(all_plots,ncol = 4)
all_plots <- get_all_plots(all_tp,
                           series = "CE16OV", tp_keep ="2007.91666666667",
                           fst_degree = fst_degree)
wrap_plots(all_plots,ncol = 4)
all_plots <- get_all_plots(all_tp,
                           series = "CE16OV", tp_keep ="2020.08333333333",
                           fst_degree = fst_degree)
wrap_plots(all_plots,ncol = 4)
all_plots <- get_all_plots(all_tp,
                           series = "CE16OV", tp_keep ="2001.83333333333",
                           fst_degree = fst_degree)
wrap_plots(all_plots,ncol = 4)
all_plots <- get_all_plots(all_tp,
                           series = "CE16OV", tp_keep ="2020.25",
                           fst_degree = fst_degree)
wrap_plots(all_plots,ncol = 4)


all_plots <- get_all_plots(all_tp,
                           series = "DPCERA3M086SBEA", tp_keep ="2007.91666666667",
                           fst_degree = fst_degree)
wrap_plots(all_plots,ncol = 4)
all_plots <- get_all_plots(all_tp,
                           series = "DPCERA3M086SBEA", tp_keep ="2020.08333333333",
                           fst_degree = fst_degree)
wrap_plots(all_plots,ncol = 4)
all_plots <- get_all_plots(all_tp,
                           series = "DPCERA3M086SBEA", tp_keep ="2009.41666666667",
                           fst_degree = fst_degree)
wrap_plots(all_plots,ncol = 4)
all_plots <- get_all_plots(all_tp,
                           series = "DPCERA3M086SBEA", tp_keep ="2020.25",
                           fst_degree = fst_degree)
wrap_plots(all_plots,ncol = 4)


all_plots <- get_all_plots(all_tp,
                           "INDPRO", tp_keep ="2007.91666666667",
                           fst_degree = fst_degree)
wrap_plots(all_plots,ncol = 4)
all_plots <- get_all_plots(all_tp,
                           "INDPRO", tp_keep ="2020.08333333333",
                           fst_degree = fst_degree)
wrap_plots(all_plots,ncol = 4)
all_plots <- get_all_plots(all_tp,
                           "INDPRO", tp_keep ="2001.83333333333",
                           fst_degree = fst_degree)
wrap_plots(all_plots,ncol = 4)
all_plots <- get_all_plots(all_tp,
                           "INDPRO", tp_keep ="2009.41666666667",
                           fst_degree = fst_degree)
wrap_plots(all_plots,ncol = 4)
all_plots <- get_all_plots(all_tp,
                           "INDPRO", tp_keep ="2020.25",
                           fst_degree = fst_degree)
wrap_plots(all_plots,ncol = 4)



all_plots <- get_all_plots(all_tp,
                           series = "PAYEMS", tp_keep ="2001.16666666667",
                           fst_degree = fst_degree)
wrap_plots(all_plots,ncol = 4)
all_plots <- get_all_plots(all_tp,
                           series = "PAYEMS", tp_keep ="2007.91666666667",
                           fst_degree = fst_degree)
wrap_plots(all_plots,ncol = 4)
all_plots <- get_all_plots(all_tp,
                           series = "PAYEMS", tp_keep ="2020.08333333333",
                           fst_degree = fst_degree)
wrap_plots(all_plots,ncol = 4)
all_plots <- get_all_plots(all_tp,
                           series = "PAYEMS", tp_keep ="2020.25",
                           fst_degree = fst_degree)
wrap_plots(all_plots,ncol = 4)



all_plots <- get_all_plots(all_tp,
                           series = "RETAILx", tp_keep ="2007.91666666667",
                           fst_degree = fst_degree)
wrap_plots(all_plots,ncol = 4)
all_plots <- get_all_plots(all_tp,
                           series = "RETAILx", tp_keep ="2020.08333333333",
                           fst_degree = fst_degree)
wrap_plots(all_plots,ncol = 4)
all_plots <- get_all_plots(all_tp,
                           series = "RETAILx", tp_keep ="2001.83333333333",
                           fst_degree = fst_degree)
wrap_plots(all_plots,ncol = 4)
all_plots <- get_all_plots(all_tp,
                           series = "RETAILx", tp_keep ="2020.25",
                           fst_degree = fst_degree)
wrap_plots(all_plots,ncol = 4)



all_plots <- get_all_plots(all_tp,
                           series = "W875RX1", tp_keep ="2001.16666666667",
                           fst_degree = fst_degree)
wrap_plots(all_plots,ncol = 4)
all_plots <- get_all_plots(all_tp,
                           series = "W875RX1", tp_keep ="2007.91666666667",
                           fst_degree = fst_degree)
wrap_plots(all_plots,ncol = 4)
all_plots <- get_all_plots(all_tp,
                           series = "W875RX1", tp_keep ="2020.08333333333",
                           fst_degree = fst_degree)
wrap_plots(all_plots,ncol = 4)
all_plots <- get_all_plots(all_tp,
                           series = "W875RX1", tp_keep ="2001.83333333333",
                           fst_degree = fst_degree)
wrap_plots(all_plots,ncol = 4)
all_plots <- get_all_plots(all_tp,
                           series = "W875RX1", tp_keep ="2020.25",
                           fst_degree = fst_degree)
wrap_plots(all_plots,ncol = 4)
