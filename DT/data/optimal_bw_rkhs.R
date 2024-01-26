library(rjd3filters)
rkhs_filter(asymmetricCriterion  = "Timeliness",
            passband = 2*pi/36)
rkhs_optimal_bw(asymmetricCriterion  = "Timeliness",
                passband = 2*pi/36,optimal.minBandwidth = 7)
rkhs_optimal_bw(asymmetricCriterion  = "Timeliness",
                passband = 2*pi/36,optimal.minBandwidth = 7)
round(rkhs_optimal_bw(asymmetricCriterion  = "FrequencyResponse",
                      passband = pi,optimal.minBandwidth = 6.01),2)
round(rkhs_optimal_bw(asymmetricCriterion  = "Accuracy",
                      passband =pi,optimal.minBandwidth = 6.1),2)
data_bw <- rbind(rkhs_optimal_bw(asymmetricCriterion  = "FrequencyResponse",
                                 passband = pi,optimal.minBandwidth = 6.01),
                 rkhs_optimal_bw(asymmetricCriterion  = "FrequencyResponse",
                                 passband = 2*pi/12,optimal.minBandwidth = 6.01),
                 rkhs_optimal_bw(asymmetricCriterion  = "Accuracy",
                                 passband =pi,optimal.minBandwidth = 6.01),
                 rkhs_optimal_bw(asymmetricCriterion  = "Accuracy",
                                 passband =2*pi/12,optimal.minBandwidth = 6.01),
                 c(6.01, 6.01, 7.12, 8.44, 9.46, 10.39),
                 rkhs_optimal_bw(asymmetricCriterion  = "Timeliness",
                                 passband = 2*pi/36,optimal.minBandwidth = 6.01),
                 rkhs_optimal_bw(asymmetricCriterion  = "Timeliness",
                                 passband = 2*pi/12,optimal.minBandwidth = 6.01)
)
data_bw <- round(data_bw,2)
rownames(data_bw) <- c("$\\omega_1 = \\pi$ (valeurs utilisées)",
                       "$\\omega_1 = 2\\pi/12$",
                       "$\\omega_1 = \\pi$ (valeurs utilisées)",
                       "$\\omega_1 = 2\\pi/12$",
                       "Valeurs de l'article (avec $\\omega_1 = 2\\pi/36$)",
                       "$\\omega_1 = 2\\pi/36$",
                       "$\\omega_1 = 2\\pi/12$")
colnames(data_bw) <- sprintf("$%s$", colnames(data_bw))
saveRDS(data_bw, "data_fredm/rkhs_bw_optimal.RDS")
saveRDS(data_bw, "DT/data/rkhs_bw_optimal.RDS")
