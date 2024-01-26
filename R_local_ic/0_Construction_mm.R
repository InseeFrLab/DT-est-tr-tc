library(rjd3filters)
library(ggplot2)
library(patchwork)
library(AQLThesis)
h <- 6 
q <- 6
X_gen <- function(d = 1, p = 6, q = p){
  sapply(0:d, function(exp) seq(-p, q)^exp)
}
gen_MM <- function(p=6, q=p, d=2){
  k = rjd3filters::get_kernel("Henderson", h = h)
  k = c(rev(k$coef[-1]), k$coef[seq(0,q)+1])
  K = diag(k)
  X = X_gen(d=d, p = h, q = q)
  e1 = e2 = e3 = matrix(0, ncol = 1, nrow = d+1)
  e1[1] = 1
  e2[2] = 1
  e3[3] = 1
  M1 = K %*% X %*% solve(t(X) %*% K %*% X, e1)
  M2 = K %*% X %*% solve(t(X) %*% K %*% X, e2)
  M3 = K %*% X %*% solve(t(X) %*% K %*% X, e3)
  cbind(M1, M2, M3)
}
d2 = gen_MM(d=2)
d3 = gen_MM(d=3)
ylim_m2 = range(c(d2[,2], d2[,3]))
plot_coef_ggplot <- function(data){
  data$date <- factor(rownames(data), levels = rownames(data),ordered = TRUE)
  dataGraph <- reshape2::melt(data)
  
  ggplot(data = dataGraph, aes(x = date, y = value, group = variable,
                               colour = variable)) +
    geom_line(linewidth = 0.7) +
    geom_point(size = 1) +
    theme(panel.background = element_rect(fill = "white", colour = NA),
          panel.border = element_rect(fill = NA, colour = "grey20"),
          panel.grid.major = element_line(colour = "grey92"),
          panel.grid.minor = element_line(colour = "grey92",
                                          linewidth = 0.25),
          strip.background = element_rect(fill = "grey85", colour = "grey20"),
          complete = TRUE, plot.title = element_text(hjust = 0.5),
          legend.title=element_blank()) +
    labs(x = NULL, y = "Coefficients")
}

MM_D3 = data.frame(gen_MM(d=3,q=6)[,2], gen_MM(d=3,q=6)[,3])
rownames(MM_D3) <- rjd3filters:::coefficients_names(-6,6)
colnames(MM_D3) <- c("Est. pente", "Est. concavité")
plot_coef_ggplot(MM_D3)

MM_d3_pente = data.frame(sapply(0:6, function(x) c(gen_MM(d=3,q=x)[,2], rep(NA, 6-x))))
rownames(MM_d3_pente) <-  rjd3filters:::coefficients_names(-6,6)
colnames(MM_d3_pente) <- sprintf("q=%i",0:6)

MM_d3_deriv2 = data.frame(sapply(0:6, function(x) c(gen_MM(d=3,q=x)[,3], rep(NA, 6-x))))
rownames(MM_d3_deriv2) <-  rjd3filters:::coefficients_names(-6,6)
colnames(MM_d3_deriv2) <- sprintf("q=%i",0:6)

MM_d2_pente = data.frame(sapply(0:6, function(x) c(gen_MM(d=2,q=x)[,2], rep(NA, 6-x))))
rownames(MM_d2_pente) <-  rjd3filters:::coefficients_names(-6,6)
colnames(MM_d2_pente) <- sprintf("q=%i",0:6)

MM_d2_deriv2 = data.frame(sapply(0:6, function(x) c(gen_MM(d=2,q=x)[,3], rep(NA, 6-x))))
rownames(MM_d2_deriv2) <-  rjd3filters:::coefficients_names(-6,6)
colnames(MM_d2_deriv2) <- sprintf("q=%i",0:6)
p = (plot_coef_ggplot(MM_d3_pente) + ggtitle("MM utilisées pour estimer la pente") +
       theme(legend.position="none")
) + (plot_coef_ggplot(MM_d3_deriv2) + ggtitle("MM utilisées pour estimer la concavité"))

p21 = plot_coef_ggplot(MM_d2_pente) +
  theme(legend.position="none")
p22 = plot_coef_ggplot(MM_d2_deriv2)
p = (p21 + ggtitle("Estimateurs de la pente")) +
  (p22 + ggtitle("Estimateurs de la concavité"))
p
ggMultisave("DT/img/filters_used/mm_penteconcavite", 
            plot = p,
            width = 7,height = 3)
