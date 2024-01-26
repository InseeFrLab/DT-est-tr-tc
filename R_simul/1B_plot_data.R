library(AQLThesis)
load_simul <- function(x){
  data <- readRDS(sprintf("data_simul/byseries/%s.RDS", x))
  data <- data[[length(data)]]
  data
}
data <- do.call(cbind, lapply(sprintf("%svariability%i", rep(c("low", "medium", "high"), each = 3),
                                      rep(1:3, 3)),load_simul))
data <- data[,seq(from = 2, by = 3, length.out = 3)] 
colnames(data) <- paste("Variabilité", c("faible", "moyenne", "forte"))
dataGraph <- data.frame(cbind(time(data), data))
colnames(dataGraph) <- c("date", colnames(data))
dataGraph <- reshape2::melt(dataGraph, id="date")
p <- ggplot(data = dataGraph, 
            aes(x = date, y = value, group = variable, colour = variable)) +
  geom_line(linewidth=0.7, data = dataGraph) +
  labs(title = NULL,
       x = NULL, y = NULL) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10),
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 12),
                     labels = function(x) format(x, decimal.mark = ",")) + 
  theme_bw() +theme(legend.background = element_rect(fill = alpha('gray99', 0.4),
                                                     colour = "gray80", linetype = "solid"),
                    legend.justification = c(0,0),
                    legend.position = c(0,0),
                    legend.key = element_blank(),
                    legend.title = element_blank())

ggMultisave("DT/img/simulations/simul_data", 
            plot = p,
            width = 8, height = 4)


