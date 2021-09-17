library(dplyr)
library(magrittr)
library(ggplot2)

set.seed(123)

simexercise <- function(n) {
  sim_datasets <- data.frame()
  for (i in 1:n) {
    new_sample <- rnorm(1000,0,1)
    M <- mean(new_sample)
    SE <- 1/sqrt(length(new_sample))
    lower_int <- M - 1.96*SE
    upper_int <- M + 1.96*SE
    sim_datasets <- rbind(sim_datasets, 
                          data.frame("Mean" = M, "Bias" = M - 0, "Std.error" = SE, "Lower" = lower_int, "Upper" = upper_int))
  }
  return(sim_datasets)
}

sim_data <- simexercise(100)
sim_data$Covered <- ifelse(sim_data$Lower > 0 | sim_data$Upper < 0, "no", "yes")
ggplot(sim_data, aes(y = Mean, x = 1:100, colour = Covered)) +
  geom_hline(yintercept = 0, lwd = 2, colour = "grey") +
  geom_pointrange(aes(ymax = Upper, ymin = Lower)) +
  xlab("Simulation") +
  ylab("Means and 95% confidence intervals")

sim_data %>% 
  filter(Covered == "no")