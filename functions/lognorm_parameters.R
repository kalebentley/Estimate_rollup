# log-normal parameter moment matching (convert mean/sd to alpha and beta)
lognorm_parameters <- function(mean,sd) {
  beta<-(log((sd^2 + mean^2)/mean^2))^0.5
  alpha<-log(mean) - 1/2 * log((sd^2 + mean^2)/mean^2)
  data.frame(alpha = alpha, beta=beta)
}