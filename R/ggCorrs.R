#' @title ACF, PACF and CCF
#'
#' @description It uses the \pkg{ggplot2} package to create three usefull functions in time series analysis: ACF, PACF and CCF.
#'
#' @param x,y Variables of interest.
#' @param min_lag,max_lag They correspond to the minimum lag and the maximum lag. Default values are \code{min_lag=-20} and \code{max_lag=20}.
#' @param plot_type It specifies the type of the plot. You can choose \code{"ACF"} (default) for the autocorrelation function, \code{"PACF"} for the partial autocorrelation function and \code{"CCF"} for the cross-correlation function.
#' @param alpha The type-I error to build confidence intervals. The default value is \code{alpha=.05}.
#'
#' @return Return the desired plot. It is possible to extract the lags and the corresponding values.
#'
#' The confidence intervals are calculated using the asymptotic distribution of the correlation estimator \eqn{\rho}~N(0,T^\eqn{{-1}}). Therefore IC_alpha(\eqn{\rho})=(-qnorm(1-alpha/2)sqrt(T^\eqn{{-1}}), +qnorm(1-alpha/2)sqrt(T^\eqn{{-1}})).
#'
#' @examples
#' # ::::::::::: #
#' # TOY EXAMPLE #
#' # ::::::::::: #
#'
#' library("ggplot2")
#' set.seed(111)
#' x <- rnorm(50)
#' y <- rchisq(50, 4)
#' g <- ggCorrs(x)
#' # g <- ggCorrs(x, print_vals = T)
#' g$data$lag # printing lags
#' g$data$acf.val # printing values of ACF
#' ggCorrs(x, y, plot_type = "CCF")
#'
#'
#' @export ggCorrs

ggCorrs <- function(x, y = NULL, min_lag = -20, max_lag = 20, plot_type = "ACF", print_vals = F, alpha = .05){

  # If ACF
  if(plot_type == "ACF"){
    if(NROW(y) != 0) print("y not required")
    N <- NROW(x)
    acf.data = acf(x, plot = F)
    index = which(acf.data$lag[, 1, 1] %in% 0:max_lag)
    acf = data.frame(lag = acf.data$lag[index, 1, 1],
                     acf.val = acf.data$acf[index, 1, 1])
    return(ggplot(acf, aes(x = lag, y = acf.val)) +
             geom_bar(stat = 'identity') +
             labs(x = "Lag", y = "ACF")+
             geom_hline(yintercept = qnorm(1-alpha/2)/sqrt(N), color = 'blue', linetype = 'dashed') +
             geom_hline(yintercept = -qnorm(1-alpha/2)/(sqrt(N)), color = 'blue', linetype = 'dashed'))

  }

  # If PACF
  if(plot_type == "PACF"){
    if(NROW(y) != 0) print("y not required")
    N <- NROW(x)
    pacf.data = pacf(x, plot = F)
    index = which(pacf.data$lag[, 1, 1] %in% 0:max_lag)
    pacf = data.frame(lag = pacf.data$lag[index, 1, 1],
                      pacf.val = pacf.data$acf[index, 1, 1])
    return(ggplot(pacf,
                  aes(x = lag, y = pacf.val)) +
             geom_bar(stat = 'identity') +
             labs(x = "Lag", y = "PACF")+
             geom_hline(yintercept = qnorm(1-alpha/2)/sqrt(N), color = 'blue', linetype = 'dashed') +
             geom_hline(yintercept = -qnorm(1-alpha/2)/(sqrt(N)), color = 'blue', linetype = 'dashed'))
  }

  # If CCF
  if(plot_type == "CCF"){
    if(NROW(y) == 0) print("y required")
    N <- NROW(x)
    ccf.data = ccf(x, y, plot = F)
    index = which(ccf.data$lag[,1,1] %in% min_lag:max_lag)
    ccf = data.frame(lag = ccf.data$lag[index, 1, 1],
                     ccf.val = ccf.data$acf[index, 1, 1])
    return(ggplot(ccf,
                  aes(x = lag, y = ccf.val)) +
             geom_bar(stat = 'identity') +
             labs(x = "Lag", y = "CCF")+
             geom_hline(yintercept = qnorm(1-alpha/2)/sqrt(N), color = 'blue', linetype = 'dashed') +
             geom_hline(yintercept = -qnorm(1-alpha/2)/(sqrt(N)), color = 'blue', linetype = 'dashed'))
  }
}
