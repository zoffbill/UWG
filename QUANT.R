#' Fonction Quantile pour la distribution UWG
#'
#' @param y Probabilité pour laquelle calculer le quantile.
#' @param lambda Paramètre lambda de la distribution UWG.
#' @param beta Paramètre beta de la distribution UWG.
#' @param alpha Paramètre alpha de la distribution UWG.
#' @param k Paramètre k de la distribution UWG.
#' @return Le quantile correspondant à la probabilité y.
#' @export
QUANT <- function(y, lambda, beta, alpha, k) {
  return(((lambda^(1/alpha)) * (-log(1 - y))^(1/(alpha * k))) / (beta + (lambda^(1/alpha)) * (-log(1 - y))^(1/(alpha * k))))
}
