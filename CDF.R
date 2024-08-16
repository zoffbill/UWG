#' Fonction de répartition pour la distribution UWG
#'
#' @param x Valeur pour laquelle calculer la fonction de répartition.
#' @param alpha Paramètre alpha de la distribution UWG.
#' @param beta Paramètre beta de la distribution UWG.
#' @param lambda Paramètre lambda de la distribution UWG.
#' @param k Paramètre k de la distribution UWG.
#' @return La fonction de répartition pour la valeur donnée.
#' @export
CDF <- function(x, alpha, beta, lambda, k) {
  return(1 - exp(-(( (beta * x / (1 - x)) ^ alpha) / lambda) ^ k))
}
