#' Fonction de densité pour la distribution UWG
#'
#' @param x Valeur pour laquelle calculer la densité.
#' @param params Paramètres de la distribution UWG : alpha, beta, lambda, k.
#' @return La densité pour la valeur donnée.
#' @export
PDF <- function(x, params) {
  alpha <- params[1]
  beta <- params[2]
  lambda <- params[3]
  k <- params[4]

  numerator <- alpha * k / (lambda)^k
  denominator1 <- beta / (1 - x)^2
  exponent <- alpha * k - 1
  term1 <- (beta * x / (1 - x))^exponent
  exponent2 <- -((beta * x / (1 - x))^alpha / lambda)^k
  term2 <- exp(exponent2)

  density <- numerator * denominator1 * term1 * term2
  density
}
