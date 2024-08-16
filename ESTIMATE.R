#' Estimation des paramètres de la distribution UWG par simulation de Monte Carlo
#'
#' @param data Données à utiliser pour l'estimation.
#' @param density_function Fonction de densité de la distribution UWG.
#' @param start_params Paramètres initiaux pour l'estimation.
#' @param num_iterations Nombre d'itérations pour la simulation de Monte Carlo.
#' @return Liste contenant les paramètres estimés et la log-vraisemblance correspondante.
#' @export
ESTIMATE <- function(data, density_function, start_params, num_iterations) {
  log_likelihood <- function(params, data, density_function) {
    density_values <- density_function(data, params)
    log_likelihood_value <- sum(log(density_values))
    return(log_likelihood_value)
  }

  best_params <- start_params
  best_log_like <- log_likelihood(start_params, data, density_function)

  for (i in 1:num_iterations) {
    new_params <- start_params + rnorm(length(start_params), 0, 0.1) # Perturbation aléatoire des paramètres
    new_log_like <- log_likelihood(new_params, data, density_function)

    if (new_log_like > best_log_like) {
      best_params <- new_params
      best_log_like <- new_log_like
    }
  }

  return(list(params = best_params, log_like = best_log_like))
}
