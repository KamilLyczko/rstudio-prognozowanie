get_nnar_model_type <- function(fit) {
  type <- fit$method
  if(!is.null(fit$xreg))
    type <- paste0(type, 
                   " z regresorem zewnętrznym")
  return(type)
}