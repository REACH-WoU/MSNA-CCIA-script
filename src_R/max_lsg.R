make_lsg <- function(
    .dataset,
    crit_to_4plus = NULL,
    crit_to_4 = NULL,
    crit_to_3 = NULL,
    crit_to_2 = NULL,
    non_crit = NULL) {
  
  #if there is at least a 4+ score indicator
  if(!is.null(crit_to_4plus)) {
    indicator_to_select <- c(crit_to_4plus, crit_to_4, crit_to_3,crit_to_2, non_crit)
    
    if(sum(.dataset[indicator_to_select] > 5, na.rm = T) > 0) {
      stop("Cannot have values equals above 5")
    }
    
    make_single_lsg(.dataset,
             crit_to_4plus = crit_to_4plus,
             crit_to_4 = crit_to_4, 
             crit_to_3 = crit_to_3, 
             crit_to_2 = crit_to_2,
             non_crit = non_crit,
             indicator_to_select = indicator_to_select)
    
    
  } else if (!is.null(crit_to_4)){ #if there is at least 4 score indicator
    indicator_to_select <- c(crit_to_4, crit_to_3,crit_to_2, non_crit)
    
    if(sum(.dataset[indicator_to_select] > 4, na.rm = T) > 0) {
      stop("You don't have 4+ indicators, values cannot be above 4")
    }
    
    make_single_lsg(.dataset,
                    crit_to_4plus = NULL,
                    crit_to_4 = crit_to_4, 
                    crit_to_3 = crit_to_3, 
                    crit_to_2 = crit_to_2,  
                    non_crit = non_crit,
                    indicator_to_select = indicator_to_select)
  } else if (!is.null(crit_to_3)){ #if there is at least 4 score indicator
    indicator_to_select <- c(crit_to_3,crit_to_2, non_crit)
    
    if(sum(.dataset[indicator_to_select] > 3, na.rm = T) > 0) {
      stop("You don't have 3 indicators, values cannot be above 3")
    }
    
    make_single_lsg(.dataset,
                    crit_to_4plus = NULL,
                    crit_to_4 = crit_to_4, 
                    crit_to_3 = crit_to_3, 
                    crit_to_2 = crit_to_2,  
                    non_crit = non_crit,
                    indicator_to_select = indicator_to_select)
  } else {
    indicator_to_select <- c(crit_to_2,  non_crit)
    
    if(sum(.dataset[indicator_to_select] > 2, na.rm = T) > 0) {
      print("You don't have indicators scoring 2, values cannot be above 2")
    }
    
    make_single_lsg(.dataset,
                    crit_to_4plus = NULL,
                    crit_to_4 = NULL, 
                    crit_to_3 = crit_to_3, 
                    crit_to_2 = crit_to_2,
                    non_crit = non_crit,
                    indicator_to_select = indicator_to_select)

  }
}


make_single_lsg <- function(.dataset,
                     crit_to_4plus,
                     crit_to_4, 
                     crit_to_3, 
                     crit_to_2,
                     non_crit,
                     indicator_to_select) {
  ## Calcular el maximo preliminar eliminando los NA
  .dataset[["max_preliminar"]] <- .dataset[, indicator_to_select] |>
    apply(1, FUN = function(xx) {
      if(all(is.na(xx[indicator_to_select]))) {
        NA_real_
        ###### Desde aqui revisar
      }else { #else return the max.
        xx |> max(na.rm = T)
      }
    })
  !is.null(crit_to_4plus)
  if (!is.null(crit_to_4plus)) {
    .dataset[["max_crit_to_4plus"]] <- max.value(.dataset, crit_to_4plus)
    .dataset <- .dataset %>% mutate(max_crit_to_4plus = case_when(is.na(max_crit_to_4plus) ~ 5,
                                                                TRUE ~ max_crit_to_4plus))
  }else {
    .dataset[["max_crit_to_4plus"]] <- NA_real_
  }
  if (!is.null(crit_to_4)) {
    .dataset[["max_crit_to_4"]] <- max.value(.dataset, crit_to_4)
    .dataset <- .dataset %>% mutate(max_crit_to_4 = case_when(is.na(max_crit_to_4) ~ 4,
                                                            TRUE ~ max_crit_to_4))
  }else {
    .dataset[["max_crit_to_4"]] <- NA_real_
  }
  if (!is.null(crit_to_3)) {
    .dataset[["max_crit_to_3"]] <- max.value(.dataset, crit_to_3)
    .dataset <- .dataset %>% mutate(max_crit_to_3 = case_when(is.na(max_crit_to_3) ~ 3,
                                                            TRUE ~ max_crit_to_3))
  }else {
    .dataset[["max_crit_to_3"]] <- NA_real_
  }
  if (!is.null(crit_to_2)) {
    .dataset[["max_crit_to_2"]] <- max.value(.dataset, crit_to_2)
    .dataset <- .dataset %>% mutate(max_crit_to_2 = case_when(is.na(max_crit_to_2) ~ 2,
                                                            TRUE ~ max_crit_to_2))
  }else {
    .dataset[["max_crit_to_2"]] <- NA_real_
  }
  if (!is.null(non_crit)) {
    .dataset[["max_non_crit"]] <- max.value(.dataset, non_crit)
    .dataset <- .dataset %>% mutate(max_non_crit = case_when(is.na(max_non_crit) ~ 3,
                                                           TRUE ~ max_non_crit))
  }else {
    .dataset[["max_non_crit"]] <- NA_real_
  }
  
  
  .dataset <- .dataset %>% rowwise() %>% 
    mutate(max_sinNA = pmax(max_crit_to_4plus, max_crit_to_4, max_crit_to_3,max_crit_to_2, max_non_crit, na.rm = T))
  
  .dataset <- .dataset %>% mutate(max_final = case_when(max_preliminar < max_sinNA ~ NA_real_,
                                                      TRUE ~ max_preliminar))
  
  #prueba <- .dataset %>% select(lsg_wash_crit2, lsg_wash_crit1, lsg_wash_crit3, nc_wash_total, max_preliminar, max_sinNA, max_final, uuid)
  
  return(.dataset$max_final)
}




## Calcular los maximos 
max.value <- function(.dataset, columnas) {
  
  # Obtener el nombre del data frame
  name.df <- as.character(substitute(.dataset))
  
  # pegar el signo de dolar a las columnas
  evaluar <- paste(name.df, columnas, sep = "$")
  # agregar separador de ,
  evaluar <- paste(evaluar, collapse = ", " )
  # agregar la funcion de base::pmax
  evaluar <- paste("base::pmax(", evaluar, ", na.rm = F)");evaluar
  
  .dataset[["maximo"]] <- eval(parse(text=evaluar))
  
  return(.dataset[["maximo"]])
}
