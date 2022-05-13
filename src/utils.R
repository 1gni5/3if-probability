belowThreshold <- function(pValues, threshold = 0.01) {
  
  results <- list (
    RANDU = c(),
    StandardMinimal = c(),
    VonNeumann = c(),
    MersenneTwister = c()
  )
  
  # Analyse des valeurs générées par RANDU
  for (x in pValues$RANDU) {
    
    # Vérifie si la valeur est en dessous du seuil
    if (x <= threshold) {
      results$RANDU <- append(results$RANDU, x)
    }
  }
  
  # Analyse des valeurs générées par Standard Minimal
  for (x in pValues$StandardMinimal) {
    
    # Vérifie si la valeur est en dessous du seuil
    if (x <= threshold) {
      results$StandardMinimal <- append(results$StandardMinimal, x)
    }
  }
  
  # Analyse des valeurs générées par Von Neumann
  for (x in pValues$VonNeumann) {
    
    # Vérifie si la valeur est en dessous du seuil
    if (x <= threshold) {
      results$VonNeumann <- append(results$VonNeumann, x)
    }
  }
  
  # Analyse des valeurs générées par Mersenne Twister
  for (x in pValues$MersenneTwister) {
    
    # Vérifie si la valeur est en dessous du seuil
    if (x <= threshold) {
      results$MersenneTwister <- append(results$MersenneTwister, x)
    }
  }
  
  return(results)
}
