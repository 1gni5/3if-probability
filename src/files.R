# --- Question 6 ---
FileMM1 <- function(lambda, mu, D)
{
  incomingClients <- c()
  outgoingClients <- c()
  
  processedClients <- 0
  
  # Calcul l'intervalle entre l'arrivées des 2 derniers clients
  clientInterval <- rexp(1, lambda)
  
  # Si la file d'attente est vide
  incomingClients <- append(incomingClients, clientInterval)
  processedClients <- 1
  
  # Calcul l'intervalle de traitement du serveur
  serverInterval <- rexp(1, mu)
  
  # Set current client outgoing date
  outgoingClients <- append(
    outgoingClients, 
    incomingClients[processedClients] + serverInterval
  )
  
  # Réduit l'analyse à la période D
  while (
    min(incomingClients[processedClients], 
        outgoingClients[processedClients]) < D
  ){
    
    # Calcul l'intervale d'entré
    clientInterval <- rexp(1, lambda)
    
    # Ajoute la date d'entrée
    incomingClients <- append (
      incomingClients, 
      incomingClients[processedClients] + clientInterval
    )
    
    processedClients <- processedClients + 1
    
    serverInterval <- rexp(1, mu)
    
    # Ajoute la date de sortie
    outgoingClients <- append(
      outgoingClients, 
      max ( # Gère le cas d'un serveur occupé ou libre
        incomingClients[processedClients], # Serveur libre
        outgoingClients[processedClients - 1] # Serveur occupé
      ) + serverInterval
    )
  }
  
  # Retire les éléments en dehors de la période d'observation
  incomingClients <- incomingClients[incomingClients < D]
  outgoingClients <- outgoingClients[outgoingClients < D]
  
  return(list(
    departs = outgoingClients, 
    arrivees = incomingClients)
  )
}

# --- Question 7 ---
systemEvolution <- function(incoming, outgoing)
{
  # Nombre de clients dans le système à l'instant t
  numberOfClient <- 0
  
  # Curseurs arrivées et départs
  cursorIn <- 1
  cursorOut <- 1
  
  state <- list(
    time <- c(),
    capacity <- c()
  )
  
  # Tant que les 2 pointeurs sont dans les limites des vecteurs
  while ( 
    cursorIn < length(incoming)
    && cursorOut < length(outgoing)
  ){
    # Récupère la date la plus proche
    date <- min(results$arrivees[cursorIn], results$departs[cursorOut])
    
    # Date d'arrivé
    if (date == results$arrivees[cursorIn])
    {
      numberOfClient <- numberOfClient + 1
      cursorIn <- cursorIn + 1
    }else{
      numberOfClient <- numberOfClient - 1
      cursorOut <- cursorOut + 1
    }

    # Met à jour l'historique de l'état du système
    state$time <- append(state$time, date)
    state$capacity <- append(state$capacity, numberOfClient)
  }

  # Ajoute les valeurs d'arrivées manquantes 
  for (cursor in cursorIn:length(results$arrivees))
  {
    date <- results$arrivees[cursor]
    numberOfClient <- numberOfClient + 1
    
    # Met à jour l'historique de l'état du système
    state$time <- append(state$time, date)
    state$capacity <- append(state$capacity, numberOfClient)
  }

  # Ajoute les valeurs de sorties manquantes
  for (cursor in cursorOut:length(results$departs))
  {
    date <- results$departs[cursor]
    numberOfClient <- numberOfClient - 1
    
    # Met à jour l'historique de l'état du système
    state$time <- append(state$time, date)
    state$capacity <- append(state$capacity, numberOfClient)
  }
  
  return(state)
}

turnAroundTime <- function(inDate, outDate)
{
  timePassed <- c()
  for (i in 1:min(length(inDate), length(outDate)))
  {
    timePassed <- append(timePassed, outDate[i] - inDate[i])
  }
  return(timePassed)
}

