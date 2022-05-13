FileMM1 <- function(lambda, mu, D)
{
  incomingClients <- c()
  outgoingClients <- c()
  
  processedClients <- 0
  clients <- c()
  
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
    
  while (min(incomingClients[processedClients], outgoingClients[processedClients]) < D)
  {
    clientInterval <- rexp(1, lambda)
    
    incomingClients <- append (
      incomingClients, 
      incomingClients[processedClients] + clientInterval
    )
    
    processedClients <- processedClients + 1
    
    serverInterval <- rexp(1, mu)
    
    outgoingClients <- append(
      outgoingClients, 
      max(
        incomingClients[processedClients], # Serveur libre
        outgoingClients[processedClients - 1] # Serveur occupé
      ) + serverInterval
    )
    
  }
  
  # Retire les derniers éléments (en dehors de l'intervale d'observation)
  incomingClients <- incomingClients[incomingClients < D]
  outgoingClients <- outgoingClients[outgoingClients < D]
  
  return(list(departs=outgoingClients, arrivees=incomingClients))
}

systemEvolution <- function(results)
{
  # Date de chaque événement (entrée ou sortie)
  timeTable <- sort(c(results$arrivees, results$departs))
  
  # Nombre de clients dans le système à l'instant t
  noClients <- 0
  
  # Curseurs arrivées et départs
  cursorIn <- 1
  cursorOut <- 1
  
  state <- list(
    t <- c(),
    nb <- c()
  )
  
  while (
    cursorIn < length(results$arrivees)
    && cursorOut < length(results$departs)
  )
  {
    if (results$arrivees[cursorIn] <= results$departs[cursorOut])
    {
      t <- results$arrivees[cursorIn]
      noClients <- noClients + 1
      cursorIn <- cursorIn + 1
    }
    else
    {
      t <- results$departs[cursorOut]
      noClients <- noClients - 1
      cursorOut <- cursorOut + 1
    }

    state$t <- append(state$t, t)
    state$nb <- append(state$nb, noClients)
  }

  for (cursor in cursorIn:length(results$arrivees))
  {
    t <- results$arrivees[cursor]
    noClients <- noClients + 1
    state$t <- append(state$t, t)
    state$nb <- append(state$nb, noClients)
  }

  for (cursor in cursorOut:length(results$departs))
  {
    t <- results$departs[cursor]
    noClients <- noClients - 1
    state$t <- append(state$t, t)
    state$nb <- append(state$nb, noClients)
  }
  
  
  return(state)
}

erlang <- function(lambda, mu)
{
  return(lambda / mu)
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
