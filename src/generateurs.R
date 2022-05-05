VonNeumann <- function(n, p=1, graine)
{
  x <-  rep(graine,n*p+1)
  for(i in 2:(n*p+1))
  {
    numbers <- strsplit(format(x[i-1]^2,scientific=FALSE),'')[[1]]
    while(length(numbers)>4){ 
        numbers <- numbers[2:(length(numbers)-1)] 
    }
    x[i] <- as.numeric(numbers)%*%(10^seq(length(numbers)-1,0,-1))
  }
  x <- matrix(x[2:(n*p+1)],nrow=n,ncol=p)
  return(x)
}


MersenneTwister <- function(n, p=1, graine)
{
  set.seed(graine,kind='Mersenne-Twister')
  x <- sample.int(2^32-1,n*p)
  x <- matrix(x,nrow=n,ncol=p)
  return(x)
}


binary <- function(x)
{
  if((x<2^31)&(x>=0))
    return( as.integer(intToBits(as.integer(x))) )
  else{
    if((x<2^32)&(x>0))
      return( c(binary(x-2^31)[1:31], 1) )
    else{
      cat('Erreur dans binary : le nombre etudie n est pas un entier positif en 32 bits.\n')
      return(c())
    }
  }
}

# --- Question 1 ---
RANDU <- function(k, seed)
{
  a <- 65539
  b <- 0
  m <- 2^31
  
  # Create first element based on seed.
  x <- c((a * seed + b) %% m)
  
  # Iterate over every state
  for (i in 2:k)
  {
    x[i] <- (a * x[i-1] + b) %% m
  }
  
  return(x)
}

StandardMinimal <- function(k, seed)
{
  a <- 16807
  b <- 0
  m <- 2^31 - 1
  
  # Create first element based on seed.
  x <- c((a * seed + b) %% m)
  
  # Iterate over every state
  for (i in 2:k)
  {
    x[i] <- (a * x[i-1] + b) %% m
  }
  
  return(x)
}

# --- Question 2 ---
Frequency <- function(x, nb)
{
  pValues = c()
  binSeq = c()
  
  # For every element of x
  for (i in 1:length(x))
  {
    # Create a binary representation
    bin <- binary(x[i])
    
    # Add to the sequence
    binSeq <- append(binSeq, bin)
    
    # If the sequence is long enougth
    if (length(binSeq) >= nb)
    {
      # Convert 0 -> -1
      binSeq <- (binSeq * 2) - 1
      
      # We're only interested in the first nb bits
      binSeq <- binSeq[1:nb]
      
      # Compute pValue
      sObs <- abs(sum(binSeq)) / sqrt(nb)
      pValue <- 2 * (1 - pnorm(sObs))
      pValues <- append(pValues, pValue)
      
      # Reset current sequence
      binSeq <- c()
    }
  }
  return(pValues)
}

