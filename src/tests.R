# --- Question 2 ---
Frequency <- function(x, nb)
{
  bitSum <- 0
  nbBits <- 0
  
  # For every element of x
  for (i in 1:length(x))
  {
    # Create a binary representation
    bitSeq <- binary(x[i])
    
    # Convert 0 -> -1
    bitSeq <- (bitSeq * 2) - 1
    
    # We're only interested in the first nb bits
    bitSeq <- bitSeq[1:nb]
    
    nbBits <- nbBits + length(bitSeq)
  
    # Sum-up bits
    bitSum <- bitSum + sum(bitSeq)
  }
  
  sObs <- abs(bitSum) / sqrt(nbBits)
  pValue <- 2 * (1 - pnorm(sObs))
  
  return(pValue)
}

# --- Question 4 ---
Runs <- function(x, nb)
{
  nbBits <- 0
  bitSum <- 0
  Vn <- 1
  lastBit <- 0
  
  # For every element of x
  for (i in 1:length(x))
  {
    # Create a binary representation
    bitSeq <- binary(x[i])
    
    # We're only interested in the first nb bits
    bitSeq <- bitSeq[1:nb]
    
    # Count number of '1'
    bitSum <- bitSum + sum(bitSeq)
    
    # Add to processed bits
    nbBits <- nbBits + length(bitSeq)
    
    # Ajust bitSeq with bit from last value
    if (i > 1) # We have a lastBit to use
    {
      bitSeq <- append(bitSeq, lastBit, 0)
    }
    
    # Compute Vn
    for (k in 1:(length(bitSeq) - 1))
    {
      if (bitSeq[k] != bitSeq[k + 1])
      {
        Vn <- Vn + 1
      }
    }
    
    # Store last bit for next iteration
    lastBit <- bitSeq[length(bitSeq)]
  }
  
  pi <- bitSum / nbBits
  
  if (abs(pi - 0.5) >= 2 * sqrt(nbBits))
  {
    return(0.0)
  }
  
  pValue <- 2 * (1 - pnorm(abs(Vn - 2 * nbBits * pi * (1 - pi)) / (2 * sqrt(nbBits) * pi * (1 - pi))))
  return(pValue)
}