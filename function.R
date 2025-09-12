compute_spring_objectives <- function(norm_input) {
  
  # Define real variable ranges
  ranges <- list(
    d    = c(0.207, 0.5),
    D    = c(0.6, 3.0),
    Pc   = c(10, 1000),
    rho  = c(7.8, 8.0),
    q    = c(0.1, 10),
    F    = c(100, 1000),
    G    = c(11.5*10^(-6), 12*10^(-6)),
    Li   = c(10, 20)
  )
  
  # Map from [-1, 1] to actual range
  to_real_range <- function(x, min, max) {
    return ((x + 1) / 2 * (max - min) + min)
  }
  real_values <- rep(1, 8)
  for(i in 1:length(ranges)) {
    real_values[i] <- to_real_range(norm_input[i], ranges[[i]][1], ranges[[i]][2])
  }
  
  # Unpack real values
  d    <- real_values[1]
  D    <- real_values[2]
  Pc   <- real_values[3]
  rho  <- real_values[4]
  q    <- real_values[5]
  F    <- real_values[6]
  G <- real_values[7]
  Li <- real_values[8]
  
  # Constants
  g  <- 9.81
  
  # Derived variables
  A <- pi * d^2 / 4       # Cross-sectional area
  Fc <- Pc * A            # f1
  delta <- d / 4          # f2
  Fd <- (144 * rho * q^2) / (g * A)   # f3
  R <- Fd / delta         # f4
  C <- D / d              # f10
  K <- (4 * C - 1) / (4 * C - 4) + 0.61 / C   # f9
  S <- (2.55 * F * D / d^3) * K      # f5
  N <- (G * d^4) / (8 * D^3 * R)     # f6
  Ls <- d * (N + 2)                  # f7
  Lf <- Li + Fc / R                 # f8
  
  return(c(
    Fc,
    delta,
    Fd,
    R,
    S,
    N,
    Ls,
    Lf,
    K,
    C
  ))
}
