# R code to draw an elephant from four parameters (five if you want an eye and a wiggling trunk)
# (see Mayer et al. 2010 [Am. J. Phys. 78:648-649] for details)

# elephant_parameters <- c(50 - 30i, 18 + 8i, 12 - 10i, -14 - 60i, 40 + 20i)

# helper function to calculate a Fourier series
fourier_series <- function(t_param, c_param) {
  
  out <- rep(0, length(t_param))
  
  a_param <- Re(c_param)
  b_param <- Im(c_param)
  
  for (i in seq_along(c_param))
    out <- out + a_param[i] * cos(i * t_param) + b_param[i] * sin(i * t_param)
  
  out
  
}

# function to create the elephant outline with an eye
elephant <- function(t_param, parameters) {
  
  npar <- 6
  
  Cx <- rep(0 + 0i, npar)
  Cy <- rep(0 + 0i, npar)
  
  Cx[1] <- Re(parameters[1]) * 1i
  Cx[2] <- Re(parameters[2]) * 1i
  Cx[3] <- Re(parameters[3])
  Cx[5] <- Re(parameters[4])
  
  Cy[1] <- Im(parameters[4]) + Im(parameters[1]) * 1i
  Cy[2] <- Im(parameters[2]) * 1i
  Cy[3] <- Im(parameters[3]) * 1i
  
  # eye_parameter
  x <- fourier_series(t_param, Cy)
  y <- fourier_series(t_param, Cx)
  
  eye_x <- Im(parameters[5])
  eye_y <- Im(parameters[5])
  
  list(x = x, y = -y, eye_x = eye_x, eye_y = eye_y)
  
}
