#LIBRARIES
library(readr)

library(quantmod)

#FUNCTIONS 
read.bossa.data <- function(vec.names) { 
  p <- length(vec.names)
  n1 <- 20000
  dates <- matrix(99999999, p, n1)
  closes <- matrix(0, p, n1)
  max.n2 <- 0
  
  for (i in 1:p) {
    filename <- paste("OneDrive/Year 3/ST326/Summative/stocks/",vec.names[i], ".txt", sep="")
    tmp <- scan(filename, list(NULL,date=numeric(), NULL, NULL, NULL, close=numeric(), NULL), skip=1, sep=",") 
    n2 <- length(tmp$date)
    max.n2 <- max(n2, max.n2) 
    dates[i,1:n2] <- tmp$date 
    closes[i,1:n2] <- tmp$close 
  }
  
  dates <- dates[,1:max.n2] 
  closes <- closes[,1:max.n2]
  
  days <- rep(0, n1)
  arranged.closes <- matrix(0, p, n1)
  
  date.indices <- starting.indices <- rep(1, p) 
  already.started <- rep(0, p)
  day <- 1
  
  while(max(date.indices) <= max.n2) { 
    current.dates <- current.closes <- rep(0, p) 
    for (i in 1:p) {
      current.dates[i] <- dates[i,date.indices[i]] 
      current.closes[i] <- closes[i,date.indices[i]] 
    }
    
    min.indices <- which(current.dates == min(current.dates))
    days[day] <- current.dates[min.indices[1]]
    arranged.closes[min.indices,day] <- log(current.closes[min.indices]) 
    arranged.closes[-min.indices,day] <- arranged.closes[-min.indices, max(day-1, 1)]
    
    already.started[min.indices] <- 1
    starting.indices[-which(already.started == 1)] <- starting.indices[-which(already.started == 1)] + 1
    day <- day + 1
    date.indices[min.indices] <- date.indices[min.indices] + 1 
  }
  
  days <- days[1:(day-1)]
  arranged.closes <- arranged.closes[,1:(day-1)]
  
  max.st.ind <- max(starting.indices)
  
  r <- matrix(0, p, (day-max.st.ind-1))
  for (i in 1:p) {
    r[i,] <- diff(arranged.closes[i,max.st.ind:(day-1)]) 
    r[i,] <- r[i,] / sqrt(var(r[i,]))
    r[i,r[i,]==0] <- rnorm(sum(r[i,]==0))
  }
  
  return(list(dates=dates, closes=closes, days=days, arranged.closes=arranged.closes, starting.indices=starting.indices, r=r)) 
}

pred.footsie.prepare <- function(max.lag = 5, split = c(50, 25), mask = rep(1, 10)) {
  # this function prepares the data for the prediction exercise and splits them into a train, validation and test sets
  # max.lag - the maximum FT-SE100 lag to include in the prediction
  # split - how much of the data (in percentage terms) to include in the training and validation sets, respectively
  # mask - which other indices to include (1 for yes, 0 for no)
  
  ind <- read.bossa.data(c("GSPC", "AAPL", "AMZN", "AVGO", "GOOGL", "IBM", "META", "MSFT", "NVDA", "PLTR", "TSLA"))
  d <- dim(ind$r)
  start.index <- max(3, max.lag + 1)
  y <- matrix(0, d[2] - start.index + 1, 1)
  x <- matrix(0, d[2] - start.index + 1, d[1] - 1 + max.lag)
  y[,1] <- ind$r[1,start.index:d[2]]
  for (i in 1:max.lag) {
    x[,i] <- ind$r[1,(start.index-i):(d[2]-i)]
  }
  shift.indices <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)  ## For American and Latin American exchanges, we look at data up to t-1 if t is current time
  for (i in 2:(d[1])) {
    x[,i+max.lag-1] <- ind$r[i,(start.index-1-shift.indices[i-1]):(d[2]-
                                                                     1-shift.indices[i-1])]
  }
  end.training <- round(split[1] / 100 * d[2])
  end.validation <- round(sum(split[1:2]) / 100 * d[2])
  x <- x[,as.logical(c(rep(1, max.lag), mask))]
  y.train <- as.matrix(y[1:end.training], end.training, 1)
  x.train <- x[1:end.training,]
  y.valid <- as.matrix(y[(end.training+1):(end.validation)], end.validation-
                         end.training, 1)
  x.valid <- x[(end.training+1):(end.validation),]
  y.test <- as.matrix(y[(end.validation+1):(d[2] - start.index + 1)], d[2]-
                        start.index-end.validation+1, 1)
  x.test <- x[(end.validation+1):(d[2] - start.index + 1),]
  list(x=x, y=y, x.train=x.train, y.train=y.train, x.valid=x.valid, y.valid=y.valid, x.test=x.test, y.test=y.test)
  
}

vol.exp.sm <- function(x, lambda) {
  # Exponential smoothing of x^2 with parameter lambda
  sigma2 <- x^2
  n <- length(x)
  for (i in 2:n)
    sigma2[i] <- sigma2[i-1] * lambda + x[i-1]^2 * (1-lambda)
  
  sigma <- sqrt(sigma2)
  resid <- x/sigma
  resid[is.na(resid)] <- 0
  sq.resid <- resid^2
  list(sigma2=sigma2, sigma=sigma, resid = resid, sq.resid = sq.resid)
}

negloglik_lambda <- function(lambda, x) {
  # x      : vector of returns (training data for ONE asset)
  # lambda : smoothing parameter in (0,1)
  
  
  if (lambda <= 0 || lambda >= 1) return(1e10) # penalise invalid lambda heavily
  
  n <- length(x)
  sigma2 <- numeric(n)
  
  sigma2[1] <- x[1]^2
  
  for (t in 2:n) {
    sigma2[t] <- lambda * sigma2[t-1] + (1 - lambda) * x[t-1]^2
  }
  
  sigma2[sigma2 <= 0] <- .Machine$double.eps # avoid log(0) just in case
  
  ll <- -0.5 * sum(log(2 * pi * sigma2) + x^2 / sigma2) # Gaussian log-likelihood
  
  return(-ll) # optim() minimises, so return negative log-likelihood
}

estimate_lambda_mle <- function(x, start = 0.9) {
  # x     : vector of returns for one asset
  # start : starting value for lambda
  
  opt <- optim(
    par   = start,
    fn    = negloglik_lambda,
    x     = x,
    method = "Brent",
    lower  = 0.0001,
    upper  = 0.9999
  ) # optimise function 
  
  list(
    lambda_hat = opt$par,
    convergence = opt$convergence
  )
}

estimate_all_lambda <- function(R.train) {
  p <- ncol(R.train)
  assets <- colnames(R.train)
  
  lambda_hat <- numeric(p)
  loglik     <- numeric(p)
  
  for (j in seq_len(p)) {
    xj  <- as.numeric(R.train[, j])          
    fit <- estimate_lambda_mle(xj)
    lambda_hat[j] <- fit$lambda_hat
  }
  
  data.frame(
    asset      = assets,
    lambda_hat = lambda_hat
  )
}

thresh.reg <- function(x, y, th, x.pred = NULL) {
  # estimation of beta in y = a + x beta + epsilon (linear regression)
  # but only using those covariates in x whose marginal correlation
  # with y exceeds th
  # use th = 0 for full regression
  # note the intercept is added
  # x.pred is a new x for which we wish to make prediction
  d <- dim(x)
  ind <- (abs(cor(x, y)) > th)
  n <- sum(ind)
  new.x <- matrix(c(rep(1, d[1]), x[,ind]), d[1], n+1)    
  gram = t(new.x) %*% new.x
  beta <- solve(gram) %*% t(new.x) %*% matrix(y, d[1], 1)
  ind.ex <- c(1, as.numeric(ind))
  ind.ex[ind.ex == 1] <- beta
  condnum = max(svd(gram)$d)/min(svd(gram)$d)
  
  pr <- 0
  if (!is.null(x.pred)) pr <- sum(ind.ex * c(1, x.pred))
  list(beta = ind.ex, pr=pr, condnum = condnum)
}

first.acf.squares.train <- function(x, lambda_y, lambda_x) {
  # x is an object returned by pred.footsie.prepare
  # lambda_y  : scalar, volatility smoothing for the response y
  # lambda_x  : vector, length = number of columns in x$x.train
  
  d <- dim(x$x.train)     # d[1] = n, d[2] = p (number of covariates)
  p <- d[2]
  
  if (length(lambda_x) != p) {
    stop("length(lambda_x) must equal number of covariates (ncol(x$x.train)).")
  }
  
  # containers
  x.train.dev <- x$x.train
  y.train.dev <- x$y.train
  x.valid.dev <- x$x.valid
  y.valid.dev <- x$y.valid
  x.test.dev  <- x$x.test
  y.test.dev  <- x$y.test
  
  ss <- 0  # sum of |first ACF| over all series
  
  # --- de-volatilise each covariate with its own lambda_x[j] ---
  for (j in 1:p) {
    # TRAIN
    v.tr <- vol.exp.sm(x$x.train[, j], lambda_x[j])
    x.train.dev[, j] <- v.tr$resid
    ss <- ss + abs(acf(v.tr$sq.resid, plot = FALSE)$acf[2])
    
    # VALID
    v.va <- vol.exp.sm(x$x.valid[, j], lambda_x[j])
    x.valid.dev[, j] <- v.va$resid
    
    # TEST
    v.te <- vol.exp.sm(x$x.test[, j], lambda_x[j])
    x.test.dev[, j] <- v.te$resid
  }
  
  # --- de-volatilise response y with its own lambda_y ---
  v_y_tr <- vol.exp.sm(x$y.train, lambda_y)
  y.train.dev <- v_y_tr$resid
  ss <- ss + abs(acf(v_y_tr$sq.resid, plot = FALSE)$acf[2])
  
  v_y_va <- vol.exp.sm(x$y.valid, lambda_y)
  y.valid.dev <- v_y_va$resid
  
  v_y_te <- vol.exp.sm(x$y.test, lambda_y)
  y.test.dev <- v_y_te$resid
  
  list(
    ss          = ss,
    y.train.dev = y.train.dev,
    x.train.dev = x.train.dev,
    y.valid.dev = y.valid.dev,
    x.valid.dev = x.valid.dev,
    y.test.dev  = y.test.dev,
    x.test.dev  = x.test.dev
  )
}

rolling.thresh.reg <- function(x, lambda_y, lambda_x, th, win, warmup, reg.function = thresh.reg) {
  # performs prediction over a rolling window of size win over the training set
  # x         : output of pred.footsie.prepare
  # lambda_y  : scalar
  # lambda_x  : vector (length = #covariates)
  # th        : threshold for thresh.reg
  # win       : window length D
  # warmup    : t0 (first time we start predicting)
  
  xx <- first.acf.squares.train(x, lambda_y, lambda_x)
  n  <- length(xx$y.train.dev)
  
  
  predi   <- truth <- condnum <- rep(0, n - warmup + 1)
  
  for (i in warmup:n) {
    y   <- xx$y.train.dev[(i - win):(i - 1)]
    xxx <- xx$x.train.dev[(i - win):(i - 1), ]
    
    zz <- reg.function(xxx, y, th, xx$x.train.dev[i, ])
    predi[i - warmup + 1]   <- zz$pr
    condnum[i - warmup + 1] <- zz$condnum
    truth[i - warmup + 1]   <- xx$y.train.dev[i]
  }
  
  pos <- sign(predi)          # trading signal
  ret <- pos * truth          # strategy return
  
  err <- sqrt(250) * mean(ret) / sqrt(var(ret))  # annualised Sharpe
  
  list(err = err, predi = predi, truth = truth, condnum = condnum)
}

rolling.thresh.reg.valid <- function(x, lambda_y, lambda_x, th, win, warmup, reg.function = thresh.reg) {
  xx <- first.acf.squares.train(x, lambda_y, lambda_x)
  n  <- length(xx$y.valid.dev)
  
  
  predi   <- truth <- condnum <- rep(0, n - warmup + 1)
  
  for (i in warmup:n) {
    y   <- xx$y.valid.dev[(i - win):(i - 1)]
    xxx <- xx$x.valid.dev[(i - win):(i - 1), ]
    
    zz <- reg.function(xxx, y, th, xx$x.valid.dev[i, ])
    predi[i - warmup + 1]   <- zz$pr
    condnum[i - warmup + 1] <- zz$condnum
    truth[i - warmup + 1]   <- xx$y.valid.dev[i]
  }
  
  pos <- sign(predi)          # trading signal
  ret <- pos * truth
  err <- sqrt(250) * mean(ret) / sqrt(var(ret))
  
  list(err = err, predi = predi, truth = truth, condnum = condnum)
}

rolling.thresh.reg.test <- function(x, lambda_y, lambda_x, th, win, warmup, reg.function = thresh.reg) {
  xx <- first.acf.squares.train(x, lambda_y, lambda_x)
  n  <- length(xx$y.test.dev)
  
  
  predi   <- truth <- condnum <- rep(0, n - warmup + 1)
  
  for (i in warmup:n) {
    y   <- xx$y.test.dev[(i - win):(i - 1)]
    xxx <- xx$x.test.dev[(i - win):(i - 1), ]
    
    zz <- reg.function(xxx, y, th, xx$x.test.dev[i, ])
    predi[i - warmup + 1]   <- zz$pr
    condnum[i - warmup + 1] <- zz$condnum
    truth[i - warmup + 1]   <- xx$y.test.dev[i]
  }
  
  pos <- sign(predi)          # trading signal
  ret <- pos * truth
  err <- sqrt(250) * mean(ret) / sqrt(var(ret))
  
  list(err = err, predi = predi, truth = truth, condnum = condnum)
}

sharpe.curves <- function(x, lambda_y, lambda_x, th, warmup, reg.function = thresh.reg,win = seq(from = 50, to = warmup - 10, by = 20)) {
  # computes Sharpe ratios for a sequence of rolling windows win
  # for train, validation and test sets
  
  w <- length(win)
  train.curve <- valid.curve <- test.curve <- numeric(w)
  
  # run first to get condnum lengths
  rreg       <- rolling.thresh.reg(x,        lambda_y, lambda_x, th, win[1], warmup, reg.function)
  rreg.valid <- rolling.thresh.reg.valid(x,  lambda_y, lambda_x, th, win[1], warmup, reg.function)
  rreg.test  <- rolling.thresh.reg.test(x,   lambda_y, lambda_x, th, win[1], warmup, reg.function)
  
  condnum        <- matrix(0, w, length(rreg$condnum))
  condnum.valid  <- matrix(0, w, length(rreg.valid$condnum))
  condnum.test   <- matrix(0, w, length(rreg.test$condnum))
  
  train.curve[1] <- rreg$err
  valid.curve[1] <- rreg.valid$err
  test.curve[1]  <- rreg.test$err
  
  condnum[1, ]        <- rreg$condnum
  condnum.valid[1, ]  <- rreg.valid$condnum
  condnum.test[1, ]   <- rreg.test$condnum
  
  if (w >= 2) {
    for (i in 2:w) {
      rreg       <- rolling.thresh.reg(x,       lambda_y, lambda_x, th, win[i], warmup, reg.function)
      rreg.valid <- rolling.thresh.reg.valid(x, lambda_y, lambda_x, th, win[i], warmup, reg.function)
      rreg.test  <- rolling.thresh.reg.test(x,  lambda_y, lambda_x, th, win[i], warmup, reg.function)
      
      train.curve[i] <- rreg$err
      valid.curve[i] <- rreg.valid$err
      test.curve[i]  <- rreg.test$err
      
      condnum[i, ]       <- rreg$condnum
      condnum.valid[i, ] <- rreg.valid$condnum
      condnum.test[i, ]  <- rreg.test$condnum
    }
  }
  
  list(
    train.curve    = train.curve,
    valid.curve    = valid.curve,
    test.curve     = test.curve,
    condnum        = condnum,
    condnum.valid  = condnum.valid,
    condnum.test   = condnum.test,
    win            = win
  )
}

rolling.pcr <- function(x.dev, y.dev, win, warmup, k = 1) {
  # x.dev : matrix of de-volatilised covariates (n x p)
  # y.dev : vector of de-volatilised response (length n)
  # win   : window length D
  # warmup: first time index we start predicting (t0)
  # k     : number of principal components (1 or 2)
  
  n <- length(y.dev)
  if (warmup >= n)
    stop("warmup must be smaller than length(y.dev)")
  
  n.pred <- n - warmup           
  predi  <- truth <- numeric(n.pred)
  
  for (idx in seq_len(n.pred)) {
    i     <- warmup - 1 + idx    
    start <- i - win + 1
    end   <- i                   
    
    Xwin <- x.dev[start:end, , drop = FALSE]
    ywin <- y.dev[start:end]
    
    # PCA on covariates in the rolling window
    pc <- prcomp(Xwin, center = TRUE, scale. = FALSE)
    
    scores <- pc$x[, 1:k, drop = FALSE]          
    
    # OLS of y on k factors
    df_fit <- data.frame(y = ywin, scores)       
    fit    <- lm(y ~ ., data = df_fit)
    
    # factor scores for time i+1 (the "next" observation we predict)
    x_next     <- matrix(x.dev[i + 1, ], nrow = 1)
    score_next <- predict(pc, newdata = x_next)[, 1:k, drop = FALSE]
    score_next <- as.data.frame(score_next)      
    
    predi[idx] <- predict(fit, newdata = score_next) # prediction
    
    truth[idx] <- y.dev[i + 1]
  }
  
  ## trading strategy: position = sign(prediction)
  pos    <- sign(predi)
  ret    <- pos * truth
  sharpe <- sqrt(250) * mean(ret) / sd(ret)
  
  list(
    sharpe = sharpe,
    predi  = predi,
    truth  = truth
  )
}

# QUESTION 1
# Download the asset data over the last five years 
vec.names <- c("GSPC", "AAPL", "AMZN", "AVGO", "GOOGL", "IBM", "META", "MSFT", "NVDA", "PLTR", "TSLA")

for (t in vec.names) {
  
  yahoo_ticker <- ifelse(t == "GSPC", "^GSPC", t) # Adjust for ^
  
  getSymbols(yahoo_ticker, src = "yahoo",
             from = "2020-12-01",
             to   = "2025-12-01",
             auto.assign = TRUE)
  
  xts_obj <- get(yahoo_ticker)
  
  # Build dataframe
  df <- data.frame(
    TICKER      = t,
    DTYYYYMMDD  = format(index(xts_obj), "%Y%m%d"),
    OPEN        = as.numeric(xts_obj[,1]),
    HIGH        = as.numeric(xts_obj[,2]),
    LOW         = as.numeric(xts_obj[,3]),
    CLOSE       = as.numeric(xts_obj[,4]),
    VOL         = as.numeric(xts_obj[,5])
  )
  
  # Create output file path
  out_path <- paste0("OneDrive/Year 3/ST326/Summative/stocks/", t, ".txt")
  
  # Write file
  write.csv(df, out_path, row.names = FALSE, quote = FALSE)
  
  cat("Saved:", out_path, "\n")
}

# Plot log prices 
ind <- read.bossa.data(vec.names)
mn <- max(ind$starting.indices)
mx <- dim(ind$arranged.closes)[2]

p <- length(vec.names)

mat <- ind$arranged.closes[1:p, mn:mx, drop = FALSE]

par(plt = c(0.1, 0.75, 0.15, 0.9))

ts.plot(
  t(mat),
  col  = 1:p,
  lty  = 1:p,
  xlab = "Time",
  ylab = "Log price",
  main = "Aligned log prices"
)

legend(
  "topright",
  inset = c(-0.35, 0),
  legend = vec.names,
  col    = 1:p,
  lty    = 1:p,
  bty    = "n"
)

# QUESTION 2
# Split the data for q = 0 into training, testing and validation data
data_q0 <- pred.footsie.prepare(max.lag = 0)

# Put the training data in one matrix
R.train <- cbind(
  GSPC  = as.vector(data_q0$y.train),
  AAPL  = data_q0$x.train[, 1],
  AMZN  = data_q0$x.train[, 2],
  AVGO  = data_q0$x.train[, 3],
  GOOGL = data_q0$x.train[, 4],
  IBM   = data_q0$x.train[,5],
  META  = data_q0$x.train[,6],
  MSFT  = data_q0$x.train[,7],
  NVDA  = data_q0$x.train[,8],
  PLTR  = data_q0$x.train[,9],
  TSLA  = data_q0$x.train[,10]
)

# Estimate lambda
lambda_table <- estimate_all_lambda(R.train)
lambda.vec <- lambda_table$lambda_hat

lambda_y    <- lambda_table$lambda_hat[lambda_table$asset == "GSPC"]
lambda_x_q0 <- lambda_table$lambda_hat[lambda_table$asset != "GSPC"]

n.assets <- ncol(R.train)
n <- nrow(R.train)

# store objects (sigma, residuals, etc.) for each series
vol.list <- vector("list", n.assets)
names(vol.list) <- colnames(R.train)

# matrix of daily volatility estimates 
sigma.train <- matrix(NA, nrow = n, ncol = n.assets)
colnames(sigma.train) <- colnames(R.train)

# Exponential filtering using each lambda for each asset
for (j in 1:n.assets) {
  xj <- R.train[, j]                    # series j
  lambda_j <- lambda.vec[j]             # MLE lambda for series j
  vj <- vol.exp.sm(xj, lambda_j)        # use series-specific lambda
  vol.list[[j]] <- vj
  sigma.train[, j] <- vj$sigma
}

#Plotting daily volatilities
for (k in 1:n.assets){
  stock = c("GSPC", "AAPL", "AMZN", "AVGO", "GOOGL", "IBM", "META", "MSFT",
            "NVDA", "PLTR", "TSLA")
  
  plot(sigma.train[,stock[k]], type="l")
}

#QUESTION 4
data_q1 <- pred.footsie.prepare(max.lag = 1)
lambda_x_q1 <- c(lambda_y, lambda_x_q0)

warmup  <- 250                 # roughly 1 year
win.vec <- seq(50, 250, 20)    

# Find the sharpe curve for q = 0 using the lambda from before
# x has 10 covariates (AAPL,...,TSLA)
sc_q0 <- sharpe.curves(
  x          = data_q0,
  lambda_y   = lambda_y,
  lambda_x   = lambda_x_q0,
  th         = 0,
  warmup     = warmup,
  reg.function = thresh.reg,
  win        = win.vec
)

# Find the sharpe curve for q = 1 using the lambda from before
# x has 11 covariates (lagged GSPC + 10 stocks)
sc_q1 <- sharpe.curves(
  x          = data_q1,
  lambda_y   = lambda_y,
  lambda_x   = lambda_x_q1,
  th         = 0,
  warmup     = warmup,
  reg.function = thresh.reg,
  win        = win.vec
)


# Plotting sharpe curves for q = 0
par(mfrow = c(1, 1))

matplot(
  win.vec,
  cbind(sc_q0$train.curve, sc_q0$valid.curve, sc_q0$test.curve),
  type = "b", pch = 1:3, lty = 1,
  xlab = "Window length D",
  ylab = "Sharpe ratio",
  main = "Sharpe ratios vs D (q = 0)"
)

legend("bottomright",
       legend = c("Train", "Validation", "Test"),
       col = 1:3, pch = 1:3, lty = 1, bty = "n")


# Plotting sharpe curves for q = 1
par(mfrow = c(1, 1))

matplot(
  win.vec,
  cbind(sc_q1$train.curve, sc_q1$valid.curve, sc_q1$test.curve),
  type = "b", pch = 1:3, lty = 1,
  xlab = "Window length D",
  ylab = "Sharpe ratio",
  main = "Sharpe ratios vs D (q = 1)"
)

legend("bottomright",
       legend = c("Train", "Validation", "Test"),
       col = 1:3, pch = 1:3, lty = 1, bty = "n")


#Checking appropriateness of OLS

# Checking the window values for two highest peaks in the sharp ratio plot
# q = 0
vals0 <- sc_q0$valid.curve
wins0 <- sc_q0$win
ord0  <- order(vals0, decreasing = TRUE)

best.idx.q0  <- ord0[2]          # 2nd best index
best.win.q0  <- wins0[best.idx.q0]

# q = 1
vals1 <- sc_q1$valid.curve
wins1 <- sc_q1$win
ord1  <- order(vals1, decreasing = TRUE)

best.idx.q1  <- ord1[2]          # 2nd best index
best.win.q1  <- wins1[best.idx.q1]

# Hard-code best window length
best.win.q0 <- 190
best.win.q1 <- 190

# Find corresponding indices in the win vectors
best.idx.q0 <- which(sc_q0$win == best.win.q0)
best.idx.q1 <- which(sc_q1$win == best.win.q1)

# Condition numbers for chosen D
cond_q0_train <- sc_q0$condnum[best.idx.q0, ]
cond_q1_train <- sc_q1$condnum[best.idx.q1, ]

# q = 0 condition numbers on TRAINING set for chosen D
plot(cond_q0_train, type = "l",
     main = paste("Cond. numbers (training, q = 0, D =", best.win.q0, ")"),
     xlab = "Rolling fit index", ylab = "condnum")

# q = 1 condition numbers on TRAINING set for chosen D
plot(cond_q1_train, type = "l",
     main = paste("Cond. numbers (training, q = 1, D =", best.win.q1, ")"),
     xlab = "Rolling fit index", ylab = "condnum")


# q = 0 condition numbers on TEST set for chosen D
plot.ts(sc_q0$condnum.test[best.idx.q0, ],
        main = paste("Cond. numbers (test, q = 0, D =", best.win.q0, ")"),
        xlab = "Rolling fit index", ylab = "condnum")

# q = 1 condition numbers on TEST set for chosen D
plot.ts(sc_q1$condnum.test[best.idx.q1, ],
        main = paste("Cond. numbers (test, q = 1, D =", best.win.q1, ")"),
        xlab = "Rolling fit index", ylab = "condnum")


# q = 0 condition numbers on VALIDATION set for chosen D
plot.ts(sc_q0$condnum.valid[best.idx.q0, ],
        main = paste("Cond. numbers (validation, q = 0, D =", best.win.q0, ")"),
        xlab = "Rolling fit index", ylab = "condnum")

# q = 1 condition numbers on VALIDATION set for chosen D
plot.ts(sc_q1$condnum.valid[best.idx.q1, ],
        main = paste("Cond. numbers (validation, q = 1, D =", best.win.q1, ")"),
        xlab = "Rolling fit index", ylab = "condnum")


# Rolling regression on VALIDATION set, q = 0
vroll_q0_valid <- rolling.thresh.reg.valid(
  x          = data_q0,
  lambda_y   = lambda_y,
  lambda_x   = lambda_x_q0,
  th         = 0,
  win        = best.win.q0,
  warmup     = warmup,
  reg.function = thresh.reg
)

# Positions / predicted returns over time
vroll_q0_valid$predi

# Plot positions

vroll_q1_valid <- rolling.thresh.reg.valid(
  x          = data_q1,
  lambda_y   = lambda_y,
  lambda_x   = lambda_x_q1,
  th         = 0,
  win        = best.win.q1,
  warmup     = warmup,
  reg.function = thresh.reg
  
)
plot.ts(vroll_q0_valid$predi,
        main = paste("Predicted positions (validation, q = 0, D =", best.win.q1, ")"),
        ylab = "Predicted return / position", xlab = "Time")

plot.ts(vroll_q1_valid$predi,
        main = paste("Predicted positions (validation, q = 1, D =", best.win.q1, ")"),
        ylab = "Predicted return / position", xlab = "Time")

# QUESTION 5
warmup  <- 250
win.vec <- seq(50, 250, 20) 
q <- 1

# remove volatility as before
data.dev <- first.acf.squares.train(data_q0, lambda_y, lambda_x_q0)

y.train <- data.dev$y.train.dev
x.train <- data.dev$x.train.dev
y.valid <- data.dev$y.valid.dev
x.valid <- data.dev$x.valid.dev
y.test  <- data.dev$y.test.dev
x.test  <- data.dev$x.test.dev

# Scree plots
pc_full <- prcomp(x.train, center = TRUE, scale. = FALSE)
plot(pc_full, type = "l", main = "Scree plot of training covariates (q = 0)")

pc_full <- prcomp(x.test, center = TRUE, scale. = FALSE)
plot(pc_full, type = "l", main = "Scree plot of testing covariates (q = 0)")

pc_full <- prcomp(x.valid, center = TRUE, scale. = FALSE)
plot(pc_full, type = "l", main = "Scree plot of validation covariates (q = 0)")


sr.train.1 <- sr.valid.1 <- sr.test.1 <- numeric(length(win.vec))
sr.train.2 <- sr.valid.2 <- sr.test.2 <- numeric(length(win.vec))

for (j in seq_along(win.vec)) {
  D <- win.vec[j]
  
  # k = 1 factor
  res.tr.1 <- rolling.pcr(x.train, y.train, win = D, warmup = warmup, k = 1)
  res.va.1 <- rolling.pcr(x.valid, y.valid, win = D, warmup = warmup, k = 1)
  res.te.1 <- rolling.pcr(x.test,  y.test,  win = D, warmup = warmup, k = 1)
  
  sr.train.1[j] <- res.tr.1$sharpe
  sr.valid.1[j] <- res.va.1$sharpe
  sr.test.1[j]  <- res.te.1$sharpe
  
  # k = 2 factors
  res.tr.2 <- rolling.pcr(x.train, y.train, win = D, warmup = warmup, k = 2)
  res.va.2 <- rolling.pcr(x.valid, y.valid, win = D, warmup = warmup, k = 2)
  res.te.2 <- rolling.pcr(x.test,  y.test,  win = D, warmup = warmup, k = 2)
  
  sr.train.2[j] <- res.tr.2$sharpe
  sr.valid.2[j] <- res.va.2$sharpe
  sr.test.2[j]  <- res.te.2$sharpe
}

par(mfrow = c(1, 1))

# k = 1 factor
matplot(
  win.vec,
  cbind(sr.train.1, sr.valid.1, sr.test.1),
  type = "b", pch = 1:3, lty = 1,
  xlab = "Window length D",
  ylab = "Sharpe ratio",
  main = "PCR (k = 1, q = 0)"
)
legend("bottomright",
       legend = c("Train", "Validation", "Test"),
       pch = 1:3, lty = 1, bty = "n")

# k = 2 factors
matplot(
  win.vec,
  cbind(sr.train.2, sr.valid.2, sr.test.2),
  type = "b", pch = 1:3, lty = 1,
  xlab = "Window length D",
  ylab = "Sharpe ratio",
  main = "PCR (k = 2, q = 0)"
)
legend("bottomright",
       legend = c("Train", "Validation", "Test"),
       pch = 1:3, lty = 1, bty = "n")

par(mfrow = c(1, 1))

# vectorise it for convenience
valid_all <- c(sr.valid.1, sr.valid.2)
D_all     <- rep(win.vec, 2)
k_all     <- c(rep(1, length(win.vec)), rep(2, length(win.vec)))

best_idx <- which.max(valid_all)
best_D_pcr <- D_all[best_idx]
best_k_pcr <- k_all[best_idx]

best_D_pcr
best_k_pcr
 
# Combined Sharpe curves

win_q0 <- sc_q0$win   

# Common y-range for all panels
y.range <- range(
  sc_q0$train.curve, sc_q0$valid.curve, sc_q0$test.curve,
  sr.train.1, sr.valid.1, sr.test.1,
  sr.train.2, sr.valid.2, sr.test.2,
  na.rm = TRUE
)

par(mfrow = c(1, 1)) 

# Training data
plot(
  win_q0, sc_q0$train.curve,
  type = "b", pch = 16, lty = 1, col = "black",
  xlab = "Window length D", ylab = "Sharpe ratio",
  ylim = y.range,
  main = "Sharpe vs D (Train, q = 0)"
)
lines(win.vec, sr.train.1, type = "b", pch = 17, lty = 2, col = "blue")
lines(win.vec, sr.train.2, type = "b", pch = 15, lty = 3, col = "red")
legend(
  "topright",
  legend = c("Thresh reg", "PCR (k = 1)", "PCR (k = 2)"),
  col    = c("black", "blue", "red"),
  pch    = c(16, 17, 15),
  lty    = c(1, 2, 3),
  bty    = "n"
)

# Test data
plot(
  win_q0, sc_q0$test.curve,
  type = "b", pch = 16, lty = 1, col = "black",
  xlab = "Window length D", ylab = "Sharpe ratio",
  ylim = y.range,
  main = "Sharpe vs D (Test, q = 0)"
)
lines(win.vec, sr.test.1, type = "b", pch = 17, lty = 2, col = "blue")
lines(win.vec, sr.test.2, type = "b", pch = 15, lty = 3, col = "red")
legend(
  "bottomright",
  legend = c("Thresh reg", "PCR (k = 1)", "PCR (k = 2)"),
  col    = c("black", "blue", "red"),
  pch    = c(16, 17, 15),
  lty    = c(1, 2, 3),
  bty    = "n"
)

# Validation data
plot(
  win_q0, sc_q0$valid.curve,
  type = "b", pch = 16, lty = 1, col = "black",
  xlab = "Window length D", ylab = "Sharpe ratio",
  ylim = y.range,
  main = "Sharpe vs D (Validation, q = 0)"
)
lines(win.vec, sr.valid.1, type = "b", pch = 17, lty = 2, col = "blue")
lines(win.vec, sr.valid.2, type = "b", pch = 15, lty = 3, col = "red")
legend(
  "bottomright",
  legend = c("Thresh reg", "PCR (k = 1)", "PCR (k = 2)"),
  col    = c("black", "blue", "red"),
  pch    = c(16, 17, 15),
  lty    = c(1, 2, 3),
  bty    = "n"
)

