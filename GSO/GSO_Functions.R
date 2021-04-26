#### Functions ####
## Functions written to help with the analysis.

# Paul's function

# ## Function to assign growth stage category given the EFG and TSF
# EFGCodes = read_csv("../ReferenceTables/GS_lookupGSO.csv")
# GSfn <- function(efg, tsf, data = EFGCodes) {
#   dat <-
#     c(unique(arrange(data[which(data$EFG_NO == efg), ], BGS_START)$BGS_START), 1000)[-1]
#   c('Juvenility', 'Adolescence', 'Mature', 'Old')[which(dat > tsf)][1]
# }

## Run models on scenarios as well
Scenarios <- function(data, Scen = GSOScen, efg = UsedEFG) {
  Scena <- data.frame(Scenario = unique(Scen$Scenario), GMA = NA)
  for (i in 1:nrow(Scena)) {
    Scena$GMA[i] <-
      geomean.fun(left_join(data.frame(EFG_GS = names(data)[-1]), Scen[which(Scen$Scenario ==
                                                                              Scena$Scenario[i]), ], by = 'EFG_GS')$PercLandscape,
                  data[-1])
  }
  return(Scena)
}

## Use the expert opinion only
Rule0 <- function(data, Max = 1) {
  Resp <- mean(data$Response[which(data$Source == 'Expert')], na.rm = TRUE)
  return(Resp)
}

## If available, use the mean of survey data, otherwise use expert opinion
Rule1 <- function(data) {
  if (sum(data$Source == 'Survey') > 0) {
    datR <- data$Response[which(data$Source == 'Survey')]
    Resp <- mean(datR, na.rm = TRUE)
  }
  else{
    Resp <- mean(data$Response[which(data$Source == 'Expert')])
  }
  return(Resp)
}

## If available, use the maximum of survey data, otherwise use expert opinion
Rule1a <- function(data) {
  if (sum(data$Source == 'Survey') > 0) {
    datR <- data$Response[which(data$Source == 'Survey')]
    Resp <- max(datR, na.rm = TRUE)
  }
  else{
    Resp <- max(data$Response[which(data$Source == 'Expert')])
  }
  return(Resp)
}

## If available, use the median of survey data, otherwise use expert opinion
Rule1b <- function(data) {
  if (sum(data$Source == 'Survey') > 0) {
    datR <- data$Response[which(data$Source == 'Survey')]
    Resp <- median(datR, na.rm = TRUE)
  }
  else{
    Resp <- median(data$Response[which(data$Source == 'Expert')])
  }
  return(Resp)
}

## If available, use the upper quartile (75th quantile) of survey data, otherwise use expert opinion
Rule1c <- function(data) {
  if (sum(data$Source == 'Survey') > 0) {
    datR <- data$Response[which(data$Source == 'Survey')]
    Resp <- quantile(datR, probs = 0.75, na.rm = TRUE)
  }
  else{
    Resp <-
      quantile(data$Response[which(data$Source == 'Expert'), probs = 0.75])
  }
  return(Resp)
}

## If available, use the weighted mean of mean survey data and expert opinion, otherwise use expert opinion
Rule2 <- function(data, Weight = 0.5) {
  ExpOp <- mean(data$Response[which(data$Source == 'Expert')])
  if (sum(data$Source == 'Survey') > 0) {
    datR <- data$Response[which(data$Source == 'Survey')]
    Resp <- Weight * mean(datR) + (1 - Weight) * ExpOp
  }
  else{
    Resp <- ExpOp
  }
  return(Resp)
}

## If available, use the weighted mean of maximum survey data and expert opinion, otherwise use expert opinion
Rule2a <- function(data, Weight = 0.5) {
  ExpOp <- max(data$Response[which(data$Source == 'Expert')])
  if (sum(data$Source == 'Survey') > 0) {
    datR <- data$Response[which(data$Source == 'Survey')]
    Resp <- Weight * max(datR) + (1 - Weight) * ExpOp
  }
  else{
    Resp <- ExpOp
  }
  return(Resp)
}

## If available, use the weighted mean of median survey data and expert opinion, otherwise use expert opinion
Rule2b <- function(data, Weight = 0.5) {
  ExpOp <- median(data$Response[which(data$Source == 'Expert')])
  if (sum(data$Source == 'Survey') > 0) {
    datR <- data$Response[which(data$Source == 'Survey')]
    Resp <- Weight * median(datR) + (1 - Weight) * ExpOp
  }
  else{
    Resp <- ExpOp
  }
  return(Resp)
}

## If available, use the weighted mean of upper quartile (75th quantile) survey data and expert opinion, otherwise use expert opinion
Rule2c <- function(data, Weight = 0.5) {
  ExpOp <-
    quantile(data$Response[which(data$Source == 'Expert'), probs = 0.75])
  if (sum(data$Source == 'Survey') > 0) {
    datR <- data$Response[which(data$Source == 'Survey')]
    Resp <-
      Weight * quantile(datR, probs = 0.75, na.rm = TRUE) + (1 - Weight) * ExpOp
  }
  else{
    Resp <- ExpOp
  }
  return(Resp)
}

## Use the mean of survey data where avaiable, otherwise use nothing
Rule3 <- function(data) {
  if (sum(data$Source == 'Survey') > 0) {
    datR <- data$Response[which(data$Source == 'Survey')]
    Resp <- mean(datR, na.rm = TRUE)
  }
  else{
    Resp <- NA
  }
  return(Resp)
}

## Use the maximum of survey data where avaiable, otherwise use nothing
Rule3a <- function(data) {
  if (sum(data$Source == 'Survey') > 0) {
    datR <- data$Response[which(data$Source == 'Survey')]
    Resp <- max(datR, na.rm = TRUE)
  }
  else{
    Resp <- NA
  }
  return(Resp)
}

## Use the maximum of survey data where avaiable, otherwise use nothing
Rule3b <- function(data) {
  if (sum(data$Source == 'Survey') > 0) {
    datR <- data$Response[which(data$Source == 'Survey')]
    Resp <- median(datR, na.rm = TRUE)
  }
  else{
    Resp <- NA
  }
  return(Resp)
}

## Use the maximum of survey data where avaiable, otherwise use nothing
Rule3c <- function(data) {
  if (sum(data$Source == 'Survey') > 0) {
    datR <- data$Response[which(data$Source == 'Survey')]
    Resp <- quantile(datR, probs = 0.75, na.rm = TRUE)
  }
  else{
    Resp <- NA
  }
  return(Resp)
}

## Function that generates the scores to use in the growth stage optimisation, including random selection
DataGen = function(data,
                   efgs,
                   area = GSOArea,
                   rule = 'Rule0',
                   FT = 'High',
                   Wt = 0.5,
                   Rand = FALSE,
                   NRep = 20) {
  nStages <- length(StageNames)
  SppList = as.data.frame(matrix(
    NA,
    nrow = length(unique(data$TAXON_ID)),
    ncol = nStages * length(efgs) + 1
  ))
  names(SppList) <-
    c('Species', paste(rep(efgs, each = nStages),
                       unique(data$GS[which(data$GS != 'NA')]),
                       sep ='_'))
  SppList$Species = unique(data$TAXON_ID)
  SppSource <- SppList
  data <- data[which(data$FireType == FT), ]
  if (Rand) {
    dataR <- data[which(data$Source == 'Expert' & data$Response >= 0), ]
    dataR$ID <- paste0('s', dataR$TAXON_ID, dataR$EFG_GS)
    dataS <-
      data[which(
        data$Source == 'Survey' &
          paste0('s', data$TAXON_ID, data$EFG_GS) %in%
          unique(dataR$ID)
      ), ]
    if (nrow(dataS) > 0) {
      dataS$ID <- paste0('s', dataS$TAXON_ID, dataS$EFG_GS)
      for (ii in 1:length(unique(dataS$EFG_GS))) {
        dataT <- dataS[which(dataS$EFG_GS == unique(dataS$EFG_GS)[ii]), ]
        Spp <- unique(dataT$TAXON_ID)
        Sites <- unique(dataT$SurvID)
        Selection <-
          sample(1:length(Sites), size = NRep, replace = TRUE)
        for (jj in 1:length(Spp)) {
          dataR <-
            rbind(dataR, dataT[which(dataT$TAXON_ID == Spp[jj]), ][Selection, ])
        }
      }
    }
    data <- dataR
  }
  for (i in 1:nrow(SppList)) {
    dat <- data[which(data$TAXON_ID == SppList$Species[i]), ]
    if (rule == 'Rule0') {
      for (j in 2:ncol(SppList)) {
        dat1 <- dat[which(dat$EFG_GS == names(SppList)[j]), ]
        SppList[i, j] <- Rule0(dat1)
        SppSource[i, j] <- 0
      }
    }
    if (rule == 'Rule1') {
      for (j in 2:ncol(SppList)) {
        dat1 <- dat[which(dat$EFG_GS == names(SppList)[j]), ]
        SppList[i, j] <- Rule1(dat1)
        SppSource[i, j] <- NA
      }
    }
    if (rule == 'Rule1a') {
      for (j in 2:ncol(SppList)) {
        dat1 <- dat[which(dat$EFG_GS == names(SppList)[j]), ]
        SppList[i, j] <- Rule1a(dat1)
        SppSource[i, j] <- NA
      }
    }
    if (rule == 'Rule1b') {
      for (j in 2:ncol(SppList)) {
        dat1 <- dat[which(dat$EFG_GS == names(SppList)[j]), ]
        SppList[i, j] <- Rule1b(dat1)
        SppSource[i, j] <- NA
      }
    }
    if (rule == 'Rule1c') {
      for (j in 2:ncol(SppList)) {
        dat1 <- dat[which(dat$EFG_GS == names(SppList)[j]), ]
        SppList[i, j] <- Rule1c(dat1)
        SppSource[i, j] <- NA
      }
    }
    if (rule == 'Rule2') {
      for (j in 2:ncol(SppList)) {
        dat1 <- dat[which(dat$EFG_GS == names(SppList)[j]), ]
        SppList[i, j] <- Rule2(dat1, Wt)
        SppSource[i, j] <- NA
      }
    }
    if (rule == 'Rule2a') {
      for (j in 2:ncol(SppList)) {
        dat1 <- dat[which(dat$EFG_GS == names(SppList)[j]), ]
        SppList[i, j] <- Rule2a(dat1, Wt)
        SppSource[i, j] <- NA
      }
    }
    if (rule == 'Rule2b') {
      for (j in 2:ncol(SppList)) {
        dat1 <- dat[which(dat$EFG_GS == names(SppList)[j]), ]
        SppList[i, j] <- Rule2b(dat1, Wt)
        SppSource[i, j] <- NA
      }
    }
    if (rule == 'Rule2c') {
      for (j in 2:ncol(SppList)) {
        dat1 <- dat[which(dat$EFG_GS == names(SppList)[j]), ]
        SppList[i, j] <- Rule2c(dat1, Wt)
        SppSource[i, j] <- NA
      }
    }
    if (rule == 'Rule3') {
      for (j in 2:ncol(SppList)) {
        dat1 <- dat[which(dat$EFG_GS == names(SppList)[j]), ]
        SppList[i, j] <- Rule3(dat1)
        SppSource[i, j] <- NA
      }
    }
    if (rule == 'Rule3a') {
      for (j in 2:ncol(SppList)) {
        dat1 <- dat[which(dat$EFG_GS == names(SppList)[j]), ]
        SppList[i, j] <- Rule3a(dat1)
        SppSource[i, j] <- NA
      }
    }
    if (rule == 'Rule3b') {
      for (j in 2:ncol(SppList)) {
        dat1 <- dat[which(dat$EFG_GS == names(SppList)[j]), ]
        SppList[i, j] <- Rule3b(dat1)
        SppSource[i, j] <- NA
      }
    }
    if (rule == 'Rule3c') {
      for (j in 2:ncol(SppList)) {
        dat1 <- dat[which(dat$EFG_GS == names(SppList)[j]), ]
        SppList[i, j] <- Rule3c(dat1)
        SppSource[i, j] <- NA
      }
    }
  }
  Areas <-
    left_join(data.frame(EFG_NO = as.numeric(str_sub(
      names(SppList[, -1]), 4, 5
    )), by = "EFG_NO"), area[, -2])$Area
  SppList[, -1] <- sweep(SppList[, -1], MARGIN = 2, Areas, '*')
  NoZero <- which(rowSums(SppList[, -1], na.rm = TRUE) != 0)
  SppList[, -1] <-
    apply(SppList[, -1], c(1, 2), function(x) {
      ifelse(is.na(x), 0, x)
    })
  return(list(SppList[NoZero, ], SppSource[NoZero, ]))
}

## Script to convert from old to new expert opinions
## ExpertScore is from ./ExpertEstimate.csv
ConvertExpert <- function(Code, x, Update = ExpertScore) {
  New <- Update[which(Update$TAXON_ID == Code), ]
  if (is.na(x) == TRUE) {
    y <- NA
  } else{
    if (x < 0.3) {
      y1 <- New$None
      y2 <- New$Few
      x1 <- 0
      x2 <- 0.3
    } else{
      if (x < 0.67) {
        y1 <- New$Few
        y2 <- New$Some
        x1 <- 0.3
        x2 <- 0.67
      } else{
        y1 <- New$Some
        y2 <- New$Lots
        x1 <- 0.67
        x2 <- 1
      }
    }
    m <- (y2 - y1) / (x2 - x1)
    b <- y1 - m * x1
    y <- m * x + b
  }
  return(y)
}

# Matt Chick's functions

## Handy helper function, constructing a n-length vector c(k, ..., k)
const_vector <- function(n, k) {
  return(sapply(seq_len(n), function (x)
    return(k)))
}

## The original objective.
geomean.fun <- function(x, spp) {
  logsum <-
    sum(log(apply(spp, 1, function (row) {
      sum(x * row, na.rm = TRUE)
    })))
  exp(logsum / (dim(spp)[1]))
}

## The reformulated objective. nloptr expects a minimization problem.
## The objective, constraints (and corresponding gradients) all take the
## same set of extra arguments.
geomean.obj <- function (x, spp) {
  logsum <-
    sum(log(apply(spp, 1, function (row) {
      sum(x * row, na.rm = TRUE)
    })))
  return(-logsum)
}

## The gradient of geomean.obj.
## Returns a vector containing the partial derivatives wrt each var.
geomean.grad <- function(x, spp) {
  ## The contribution of each row to the gradient.
  ## The derivative of log(sum(X*R)) wrt x is R[x]/(sum(X*R)).
  contrib <-
    apply(spp, 1, function(row) {
      -row / (sum(x * row, na.rm = TRUE))
    })
  ## Sum the contribution vectors to get the gradient
  grad <- apply(contrib, 1, function(dWt) {
    sum(dWt, na.rm = TRUE)
  })
  return(grad)
}

# Randon proportions function (used for generating starting values)
props <- function(ncol, nrow, var.names = NULL) {
  if (ncol < 2)
    stop("ncol must be greater than 1")
  p <- function(n) {
    ## I _think_ this should be near uniform
    z <- sample(seq(0, 1, by = .01), n, replace = TRUE)
    ## If we're very unlucky, sample may return all zeros.
    if (sum(z) == 0)
      z <- const_vector(length(x), 1)
    return(z / sum(z))
  }
  DM <- data.matrix(t(replicate(nrow, p(n = ncol))))
  return(DM)
}

CIM <- function(x) {
  c(mean(x), quantile(x, probs = c(0.025, 0.975)))
}

SppRes <- function(OptDat,
                   OptRes,
                   Scen = GSOScen,
                   TArea = GSOArea) {
  res <-
    data.frame(EFG_GS = names(OptRes[-length(OptRes)]), Optimal = as.vector(unclass(t(OptRes[1, -length(OptRes)]))))
  res2 <-
    left_join(res, Scen) %>% left_join(TArea[, -2]) %>% mutate(Land = PercLandscape *
                                                                 Area) %>%
    dplyr::select(EFG_GS, Area, Scenario, Land) %>% spread(Scenario, Land)
  res <-
    left_join(res, res2, by = 'EFG_GS') %>% mutate(Optimisation = Optimal *
                                                    Area)
  return(data.frame(TAXON_ID = OptDat$Species, as.matrix(OptDat[, -1]) %*% as.matrix(res[, -1:-3])))
}
#### Model to use ####
## Inequality constraints.
## Since the objective is monotone, we can replace the proportion
## equality constraint with <=.
## Per-species limits should be do-able in the same form.

eval_cs <- function(x, spp) {
  n <- length(StageNames)
  Res <- rep(NA, length(x) / n)
  for (i in 1:length(Res)) {
    Res[i] <- sum(x[1:n + (i - 1) * n]) - 1
  }
  return(Res)
}

## Jacobian (partial derivative matrix) for the inequalities
## Since each constraint is linear, the Jacobian is just the
## coefficients of each varible in eval_cs.
eval_cs_jac <- function(x, spp) {
  n <- length(StageNames)
  return(matrix(
    rep(c(rep(1, n), rep(0, length(
      x
    ))), length(x) / n),
    ncol = length(x),
    nrow = length(x) / n,
    byrow = TRUE
  ))
}

gso <- function(spp) {
  x0 <-
    const_vector(ncol(spp), 1 / ncol(spp)) ## *** Corresponds to the number of columns in the input ***
  run <-
    nloptr(
      x0 = x0,
      eval_f = geomean.obj,
      eval_grad_f = geomean.grad,
      lb = const_vector(length(x0), 0),
      ub = const_vector(length(x0), 1),
      eval_g_ineq = eval_cs,
      eval_jac_g_ineq = eval_cs_jac,
      opts = list(
        ## Algorithm to use.
        "algorithm" = "NLOPT_LD_MMA",
        ## Termination conditions
        "maxeval" = 1000,
        "ftol_rel" = 1.0e-15,
        "xtol_rel" = 1.0e-8,
        ## Suppress output during optimization
        "print_level" = 0
      ),
      spp = spp
    )
  ## Find the objective value corresponding to the optimal solution
  res <- rbind(c(run$solution, geomean.fun(run$solution, spp)))
  colnames(res) <- c(names(spp), "geom")
  res <- data.frame(res)
  return (res)
}


## Function to run repeated samples and calculate 95% CIs
OptRunCI <-
  function(data,
           efgs,
           Scen = GSOScen ,
           area = GSOArea,
           rule = 'Rule0',
           FiTy = 'High',
           Weight = 0.5,
           NRep = 20,
           N = 10,
           Comp = Comparison) {
    OptData <-
      DataGen(
        data,
        efgs = efgs,
        rule = rule,
        FT = FiTy,
        Wt = Weight,
        Rand = TRUE,
        NRep = NRep
      )[[1]]
    resultn <- gso(spp = OptData[, -1])
    resultS <- Scenarios(OptData, Scen = Scen, efg = efgs)
    resultS2 <- as.data.frame(t(c(resultS$GMA, resultn$geom)))
    names(resultS2) <-
      #c(levels(resultS$Scenario)[resultS$Scenario], 'Optimisation')
      c(resultS$Scenario, 'Optimisation')
    SpecRes <- SppRes(OptData, resultn, Scen = Scen, TArea = area)
    SpList <- array(NA, dim = c(nrow(SpecRes), ncol(SpecRes), N))
    SpList[, , 1] <- as.matrix(SpecRes)
    for (i in 2:N) {
      OptData <-
        DataGen(
          data,
          efgs = efgs,
          rule = rule,
          FT = FiTy,
          Wt = Weight,
          Rand = TRUE,
          NRep = NRep
        )[[1]]
      resultn[i, ] <- gso(spp = OptData[, -1])
      resultS <- Scenarios(OptData, Scen = Scen, efg = efgs)
      resultS2[i, ] <- t(c(resultS$GMA, resultn$geom[i]))
      SpList[, , i] <-
        as.matrix(SppRes(OptData, resultn, Scen = Scen, TArea = area))
    }
    res <- as.data.frame(t(apply(resultn, 2, CIM)))
    row.names(res)[nrow(res)] <- c('Optimisation')
    Scens <-
      as.data.frame(t(apply(resultS2 / resultS2[, which(names(resultS2) == Comp)], 2, CIM)))
    Scens0 <- as.data.frame(t(apply(resultS2, 2, CIM)))
    names(res) <- c('Prop', 'LB', 'UB')
    names(Scens) <- c('Prop', 'LB', 'UB')
    resultn2 <-
      data.frame(Scenario = rule, EFG_GS = colnames(resultn), res)[-length(resultn), ]
    resultn2$EFG <- as.numeric(str_sub(resultn2$EFG_GS, 4, 5))
    resultn2$GS <- StageNames
    resultsSp <- data.frame(apply(SpList, c(1, 2), mean))
    colnames(resultsSp) <- names(SpecRes)
    resultsSp <- left_join(resultsSp, FaunaCodes[,c("COMMON_NAME","SCIENTIFIC_NAME","DIVNAME","TAXON_ID")])
    return(list(resultn2, res[nrow(res), ], Scens, Scens0, resultsSp))
    # return(resultn)
  }
