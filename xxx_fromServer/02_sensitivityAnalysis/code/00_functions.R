# 1. generic functions ----

## 1.1. logit-link ----

logit <- function(x){
  
  log(x / (1 - x))
  
}

expit <- function(x){
  
  exp(x) / (1 + exp(x))
  
}

## 1.2. adjacency matrix ----

get.amat <- function (geo, names){
  nb.r <- spdep::poly2nb(geo, queen = TRUE, row.names = names)
  mat <- spdep::nb2mat(nb.r, style = 'B', zero.policy = TRUE)
  regions <- colnames(mat) <- rownames(mat)
  mat <- as.matrix(mat[1:dim(mat)[1], 1:dim(mat)[1]])
  return(mat)
}

## 1.3. summarise vector  ----

# summary (mean, variance, quantiles) of vector samples
my.summary <- function(x, CI = 0.95, na.rm = FALSE) {
  
  # x = theta1[,1]; CI = 0.95; 
  
  # lower and upper CI calculators
  lowerCI <- (1 - CI)/2
  upperCI <- 1 - lowerCI
  
  qntl <- quantile(x, 
                   probs = c(lowerCI, 0.5, upperCI),
                   na.rm = na.rm)
  data.frame(mean = mean(x, na.rm = na.rm), 
             variance = var(x, na.rm = na.rm), 
             lower = qntl[1], 
             median = qntl[2], 
             upper = qntl[3])
  
}

# 2. model fitting ----

## 2.1 precision matrices ----

precision.icar <- function(amat){
  
  # structure
  Q <- amat
  
  # organise amat into ICAR
  if(sum(Q > 0 & Q < 1) != 0){
    ## if 0 < Q_{ij} < 1 then make Q_{ij} = 1
    for(i in 1:nrow(Q)) {
      idx <- which(Q[i,] > 0 & Q[i,] < 1)
      Q[i,idx] <- 1
    }
  }
  
  # turn amat into an ICAR preciios
  ## no 0s on diagional
  ## main diagonal to be number of neighbours in total
  ## off diagonal to be 0 for not neighbours and -1 for neighbours
  diag(Q) <- 0
  diag <- apply(Q, 1, sum)
  Q[Q != 0] <- -1
  diag(Q) <- diag
  
  # scale and set constraints
  
  ## inla functions
  inla.scale.model.bym <- utils::getFromNamespace('inla.scale.model.bym', 'INLA')
  inla.bym.constr.internal <- utils::getFromNamespace('inla.bym.constr.internal', 'INLA')
  
  ## scale
  Q <- inla.scale.model.bym(Q = Q, adjust.for.con.comp = TRUE)
  
  ## constraints
  Q.constr <- inla.bym.constr.internal(Q = Q, adjust.for.con.comp = TRUE)
  
  return(Q)
  
}

precision.st.interaction <- function(type.st = c(2:4)[1], space, time, type.time = c('rw1', 'rw2')[1]){
  
  # type.st <- 4
  # space <- msoa.mat
  # time <- 2002:2020
  # type.time <- 'rw1'
  
  if(!is.matrix(space)) stop('Warning: space needs to be an adjacency matrix of neighbourhood structure')
  if(!is.vector(time)) stop('Warning: time needs to be a vector of the temporal component')
  
  # order of random walk
  if(type.time == 'rw1') { 
    order.rw <- 1 
  } else { 
    order.rw <- 2 
  }
  inla.rw <- utils::getFromNamespace('inla.rw', 'INLA')
  
  # full rank for dim check
  full.rank <- (nrow(space)) * (length(time))
  
  # type of interaction for space-time
  if (type.st == 2){
    # structured time x unstructured space
    Q.space <- diag(nrow(space))
    Q.time <- inla.rw(n = length(time), order = order.rw, scale.model = TRUE, sparse = TRUE)
    # constraint
    A1 <- kronecker(matrix(data = 1, nrow = 1, ncol = length(time)), Matrix::Diagonal(nrow(space)))
    A.constr <- A1[-1,] %>% as.matrix()
    # rank
    rank <- (nrow(space)) * (length(time) - order.rw)
    rank.def <- full.rank - rank
  } else if (type.st == 3){
    # unstructured time x structured space
    Q.space <- precision.icar(space)
    Q.time <- diag(length(time))
    # constraint
    A1 <- kronecker(Matrix::Diagonal(length(time)), matrix(data = 1, nrow = 1, ncol = nrow(space)))
    A.constr <- A1[-1,] %>% as.matrix()
    # rank
    rank <- (nrow(space) - 1) * (length(time))
    rank.def <- full.rank - rank
  } else {
    # structured time x structured space
    Q.space <- precision.icar(space)
    Q.time <- inla.rw(n = length(time), order = order.rw, scale.model = TRUE, sparse = TRUE)
    # constraint
    A1 <- kronecker(matrix(data = 1, nrow = 1, ncol = length(time)), Matrix::Diagonal(nrow(space)))
    A2 <- kronecker(Matrix::Diagonal(length(time)), matrix(data = 1, nrow = 1, ncol = nrow(space)))
    A.constr <- rbind(A1[-1,], A2[-1,]) %>% as.matrix()
    # rank
    rank <- (nrow(space) - 1) * (length(time) - order.rw)
    rank.def <- full.rank - rank
  }
  
  # order
  ## believe time order is more important than space order
  Q.space.time <- kronecker(Q.time, Q.space) %>% as.matrix()
  
  return(list(Q = Q.space.time, A.constr = A.constr, rank.def = rank.def))
  
}

## 2.2 model fit ----

### 2.2.1. new ----

model.fit <- 
  function(data, 
           formula = NULL, 
           exposure.names = NULL,
           hurdle.model = FALSE,
           family.zero = 'binomial',
           family.count = c('poisson', 'zeroinflatedpoisson0', 'zeroinflatedpoisson1', 'zeroinflatednbinomial0', 'nzpoisson')[3],
           inla.mode = c('classic', 'twostage', 'experimental')[3],
           verbose = FALSE,
           control.compute = list(config = TRUE),
           control.predictor = list(compute = TRUE),
           control.mode = list(restart = TRUE),
           control.inla = list(strategy = 'auto', int.strategy = 'auto'),
           pc.u = 1, pc.alpha = 0.01,
           pu.u.phi = 0.5, pc.alpha.phi = 2/3,
           include.exposures = TRUE,
           shared.exposures = TRUE,
           shared.spatial = TRUE,
           model.spatial = c('-', 'iid', 'bym2')[3],
           aMat = NULL,
           shared.temporal = TRUE,
           model.temporal = c('-', 'iid', 'rw1', 'rw2')[3],
           shared.spatio.temporal = TRUE,
           model.spatial.temporal = c('-', 1:4)[2],
           ...){
    
    # 0. model arguments ----
    
    # data <- data.model
    # formula <- NULL
    # exposure.names <- 'pm25'
    # hurdle.model <- TRUE
    # family.zero <- 'binomial'
    # family.count <- c('poisson', 'zeroinflatedpoisson1', 'nzpoisson')[3]
    # inla.mode <- c('classic', 'twostage', 'experimental')[3]
    # verbose <- FALSE
    # control.compute <- list(config = TRUE, dic = TRUE, waic = TRUE, cpo = TRUE)
    # control.predictor <- list(compute = TRUE)
    # control.mode <- list(restart = TRUE)
    # control.inla <- list(strategy = 'auto', int.strategy = 'auto')
    # pc.u <- 1
    # pc.alpha <- 0.01
    # pu.u.phi <- 0.5
    # pc.alpha.phi <- 2/3
    # include.exposures <- TRUE
    # shared.exposures <- FALSE
    # shared.spatial <- TRUE
    # model.spatial <- c('-', 'iid', 'bym2')[1]
    # aMat <- mat.msoa
    # shared.temporal <- TRUE
    # model.temporal <- c('-', 'iid', 'rw1', 'rw2')[1]
    # shared.spatio.temporal <- TRUE
    # model.spatial.temporal <- c('-', 1:4)[1]
    
    # 1. check model arguments ----
    
    if(!is.null(aMat)){
      if(is.null(rownames(aMat))){
        stop('Row names of a mat need to be specified to region names.')
      }
      if(is.null(colnames(aMat))){
        stop('Column names of a mat need to be specified to region names.')
      }
      if(sum(rownames(aMat) != colnames(aMat)) > 0){
        stop('Row and column names need to be the same.')
      }
      is.spatial <- TRUE
    } else {
      aMat <- matrix(data = 1, nrow = 1, ncol = 1)
      colnames(aMat) <- rownames(aMat) <- 'All'
      is.spatial <- FALSE
    }
    
    # 2. model fitting preperation ----
    
    ## 2.1. likelihood distribution ----
    
    # labels for message
    family.zero.label <- 
      switch(EXPR = family.zero,
             binomial = 'Binomial',
             stop(family.zero,  ' is not availiable for zero data distribution yet.'))
    
    family.count.label <-
      switch(EXPR = family.count,
             poisson = 'Poisson',
             zeroinflatedpoisson0 = 'Zero Inflated Poisson: Type 0',
             zeroinflatedpoisson1 = 'Zero Inflated Poisson: Type 1',
             zeroinflatednbinomial0 = 'Zero Inflated Negative Binomial: Type 0',
             nzpoisson = 'Non-Zero Poisson',
             stop(family.count,  ' is not availiable for count data distribution yet.'))
    
    ## 2.2. hyper priors ----
    
    # generic hyper priors
    hyper.pc <- list(prec = list(prior = 'pc.prec', param = c(pc.u, pc.alpha)))
    
    ## 2.3. number of unique terms ----
    
    # nuumber of unique spatial and temporal elements
    n.spatial <- nrow(aMat)
    n.temporal <- length(unique(data$YEAR)) 
    
    # 3. model fitting ----
    
    if(hurdle.model){
      
      ## 3.1 Hurdle model ----
      
      if(!(family.count %in% c('zeroinflatedpoisson0', 'zeroinflatednbinomial0', 'nzpoisson'))) {
        stop(family.count, ' needs to be a zero truncated distribution.')
      }
      
      message('--------------------------------',
              '\nHurdle Model Selected',
              appendLF = FALSE)
      
      start.1 <- proc.time()
      
      ### 3.1.1. model formula ----
      
      if(is.null(formula)) {
        
        message('\n Model Specification',
                '\n  Exposures:',  
                paste0('\n   Include: ', include.exposures),
                paste0('\n   Shared: ', shared.exposures),
                appendLF = FALSE)
        
        # base formula to build on
        formula <- Y ~ - 1
        
        # for fitting without exposures
        # but keep all outputs same
        if(include.exposures){
          
          if(is.null(exposure.names)) {stop('Need to include exposure names')}
          
          if(shared.exposures){
            
            # short formula
            exposures.formula.short <- 
              paste0(paste0(exposure.names, '_id'), 
                     collapse = ' + ')
            
            # exposure model matrix
            exposure.matrix <- 
              paste0('~ ', exposures.formula.short) %>% 
              as.formula() %>% 
              stats::model.matrix(., data = data)
            
            # needed for stack later
            exposure.matrix.z <-
              exposure.matrix.y <-
              exposure.matrix
            
            # long formula
            exposures.formula.long <- 
              paste0(' ~ . + intercept + ',
                     exposure.matrix.z %>% as.data.frame() %>% select(-'(Intercept)') %>% colnames() %>% 
                       paste0(., collapse = ' + '))
            
          } else {
            
            # data with z. and y. infront of column names
            ## zero
            data.z <- 
              data %>%
              dplyr::select(paste0(exposure.names, '_id')) %>%
              setNames(paste0('z.', names(.)))
            ## count
            data.y <- 
              data %>%
              dplyr::select(paste0(exposure.names, '_id')) %>%
              setNames(paste0('y.', names(.)))
            
            # short formula
            ## zero
            exposures.formula.short.z <- 
              paste0(paste0('z.', paste0(exposure.names, '_id')), collapse = ' + ')
            ## count
            exposures.formula.short.y <- 
              paste0(paste0('y.', paste0(exposure.names, '_id')), collapse = ' + ')
            
            # exposure model matrix
            ## zero
            exposure.matrix.z <- 
              paste0('~ ', exposures.formula.short.z) %>% 
              as.formula() %>% 
              stats::model.matrix(., data = data.z)
            ## count
            exposure.matrix.y <- 
              paste0('~ ', exposures.formula.short.y) %>% 
              as.formula() %>% 
              stats::model.matrix(., data = data.y)
            
            # long formula
            ## zero
            exposures.formula.long.z <- 
              paste0('z.intercept + ',
                     exposure.matrix.z %>% as.data.frame() %>% select(-'(Intercept)') %>% colnames() %>% 
                       paste0(., collapse = ' + '))
            ## count
            exposures.formula.long.y <- 
              paste0('y.intercept + ',
                     exposure.matrix.y %>% as.data.frame() %>% select(-'(Intercept)') %>% colnames() %>% 
                       paste0(., collapse = ' + '))
            ## final
            exposures.formula.long <-
              paste0(' ~ . + ', paste0(c(exposures.formula.long.z, exposures.formula.long.y), collapse = ' + '))
            
          }
          
        } else {
          
          if(shared.exposures){
            
            # exposure model matrix
            exposure.matrix <- 
              paste0('~ ', 1) %>% 
              as.formula() %>% 
              stats::model.matrix(., data = data)
            
            # needed for stack later
            exposure.matrix.z <-
              exposure.matrix.y <-
              exposure.matrix
            
            # long formula
            exposures.formula.long <- ' ~ . + intercept'
            
          } else {
            
            # data with z. and y. infront of column names
            ## zero
            data.z <- 
              data %>%
              dplyr::select(paste0(exposure.names, '_id')) %>%
              setNames(paste0('z.', names(.)))
            ## count
            data.y <- 
              data %>%
              dplyr::select(paste0(exposure.names, '_id')) %>%
              setNames(paste0('y.', names(.)))
            
            # exposure model matrix
            ## zero
            exposure.matrix.z <- 
              paste0('~ ', 1) %>% 
              as.formula() %>% 
              stats::model.matrix(., data = data.z)
            ## count
            exposure.matrix.y <- 
              paste0('~ ', 1) %>% 
              as.formula() %>% 
              stats::model.matrix(., data = data.y)
            
            # long formula
            ## zero
            exposures.formula.long.z <- 'z.intercept'
            ## count
            exposures.formula.long.y <- 'y.intercept'
            ## final
            exposures.formula.long <-
              paste0(' ~ . + ', paste0(c(exposures.formula.long.z, exposures.formula.long.y), collapse = ' + '))
            
          }
          
        }
        
        formula <-
          update(formula, exposures.formula.long)
        
        
        # add spatial random effect
        message('\n  Spatial random effect',
                paste0('\n   Shared: ', shared.spatial),
                appendLF = FALSE)
        if(model.spatial == '-'){
          message('\n   Model: None',
                  appendLF = FALSE)
        } else if (model.spatial == 'iid'){
          message(paste0('\n   Model: ', toupper(model.spatial)),
                  appendLF = FALSE)
          if(shared.spatial){
            # formula update
            formula <-
              update(formula, ~.  + 
                       f(z.space_id, 
                         model = model.spatial, 
                         constr = TRUE,
                         hyper = list(prec = list(prior = 'pc.prec', param = c(pc.u, pc.alpha))),
                         values = 1:n.spatial) +
                       f(y.space_id, 
                         copy = 'z.space_id', 
                         hyper = list(beta = list(fixed = FALSE))))
          } else {
            # formula update
            formula <-
              update(formula, ~.  + 
                       f(z.space_id, 
                         model = model.spatial, 
                         constr = TRUE,
                         hyper = list(prec = list(prior = 'pc.prec', param = c(pc.u, pc.alpha))),
                         values = 1:n.spatial) +
                       f(y.space_id, 
                         model = model.spatial, 
                         constr = TRUE,
                         hyper = list(prec = list(prior = 'pc.prec', param = c(pc.u, pc.alpha))),
                         values = 1:n.spatial))
          }
        } else if (model.spatial == 'bym2') {
          message(paste0('\n   Model: ', toupper(model.spatial)),
                  appendLF = FALSE)
          if(shared.spatial){
            # formula update
            formula <-
              update(formula, ~.  + 
                       f(z.space_id,
                         model = model.spatial, 
                         graph = aMat, 
                         constr = TRUE,
                         scale.model = TRUE,
                         hyper = list(prec = list(prior = 'pc.prec', param = c(pc.u, pc.alpha)),
                                      phi = list(prior = 'pc', param = c(pu.u.phi, pc.alpha.phi))),
                         values = 1:n.spatial) +
                       f(y.space_id, 
                         copy = 'z.space_id',
                         hyper = list(beta = list(fixed = FALSE))))
          } else {
            # formula update
            formula <-
              update(formula, ~.  + 
                       f(z.space_id, 
                         model = model.spatial, 
                         graph = aMat, 
                         constr = TRUE,
                         scale.model = TRUE,
                         hyper = list(prec = list(prior = 'pc.prec', param = c(pc.u, pc.alpha)),
                                      phi = list(prior = 'pc', param = c(pu.u.phi, pc.alpha.phi))),
                         values = 1:n.spatial) +
                       f(y.space_id, 
                         model = model.spatial, 
                         graph = aMat, 
                         constr = TRUE,
                         scale.model = TRUE,
                         hyper = list(prec = list(prior = 'pc.prec', param = c(pc.u, pc.alpha)),
                                      phi = list(prior = 'pc', param = c(pu.u.phi, pc.alpha.phi))),
                         values = 1:n.spatial))
          }
        } else {
          stop('Spatial model can be IID or BYM2.')
        }
        
        # add temporal random effect
        message('\n  Temporal random effect',
                paste0('\n   Shared: ', shared.temporal),
                appendLF = FALSE)
        if(model.temporal == '-'){
          message('\n   Model: None',
                  appendLF = FALSE)
        } else if (model.temporal %in% c('iid', 'rw1', 'rw2')) {
          message(paste0('\n   Model: ', toupper(model.temporal)),
                  appendLF = FALSE)
          if(shared.temporal){
            # formula update
            formula <-
              update(formula, ~ . + 
                       f(z.year_id, 
                         model = model.temporal, 
                         constr = TRUE,
                         hyper = list(prec = list(prior = 'pc.prec', param = c(pc.u, pc.alpha))),
                         values = 1:n.temporal) +
                       f(y.year_id,
                         copy = 'z.year_id',
                         hyper = list(beta = list(fixed = FALSE))))
          } else {
            # formula update
            formula <-
              update(formula, ~ . + 
                       f(z.year_id, 
                         model = model.temporal, 
                         constr = TRUE,
                         hyper = list(prec = list(prior = 'pc.prec', param = c(pc.u, pc.alpha))),
                         values = 1:n.temporal) +
                       f(y.year_id, 
                         constr = TRUE,
                         model = model.temporal, 
                         hyper = list(prec = list(prior = 'pc.prec', param = c(pc.u, pc.alpha))),
                         values = 1:n.temporal))
            
          }
        } else {
          stop('Temporal model can be IID, RW1 or RW2.')
        }
        
        # add spatio-temporal random effect
        message('\n  Spatial-temporal random effect',
                paste0('\n   Shared: ', shared.spatio.temporal),
                appendLF = FALSE)
        if(model.spatial.temporal == '-'){
          message('\n   Model: None',
                  appendLF = FALSE)
        } else if (model.spatial.temporal == '1'){
          message(paste0('\n   Model: Type ', as.roman(model.spatial.temporal)),
                  appendLF = FALSE)
          if(shared.spatio.temporal){
            formula <-
              update(formula, ~ . + 
                       f(z.spaceYear_id, 
                         model = 'iid', 
                         constr = TRUE,
                         hyper = list(prec = list(prior = 'pc.prec', param = c(pc.u, pc.alpha))),
                         values = 1:(n.spatial*n.temporal)) +
                       f(y.spaceYear_id,
                         copy = 'z.spaceYear_id',
                         hyper = list(beta = list(fixed = FALSE))))
          } else {
            formula <-
              update(formula, ~ . + 
                       f(z.spaceYear_id, 
                         model = 'iid', 
                         constr = TRUE,
                         hyper = list(prec = list(prior = 'pc.prec', param = c(pc.u, pc.alpha))),
                         values = 1:(n.spatial*n.temporal)) +
                       f(y.spaceYear_id,
                         model = 'iid', 
                         constr = TRUE,
                         hyper = list(prec = list(prior = 'pc.prec', param = c(pc.u, pc.alpha))),
                         values = 1:(n.spatial*n.temporal)))
          }
        } else if (model.spatial.temporal %in% c(2:4)) {
          message(paste0('\n   Model: Type ', as.roman(model.spatial.temporal)),
                  appendLF = FALSE)
          # uses a rw1 temporal component by default
          st.components <- precision.st.interaction(type.st = model.spatial.temporal, space = aMat, time = 1:n.temporal)
          if(shared.spatio.temporal){
            formula <-
              update(formula, ~ . + 
                       f(z.spaceYear_id, 
                         model = 'generic0',
                         Cmatrix = st.components$Q,
                         rankdef = st.components$rank.def,
                         constr = TRUE,
                         extraconstr = list(A = st.components$A.constr, e = rep(o, nrow(st.components$A.constr))),
                         hyper = list(prec = list(prior = 'pc.prec', param = c(pc.u, pc.alpha))),
                         values = 1:(n.spatial*n.temporal)) +
                       f(y.spaceYear_id,
                         copy = 'z.spaceYear_id',
                         hyper = list(beta = list(fixed = FALSE))))
          } else {
            formula <-
              update(formula, ~ . + 
                       f(z.spaceYear_id, 
                         model = 'generic0',
                         Cmatrix = st.components$Q,
                         rankdef = st.components$rank.def,
                         constr = TRUE,
                         extraconstr = list(A = st.components$A.constr, e = rep(o, nrow(st.components$A.constr))),
                         hyper = list(prec = list(prior = 'pc.prec', param = c(pc.u, pc.alpha))),
                         values = 1:(n.spatial*n.temporal)) +
                       f(y.spaceYear_id, 
                         model = 'generic0',
                         Cmatrix = st.components$Q,
                         rankdef = st.components$rank.def,
                         constr = TRUE,
                         extraconstr = list(A = st.components$A.constr, e = rep(o, nrow(st.components$A.constr))),
                         hyper = list(prec = list(prior = 'pc.prec', param = c(pc.u, pc.alpha))),
                         values = 1:(n.spatial*n.temporal)))
          }
        } else {
          stop('Spatio-temporal model can be Type I - IV.')
        }
        
      }
      
      end.1 <- proc.time() - start.1
      
      ### 3.1.2 data organising ----
      
      # identify non-zero counts
      non.zero.position <- data$Y > 0
      
      # data for stack
      ## zero
      Y.est.z <- cbind(dplyr::if_else(data$Y == 0, 0, 1), NA)
      ## count 
      Y.est.y <- cbind(NA, data$Y[non.zero.position])
      
      # Adjancy matrix
      A.full <- matrix(1, nrow = nrow(data), ncol = 1)
      A.non.zero <- matrix(1, nrow = nrow(data[non.zero.position,]), ncol = 1)
      
      # effects for stack
      ## exposures
      ### zero
      exposure.matrix.stk.z <- 
        exposure.matrix.z %>% as.data.frame() %>% dplyr::select(-'(Intercept)') %>% as.matrix()
      ### count
      exposure.matrix.stk.y <- 
        # non zero only
        exposure.matrix.y[non.zero.position,] %>% as.data.frame() %>% dplyr::select(-'(Intercept)') %>% as.matrix()
      ## random effects
      ### zero
      random.effects.stk.z <-
        data %>% 
        dplyr::select('space_id', 'year_id', 'spaceYear_id') %>% 
        setNames(paste0('z.', names(.)))
      ### count
      random.effects.stk.y <-
        # non zero only
        data[non.zero.position,] %>% 
        dplyr::select('space_id', 'year_id', 'spaceYear_id') %>% 
        setNames(paste0('y.', names(.)))
      
      # define stacks
      ## zero
      stk.est.z <-
        INLA::inla.stack(data = list(Y = Y.est.z,
                                     link = 1),
                         A = list(A = A.full, 
                                  1),
                         effects = list(c(list(z.intercept = 1),
                                          list(intercept = 1)),
                                        data.frame(exposure.matrix.stk.z,
                                                   random.effects.stk.z)),
                         tag = 'z.est')
      ## count
      stk.est.y <-
        INLA::inla.stack(data = list(Y = Y.est.y,
                                     E = data$E[non.zero.position],
                                     link = 2),
                         A = list(A = A.non.zero,
                                  1),
                         effects = list(c(list(y.intercept = 1),
                                          list(intercept = 1)),
                                        data.frame(exposure.matrix.stk.y,
                                                   random.effects.stk.y)),
                         tag = 'y.est')
      
      # final inla data
      ## full stack
      stk.full <- INLA::inla.stack(stk.est.z, stk.est.y)
      ## new data
      data.new <- INLA::inla.stack.data(stk.full)
      
      ### 3.1.3 model fit ----
      
      start.2 <- proc.time()
      
      message('\n  Distribution',
              paste0('\n   Zero: ', family.zero.label),
              paste0('\n   Count: ', family.count.label),
              '\n Fitting model.... \n ',
              appendLF = FALSE)
      
      control.family <-
        switch(EXPR = family.count,
               nzpoisson = {
                 # empty list for both zero and count
                 list(list(), list())
               },
               zeroinflatedpoisson0 = {
                 # empty list for zero
                 # for count ensure proper truncation by setting theta to large negative value
                 # ensures logit^{-1}(theta) approached zero
                 list(list(),
                      list(hyper = list(theta = list(initial = -20, fixed = TRUE))))
               },
               zeroinflatednbinomial0 = {
                 # empty list for zero
                 # for count ensure proper truncation by setting theta to large negative value
                 # ensures logit^{-1}(theta) approached zero
                 list(list(),
                      list(hyper = list(theta = list(initial = -20, fixed = TRUE))))
               },
               stop(family.count,  ' must be a truncated distribution.') )
      
      fit <-
        INLA::inla(formula = formula,
                   family = c(family.zero, family.count),
                   data = data.new,
                   E = E,
                   control.compute = control.compute,
                   control.predictor = list(A = INLA::inla.stack.A(stk.full), link = data.new$link, compute = TRUE),
                   control.family = control.family,
                   control.inla = control.inla,
                   control.mode = control.mode,
                   inla.mode = inla.mode,
                   num.threads = round(parallel::detectCores()*0.8),
                   verbose = verbose,
                   ...)
      
      # for output
      shared.components <- list(exposures = shared.exposures, spatial = shared.spatial, temporal = shared.temporal, spatio.temporal = shared.spatio.temporal)
      
      end.2 <- proc.time() - start.2
      
    } else {
      
      ## 3.2. Non-hurdle model ----
      
      message('--------------------------------',
              '\nNon Hurdle Model Selected',
              appendLF = FALSE)
      
      start.1 <- proc.time()
      
      ### 3.2.1. model formula ----
      
      if(is.null(formula)) {
        
        message('\n Model Specification',
                '\n  Exposures:',  
                paste0('\n   Include: ', include.exposures),
                appendLF = FALSE)
        
        formula <-
          # final formula
          Y ~ - 1
        
        if(include.exposures){
          
          if(is.null(exposure.names)) {stop('Need to include exposure names')}
          
          # short formula
          exposures.formula.short <- 
            paste0(paste0(exposure.names, '_id'), collapse = ' + ')
          
          # exposure model matrix
          exposure.matrix <- 
            paste0('~ ', exposures.formula.short) %>% 
            as.formula() %>% 
            stats::model.matrix(., data = data)
          
          # long formula
          exposures.formula.long <- 
            paste0(' ~ . + intercept + ',
                   exposure.matrix %>% as.data.frame() %>% select(-'(Intercept)') %>% colnames() %>% 
                     paste0(., collapse = ' + '))
          
        } else {
          
          # exposure model matrix
          exposure.matrix <- 
            paste0('~ ', 1) %>% 
            as.formula() %>% 
            stats::model.matrix(., data = data)
          
          # long formula
          exposures.formula.long <- ' ~ . + intercept'
          
        }
        
        formula <-
          update(formula, exposures.formula.long)
        
        # add spatial random effect
        message('\n  Spatial random effect',
                appendLF = FALSE)
        if(model.spatial == '-'){
          message('\n   Model: None',
                  appendLF = FALSE)
        } else if (model.spatial == 'iid'){
          message(paste0('\n   Model: ', toupper(model.spatial)),
                  appendLF = FALSE)
          # formula update
          formula <-
            update(formula, ~.  + 
                     f(space_id, 
                       model = model.spatial, 
                       constr = TRUE,
                       hyper = list(prec = list(prior = 'pc.prec', param = c(pc.u, pc.alpha))),
                       values = 1:n.spatial))
        } else if (model.spatial == 'bym2') {
          message(paste0('\n   Model: ', toupper(model.spatial)),
                  appendLF = FALSE)
          formula <-
            update(formula, ~.  + 
                     f(space_id, 
                       model = model.spatial, 
                       graph = aMat, 
                       constr = TRUE,
                       scale.model = TRUE,
                       hyper = list(prec = list(prior = 'pc.prec', param = c(pc.u, pc.alpha)),
                                    phi = list(prior = 'pc', param = c(pu.u.phi, pc.alpha.phi))),
                       values = 1:n.spatial))
        } else {
          stop('Spatial model can be IID or BYM2.')
        }
        
        # add temporal random effect
        message('\n  Temporal random effect',
                appendLF = FALSE)
        if(model.temporal == '-'){
          message('\n   Model: None',
                  appendLF = FALSE)
        } else if (model.temporal %in% c('iid', 'rw1', 'rw2')) {
          message(paste0('\n   Model: ', toupper(model.temporal)),
                  appendLF = FALSE)
          formula <-
            update(formula, ~ . + 
                     f(year_id, 
                       model = model.temporal, 
                       constr = TRUE,
                       hyper = list(prec = list(prior = 'pc.prec', param = c(pc.u, pc.alpha))),
                       values = 1:n.temporal))
        } else {
          stop('Temporal model can be IID, RW1 or RW2.')
        }
        
        # add spatio-temporal random effect
        message('\n  Spatial-temporal random effect',
                appendLF = FALSE)
        if(model.spatial.temporal == '-'){
          message('\n   Model: None',
                  appendLF = FALSE)
        } else if (model.spatial.temporal == '1'){
          message(paste0('\n   Model: Type ', as.roman(model.spatial.temporal)),
                  appendLF = FALSE)
          formula <-
            update(formula, ~ . + 
                     f(spaceYear_id, 
                       model = 'iid', 
                       constr = TRUE,
                       hyper = list(prec = list(prior = 'pc.prec', param = c(pc.u, pc.alpha))),
                       values = 1:(n.spatial*n.temporal)))
          
        } else if (model.spatial.temporal %in% c(2:4)) {
          message(paste0('\n   Model: Type ', as.roman(model.spatial.temporal)),
                  appendLF = FALSE)
          # uses a rw1 temporal component by default
          st.components <- precision.st.interaction(type.st = model.spatial.temporal, space = aMat, time = 1:n.temporal)
          formula <-
            update(formula, ~ . + 
                     f(z.spaceYear_id, 
                       model = 'generic0',
                       Cmatrix = st.components$Q,
                       rankdef = st.components$rank.def,
                       constr = TRUE,
                       extraconstr = list(A = st.components$A.constr, e = rep(o, nrow(st.components$A.constr))),
                       hyper = list(prec = list(prior = 'pc.prec', param = c(pc.u, pc.alpha))),
                       values = 1:(n.spatial*n.temporal)))
          
        } else {
          stop('Spatio-temporal model can be Type I - IV.')
        }
        
      }
      
      ### 3.2.2. data organisation ----
      
      # model matrix for exposures (fixed effects)
      exposure.matrix.stk <- 
        # model matrix with intercept column removed
        exposure.matrix %>% as.data.frame() %>% dplyr::select(-'(Intercept)') %>% as.matrix()
      
      # random effects for stack
      random.effects.stk <-
        data %>% 
        dplyr::select('space_id', 'year_id', 'spaceYear_id')
      
      # define stacks
      stk <-
        INLA::inla.stack(data = list(Y = data$Y,
                                     E = data$E, 
                                     link = 1),
                         A = list(A = matrix(1, nrow = nrow(data), ncol = 1),
                                  1),
                         effects = list(list(intercept = 1),
                                        data.frame(exposure.matrix.stk,
                                                   random.effects.stk)),
                         tag = 'stk')
      
      # final inla data
      ## full stack
      stk.full <- INLA::inla.stack(stk)
      ## new data
      data.new <- INLA::inla.stack.data(stk.full)
      
      end.1 <- proc.time() - start.1
      
      start.2 <- proc.time()
      
      ### 3.2.3. model fit ----
      
      message('\n  Distribution',
              paste0('\n   Count: ', family.count.label),
              '\n Fitting model.... \n ',
              appendLF = FALSE)
      
      fit <-
        INLA::inla(formula = formula,
                   family = family.count,
                   data = data.new,
                   E = E,
                   control.compute = control.compute,
                   control.predictor = list(A = INLA::inla.stack.A(stk.full), link = data.new$link, compute = TRUE),
                   control.inla = control.inla,
                   control.mode = control.mode,
                   inla.mode = inla.mode,
                   num.threads = round(parallel::detectCores()*0.8),
                   verbose = verbose,
                   ...)
      
      # for output
      shared.components <- NULL
      
      end.2 <- proc.time() - start.2
      
    }
    
    # 4. return ----
    
    message('\nTime take for model fitting',
            '\n Data sort (s): ', end.1[3] %>% as.numeric() %>% round(., digits = 2),
            '\n Model fit (s): ', end.2[3] %>% as.numeric() %>% round(., digits = 2),
            '\n Total (s): ', sum(end.1[3], end.2[3]) %>% as.numeric() %>% round(., digits = 2),
            '\n--------------------------------\n',
            appendLF = FALSE)
    
    return(list(newdata = data.new,
                stack = stk.full,
                formula = formula,
                fit = fit,
                aMat= aMat,
                family = c(family.zero, family.count),
                hyper.prior = list(pc.u = pc.u, pc.alpha = pc.alpha,
                                   pu.u.phi = pu.u.phi, pc.alpha.phi = pc.alpha.phi),
                prior.model = list(model.spatial = model.spatial, model.temporal = model.temporal),
                shared.components = shared.components))
  }

### 2.2.2. old ----

model.fit.old <- 
  function(data, 
           formula = NULL, 
           hurdle.model = FALSE,
           family.zero = 'binomial',
           family.count = c('poisson', 'zeroinflatedpoisson0', 'zeroinflatedpoisson1', 'zeroinflatednbinomial0', 'nzpoisson')[3],
           inla.mode = c('classic', 'twostage', 'experimental')[3],
           verbose = FALSE,
           control.compute = list(config = TRUE),
           control.predictor = list(compute = TRUE),
           control.mode = list(restart = TRUE),
           control.inla = list(strategy = 'auto', int.strategy = 'auto'),
           pc.u = 1, pc.alpha = 0.01,
           pu.u.phi = 0.5, pc.alpha.phi = 2/3,
           include.exposures = TRUE,
           shared.exposures = TRUE,
           shared.spatial = TRUE,
           model.spatial = c('-', 'iid', 'bym2')[3],
           aMat = NULL,
           shared.temporal = TRUE,
           model.temporal = c('-', 'iid', 'rw1', 'rw2')[3],
           shared.spatio.temporal = TRUE,
           model.spatial.temporal = c('-', 1:4)[2],
           ...){
    
    # 0. model arguments ----
    
    # data <- data.model
    # formula <- NULL
    # hurdle.model <- TRUE
    # family.zero <- 'binomial'
    # family.count <- c('poisson', 'zeroinflatedpoisson1', 'nzpoisson')[3]
    # inla.mode <- c('classic', 'twostage', 'experimental')[3]
    # verbose <- FALSE
    # control.compute <- list(config = TRUE, dic = TRUE, waic = TRUE, cpo = TRUE)
    # control.predictor <- list(compute = TRUE)
    # control.mode <- list(restart = TRUE)
    # control.inla <- list(strategy = 'auto', int.strategy = 'auto')
    # pc.u <- 1
    # pc.alpha <- 0.01
    # pu.u.phi <- 0.5
    # pc.alpha.phi <- 2/3
    # include.exposures <- TRUE
    # shared.exposures <- FALSE
    # shared.spatial <- TRUE
    # model.spatial <- c('-', 'iid', 'bym2')[3]
    # aMat <- msoa.mat
    # shared.temporal <- TRUE
    # model.temporal <- c('-', 'iid', 'rw1', 'rw2')[3]
    # shared.spatio.temporal <- TRUE
    # model.spatial.temporal <- c('-', 1:4)[1]
    
    # 1. check model arguments ----
    
    if(!is.null(aMat)){
      if(is.null(rownames(aMat))){
        stop('Row names of a mat need to be specified to region names.')
      }
      if(is.null(colnames(aMat))){
        stop('Column names of a mat need to be specified to region names.')
      }
      if(sum(rownames(aMat) != colnames(aMat)) > 0){
        stop('Row and column names need to be the same.')
      }
      is.spatial <- TRUE
    } else {
      aMat <- matrix(data = 1, nrow = 1, ncol = 1)
      colnames(aMat) <- rownames(aMat) <- 'All'
      is.spatial <- FALSE
    }
    
    # 2. model fitting preperation ----
    
    ## 2.1. likelihood distribution ----
    
    # labels for message
    family.zero.label <- 
      switch(EXPR = family.zero,
             binomial = 'Binomial',
             stop(family.zero,  ' is not availiable for zero data distribution yet.'))
    
    family.count.label <-
      switch(EXPR = family.count,
             poisson = 'Poisson',
             zeroinflatedpoisson0 = 'Zero Inflated Poisson: Type 0',
             zeroinflatedpoisson1 = 'Zero Inflated Poisson: Type 1',
             zeroinflatednbinomial0 = 'Zero Inflated Negative Binomial: Type 0',
             nzpoisson = 'Non-Zero Poisson',
             stop(family.count,  ' is not availiable for count data distribution yet.'))
    
    ## 2.2. hyper priors ----
    
    # generic hyper priors
    hyper.pc <- list(prec = list(prior = 'pc.prec', param = c(pc.u, pc.alpha)))
    
    ## 2.3. number of unique terms ----
    
    # nuumber of unique spatial and temporal elements
    n.spatial <- nrow(aMat)
    n.temporal <- length(unique(data$YEAR)) 
    
    # 3. model fitting ----
    
    if(hurdle.model){
      
      ## 3.1 Hurdle model ----
      
      if(!(family.count %in% c('zeroinflatedpoisson0', 'zeroinflatednbinomial0', 'nzpoisson'))) {
        stop(family.count, ' needs to be a zero truncated distribution.')
      }
      
      message('--------------------------------',
              '\nHurdle Model Selected',
              appendLF = FALSE)
      
      start.1 <- proc.time()
      
      ### 3.1.1. model formula ----
      
      if(is.null(formula)) {
        
        message('\n Model Specification',
                '\n  Exposures:',  
                paste0('\n   Include: ', include.exposures),
                paste0('\n   Shared: ', shared.exposures),
                appendLF = FALSE)
        
        # base formula to build on
        formula <- Y ~ - 1
        
        exposure.list <- c('deprivation', 'diversity', 'populationDensity', 'nighttimeLight', 'totalRail', 'totalRoad', 'ndvi', 'pm25', 'no2')
        
        # for fitting without exposures
        # but keep all outputs same
        if(include.exposures){
          
          if(shared.exposures){
            
            # short formula
            exposures.formula.short <- 
              paste0(paste0(exposure.list, '_id'), 
                     collapse = ' + ')
            
            # exposure model matrix
            exposure.matrix <- 
              paste0('~ ', exposures.formula.short) %>% 
              as.formula() %>% 
              stats::model.matrix(., data = data)
            
            # needed for stack later
            exposure.matrix.z <-
              exposure.matrix.y <-
              exposure.matrix
            
            # long formula
            exposures.formula.long <- 
              paste0(' ~ . + intercept + ',
                     colnames(exposure.matrix[,-1]) %>% 
                       paste0(., collapse = ' + '))
            
          } else {
            
            # data with z. and y. infront of column names
            ## zero
            data.z <- 
              data %>%
              dplyr::select(paste0(exposure.list, '_id')) %>%
              setNames(paste0('z.', names(.)))
            ## count
            data.y <- 
              data %>%
              dplyr::select(paste0(exposure.list, '_id')) %>%
              setNames(paste0('y.', names(.)))
            
            # short formula
            ## zero
            exposures.formula.short.z <- 
              paste0(paste0('z.', paste0(exposure.list, '_id')), collapse = ' + ')
            ## count
            exposures.formula.short.y <- 
              paste0(paste0('y.', paste0(exposure.list, '_id')), collapse = ' + ')
            
            # exposure model matrix
            ## zero
            exposure.matrix.z <- 
              paste0('~ ', exposures.formula.short.z) %>% 
              as.formula() %>% 
              stats::model.matrix(., data = data.z)
            ## count
            exposure.matrix.y <- 
              paste0('~ ', exposures.formula.short.y) %>% 
              as.formula() %>% 
              stats::model.matrix(., data = data.y)
            
            # long formula
            ## zero
            exposures.formula.long.z <- 
              paste0('z.intercept + ',
                     colnames(exposure.matrix.z[,-1]) %>% 
                       paste0(., collapse = ' + '))
            ## count
            exposures.formula.long.y <- 
              paste0('y.intercept + ',
                     colnames(exposure.matrix.y[,-1]) %>% 
                       paste0(., collapse = ' + '))
            ## final
            exposures.formula.long <-
              paste0(' ~ . +', paste0(c(exposures.formula.long.z, exposures.formula.long.y), collapse = ' + '))
            
          }
          
        } else {
          
          if(shared.exposures){
            
            # exposure model matrix
            exposure.matrix <- 
              paste0('~ ', 1) %>% 
              as.formula() %>% 
              stats::model.matrix(., data = data)
            
            # needed for stack later
            exposure.matrix.z <-
              exposure.matrix.y <-
              exposure.matrix
            
            # long formula
            exposures.formula.long <- ' ~ . + intercept'
            
          } else {
            
            # data with z. and y. infront of column names
            ## zero
            data.z <- 
              data %>%
              dplyr::select(paste0(exposure.list, '_id')) %>%
              setNames(paste0('z.', names(.)))
            ## count
            data.y <- 
              data %>%
              dplyr::select(paste0(exposure.list, '_id')) %>%
              setNames(paste0('y.', names(.)))
            
            # exposure model matrix
            ## zero
            exposure.matrix.z <- 
              paste0('~ ', 1) %>% 
              as.formula() %>% 
              stats::model.matrix(., data = data.z)
            ## count
            exposure.matrix.y <- 
              paste0('~ ', 1) %>% 
              as.formula() %>% 
              stats::model.matrix(., data = data.y)
            
            # long formula
            ## zero
            exposures.formula.long.z <- 'z.intercept'
            ## count
            exposures.formula.long.y <- 'y.intercept'
            ## final
            exposures.formula.long <-
              paste0(' ~ . +', paste0(c(exposures.formula.long.z, exposures.formula.long.y), collapse = ' + '))
            
          }
          
        }
        
        formula <-
          update(formula, exposures.formula.long)
        
        
        # add spatial random effect
        message('\n  Spatial random effect',
                paste0('\n   Shared: ', shared.spatial),
                appendLF = FALSE)
        if(model.spatial == '-'){
          message('\n   Model: None',
                  appendLF = FALSE)
        } else if (model.spatial == 'iid'){
          message(paste0('\n   Model: ', toupper(model.spatial)),
                  appendLF = FALSE)
          if(shared.spatial){
            # formula update
            formula <-
              update(formula, ~.  + 
                       f(z.space_id, 
                         model = model.spatial, 
                         constr = TRUE,
                         hyper = list(prec = list(prior = 'pc.prec', param = c(pc.u, pc.alpha))),
                         values = 1:n.spatial) +
                       f(y.space_id, 
                         copy = 'z.space_id', 
                         hyper = list(beta = list(fixed = FALSE))))
          } else {
            # formula update
            formula <-
              update(formula, ~.  + 
                       f(z.space_id, 
                         model = model.spatial, 
                         constr = TRUE,
                         hyper = list(prec = list(prior = 'pc.prec', param = c(pc.u, pc.alpha))),
                         values = 1:n.spatial) +
                       f(y.space_id, 
                         model = model.spatial, 
                         constr = TRUE,
                         hyper = list(prec = list(prior = 'pc.prec', param = c(pc.u, pc.alpha))),
                         values = 1:n.spatial))
          }
        } else if (model.spatial == 'bym2') {
          message(paste0('\n   Model: ', toupper(model.spatial)),
                  appendLF = FALSE)
          if(shared.spatial){
            # formula update
            formula <-
              update(formula, ~.  + 
                       f(z.space_id,
                         model = model.spatial, 
                         graph = aMat, 
                         constr = TRUE,
                         scale.model = TRUE,
                         hyper = list(prec = list(prior = 'pc.prec', param = c(pc.u, pc.alpha)),
                                      phi = list(prior = 'pc', param = c(pu.u.phi, pc.alpha.phi))),
                         values = 1:n.spatial) +
                       f(y.space_id, 
                         copy = 'z.space_id',
                         hyper = list(beta = list(fixed = FALSE))))
          } else {
            # formula update
            formula <-
              update(formula, ~.  + 
                       f(z.space_id, 
                         model = model.spatial, 
                         graph = aMat, 
                         constr = TRUE,
                         scale.model = TRUE,
                         hyper = list(prec = list(prior = 'pc.prec', param = c(pc.u, pc.alpha)),
                                      phi = list(prior = 'pc', param = c(pu.u.phi, pc.alpha.phi))),
                         values = 1:n.spatial) +
                       f(y.space_id, 
                         model = model.spatial, 
                         graph = aMat, 
                         constr = TRUE,
                         scale.model = TRUE,
                         hyper = list(prec = list(prior = 'pc.prec', param = c(pc.u, pc.alpha)),
                                      phi = list(prior = 'pc', param = c(pu.u.phi, pc.alpha.phi))),
                         values = 1:n.spatial))
          }
        } else {
          stop('Spatial model can be IID or BYM2.')
        }
        
        # add temporal random effect
        message('\n  Temporal random effect',
                paste0('\n   Shared: ', shared.temporal),
                appendLF = FALSE)
        if(model.temporal == '-'){
          message('\n   Model: None',
                  appendLF = FALSE)
        } else if (model.temporal %in% c('iid', 'rw1', 'rw2')) {
          message(paste0('\n   Model: ', toupper(model.temporal)),
                  appendLF = FALSE)
          if(shared.temporal){
            # formula update
            formula <-
              update(formula, ~ . + 
                       f(z.year_id, 
                         model = model.temporal, 
                         constr = TRUE,
                         hyper = list(prec = list(prior = 'pc.prec', param = c(pc.u, pc.alpha))),
                         values = 1:n.temporal) +
                       f(y.year_id,
                         copy = 'z.year_id',
                         hyper = list(beta = list(fixed = FALSE))))
          } else {
            # formula update
            formula <-
              update(formula, ~ . + 
                       f(z.year_id, 
                         model = model.temporal, 
                         constr = TRUE,
                         hyper = list(prec = list(prior = 'pc.prec', param = c(pc.u, pc.alpha))),
                         values = 1:n.temporal) +
                       f(y.year_id, 
                         constr = TRUE,
                         model = model.temporal, 
                         hyper = list(prec = list(prior = 'pc.prec', param = c(pc.u, pc.alpha))),
                         values = 1:n.temporal))
            
          }
        } else {
          stop('Temporal model can be IID, RW1 or RW2.')
        }
        
        # add spatio-temporal random effect
        message('\n  Spatial-temporal random effect',
                paste0('\n   Shared: ', shared.spatio.temporal),
                appendLF = FALSE)
        if(model.spatial.temporal == '-'){
          message('\n   Model: None',
                  appendLF = FALSE)
        } else if (model.spatial.temporal == '1'){
          message(paste0('\n   Model: Type ', as.roman(model.spatial.temporal)),
                  appendLF = FALSE)
          if(shared.spatio.temporal){
            formula <-
              update(formula, ~ . + 
                       f(z.spaceYear_id, 
                         model = 'iid', 
                         constr = TRUE,
                         hyper = list(prec = list(prior = 'pc.prec', param = c(pc.u, pc.alpha))),
                         values = 1:(n.spatial*n.temporal)) +
                       f(y.spaceYear_id,
                         copy = 'z.spaceYear_id',
                         hyper = list(beta = list(fixed = FALSE))))
          } else {
            formula <-
              update(formula, ~ . + 
                       f(z.spaceYear_id, 
                         model = 'iid', 
                         constr = TRUE,
                         hyper = list(prec = list(prior = 'pc.prec', param = c(pc.u, pc.alpha))),
                         values = 1:(n.spatial*n.temporal)) +
                       f(y.spaceYear_id,
                         model = 'iid', 
                         constr = TRUE,
                         hyper = list(prec = list(prior = 'pc.prec', param = c(pc.u, pc.alpha))),
                         values = 1:(n.spatial*n.temporal)))
          }
        } else if (model.spatial.temporal %in% c(2:4)) {
          message(paste0('\n   Model: Type ', as.roman(model.spatial.temporal)),
                  appendLF = FALSE)
          # uses a rw1 temporal component by default
          st.components <- precision.st.interaction(type.st = model.spatial.temporal, space = aMat, time = 1:n.temporal)
          if(shared.spatio.temporal){
            formula <-
              update(formula, ~ . + 
                       f(z.spaceYear_id, 
                         model = 'generic0',
                         Cmatrix = st.components$Q,
                         rankdef = st.components$rank.def,
                         constr = TRUE,
                         extraconstr = list(A = st.components$A.constr, e = rep(o, nrow(st.components$A.constr))),
                         hyper = list(prec = list(prior = 'pc.prec', param = c(pc.u, pc.alpha))),
                         values = 1:(n.spatial*n.temporal)) +
                       f(y.spaceYear_id,
                         copy = 'z.spaceYear_id',
                         hyper = list(beta = list(fixed = FALSE))))
          } else {
            formula <-
              update(formula, ~ . + 
                       f(z.spaceYear_id, 
                         model = 'generic0',
                         Cmatrix = st.components$Q,
                         rankdef = st.components$rank.def,
                         constr = TRUE,
                         extraconstr = list(A = st.components$A.constr, e = rep(o, nrow(st.components$A.constr))),
                         hyper = list(prec = list(prior = 'pc.prec', param = c(pc.u, pc.alpha))),
                         values = 1:(n.spatial*n.temporal)) +
                       f(y.spaceYear_id, 
                         model = 'generic0',
                         Cmatrix = st.components$Q,
                         rankdef = st.components$rank.def,
                         constr = TRUE,
                         extraconstr = list(A = st.components$A.constr, e = rep(o, nrow(st.components$A.constr))),
                         hyper = list(prec = list(prior = 'pc.prec', param = c(pc.u, pc.alpha))),
                         values = 1:(n.spatial*n.temporal)))
          }
        } else {
          stop('Spatio-temporal model can be Type I - IV.')
        }
        
      }
      
      end.1 <- proc.time() - start.1
      
      ### 3.1.2 data organising ----
      
      # identify non-zero counts
      non.zero.position <- data$Y > 0
      
      # data for stack
      ## zero
      Y.est.z <- cbind(dplyr::if_else(data$Y == 0, 0, 1), NA)
      ## count 
      Y.est.y <- cbind(NA, data$Y[non.zero.position])
      
      # Adjancy matrix
      A.full <- matrix(1, nrow = nrow(data), ncol = 1)
      A.non.zero <- matrix(1, nrow = nrow(data[non.zero.position,]), ncol = 1)
      
      # effects for stack
      ## exposures
      ### zero
      exposure.matrix.stk.z <- 
        exposure.matrix.z[,-1]
      ### count
      exposure.matrix.stk.y <- 
        # non zero only
        exposure.matrix.y[non.zero.position,-1]
      ## random effects
      ### zero
      random.effects.stk.z <-
        data %>% 
        dplyr::select('space_id', 'year_id', 'spaceYear_id') %>% 
        setNames(paste0('z.', names(.)))
      ### count
      random.effects.stk.y <-
        # non zero only
        data[non.zero.position,] %>% 
        dplyr::select('space_id', 'year_id', 'spaceYear_id') %>% 
        setNames(paste0('y.', names(.)))
      
      # define stacks
      ## zero
      stk.est.z <-
        INLA::inla.stack(data = list(Y = Y.est.z,
                                     link = 1),
                         A = list(A = A.full, 
                                  1),
                         effects = list(c(list(z.intercept = 1),
                                          list(intercept = 1)),
                                        data.frame(exposure.matrix.stk.z,
                                                   random.effects.stk.z)),
                         tag = 'z.est')
      ## count
      stk.est.y <-
        INLA::inla.stack(data = list(Y = Y.est.y,
                                     E = data$E[non.zero.position],
                                     link = 2),
                         A = list(A = A.non.zero,
                                  1),
                         effects = list(c(list(y.intercept = 1),
                                          list(intercept = 1)),
                                        data.frame(exposure.matrix.stk.y,
                                                   random.effects.stk.y)),
                         tag = 'y.est')
      
      # final inla data
      ## full stack
      stk.full <- INLA::inla.stack(stk.est.z, stk.est.y)
      ## new data
      data.new <- INLA::inla.stack.data(stk.full)
      
      ### 3.1.3 model fit ----
      
      start.2 <- proc.time()
      
      message('\n  Distribution',
              paste0('\n   Zero: ', family.zero.label),
              paste0('\n   Count: ', family.count.label),
              '\n Fitting model.... \n ',
              appendLF = FALSE)
      
      control.family <-
        switch(EXPR = family.count,
               nzpoisson = {
                 # empty list for both zero and count
                 list(list(), list())
               },
               zeroinflatedpoisson0 = {
                 # empty list for zero
                 # for count ensure proper truncation by setting theta to large negative value
                 # ensures logit^{-1}(theta) approached zero
                 list(list(),
                      list(hyper = list(theta = list(initial = -20, fixed = TRUE))))
               },
               zeroinflatednbinomial0 = {
                 # empty list for zero
                 # for count ensure proper truncation by setting theta to large negative value
                 # ensures logit^{-1}(theta) approached zero
                 list(list(),
                      list(hyper = list(theta = list(initial = -20, fixed = TRUE))))
               },
               stop(family.count,  ' must be a truncated distribution.') )
      
      fit <-
        INLA::inla(formula = formula,
                   family = c(family.zero, family.count),
                   data = data.new,
                   E = E,
                   control.compute = control.compute,
                   control.predictor = list(A = INLA::inla.stack.A(stk.full), link = data.new$link, compute = TRUE),
                   control.family = control.family,
                   control.inla = control.inla,
                   control.mode = control.mode,
                   inla.mode = inla.mode,
                   num.threads = round(parallel::detectCores()*0.8),
                   verbose = verbose,
                   ...)
      
      # for output
      shared.components <- list(exposures = shared.exposures, spatial = shared.spatial, temporal = shared.temporal, spatio.temporal = shared.spatio.temporal)
      
      end.2 <- proc.time() - start.2
      
    } else {
      
      ## 3.2. Non-hurdle model ----
      
      message('--------------------------------',
              '\nNon Hurdle Model Selected',
              appendLF = FALSE)
      
      start.1 <- proc.time()
      
      ### 3.2.1. model formula ----
      
      if(is.null(formula)) {
        
        message('\n Model Specification',
                '\n  Exposures:',  
                paste0('\n   Include: ', include.exposures),
                appendLF = FALSE)
        
        formula <-
          # final formula
          Y ~ - 1
        
        if(include.exposures){
          
          # short formula
          exposures.formula.short <- 
            paste0(paste0(c('deprivation', 'diversity', 'populationDensity', 'nighttimeLight', 'totalRail', 'totalRoad', 'ndvi', 'pm25', 'no2'), '_id'), 
                   collapse = ' + ')
          
          # exposure model matrix
          exposure.matrix <- 
            paste0('~ ', exposures.formula.short) %>% 
            as.formula() %>% 
            stats::model.matrix(., data = data)
          
          # long formula
          exposures.formula.long <- 
            paste0(' ~ . + intercept + ',
                   colnames(exposure.matrix[,-1]) %>% 
                     paste0(., collapse = ' + '))
          
        } else {
          
          # exposure model matrix
          exposure.matrix <- 
            paste0('~ ', 1) %>% 
            as.formula() %>% 
            stats::model.matrix(., data = data)
          
          # long formula
          exposures.formula.long <- ' ~ . + intercept'
          
        }
        
        formula <-
          update(formula, exposures.formula.long)
        
        # add spatial random effect
        message('\n  Spatial random effect',
                appendLF = FALSE)
        if(model.spatial == '-'){
          message('\n   Model: None',
                  appendLF = FALSE)
        } else if (model.spatial == 'iid'){
          message(paste0('\n   Model: ', toupper(model.spatial)),
                  appendLF = FALSE)
          # formula update
          formula <-
            update(formula, ~.  + 
                     f(space_id, 
                       model = model.spatial, 
                       constr = TRUE,
                       hyper = list(prec = list(prior = 'pc.prec', param = c(pc.u, pc.alpha))),
                       values = 1:n.spatial))
        } else if (model.spatial == 'bym2') {
          message(paste0('\n   Model: ', toupper(model.spatial)),
                  appendLF = FALSE)
          formula <-
            update(formula, ~.  + 
                     f(space_id, 
                       model = model.spatial, 
                       graph = aMat, 
                       constr = TRUE,
                       scale.model = TRUE,
                       hyper = list(prec = list(prior = 'pc.prec', param = c(pc.u, pc.alpha)),
                                    phi = list(prior = 'pc', param = c(pu.u.phi, pc.alpha.phi))),
                       values = 1:n.spatial))
        } else {
          stop('Spatial model can be IID or BYM2.')
        }
        
        # add temporal random effect
        message('\n  Temporal random effect',
                appendLF = FALSE)
        if(model.temporal == '-'){
          message('\n   Model: None',
                  appendLF = FALSE)
        } else if (model.temporal %in% c('iid', 'rw1', 'rw2')) {
          message(paste0('\n   Model: ', toupper(model.temporal)),
                  appendLF = FALSE)
          formula <-
            update(formula, ~ . + 
                     f(year_id, 
                       model = model.temporal, 
                       constr = TRUE,
                       hyper = list(prec = list(prior = 'pc.prec', param = c(pc.u, pc.alpha))),
                       values = 1:n.temporal))
        } else {
          stop('Temporal model can be IID, RW1 or RW2.')
        }
        
        # add spatio-temporal random effect
        message('\n  Spatial-temporal random effect',
                appendLF = FALSE)
        if(model.spatial.temporal == '-'){
          message('\n   Model: None',
                  appendLF = FALSE)
        } else if (model.spatial.temporal == '1'){
          message(paste0('\n   Model: Type ', as.roman(model.spatial.temporal)),
                  appendLF = FALSE)
          formula <-
            update(formula, ~ . + 
                     f(spaceYear_id, 
                       model = 'iid', 
                       constr = TRUE,
                       hyper = list(prec = list(prior = 'pc.prec', param = c(pc.u, pc.alpha))),
                       values = 1:(n.spatial*n.temporal)))
          
        } else if (model.spatial.temporal %in% c(2:4)) {
          message(paste0('\n   Model: Type ', as.roman(model.spatial.temporal)),
                  appendLF = FALSE)
          # uses a rw1 temporal component by default
          st.components <- precision.st.interaction(type.st = model.spatial.temporal, space = aMat, time = 1:n.temporal)
          formula <-
            update(formula, ~ . + 
                     f(z.spaceYear_id, 
                       model = 'generic0',
                       Cmatrix = st.components$Q,
                       rankdef = st.components$rank.def,
                       constr = TRUE,
                       extraconstr = list(A = st.components$A.constr, e = rep(o, nrow(st.components$A.constr))),
                       hyper = list(prec = list(prior = 'pc.prec', param = c(pc.u, pc.alpha))),
                       values = 1:(n.spatial*n.temporal)))
          
        } else {
          stop('Spatio-temporal model can be Type I - IV.')
        }
        
      }
      
      ### 3.2.2. data organisation ----
      
      # model matrix for exposures (fixed effects)
      exposure.matrix.stk <- 
        # model matrix with intercept column removed
        exposure.matrix[,-1]
      
      # random effects for stack
      random.effects.stk <-
        data %>% 
        dplyr::select('space_id', 'year_id', 'spaceYear_id')
      
      # define stacks
      stk <-
        INLA::inla.stack(data = list(Y = data$Y,
                                     E = data$E, 
                                     link = 1),
                         A = list(A = matrix(1, nrow = nrow(data), ncol = 1),
                                  1),
                         effects = list(list(intercept = 1),
                                        data.frame(exposure.matrix.stk,
                                                   random.effects.stk)),
                         tag = 'stk')
      
      # final inla data
      ## full stack
      stk.full <- INLA::inla.stack(stk)
      ## new data
      data.new <- INLA::inla.stack.data(stk.full)
      
      end.1 <- proc.time() - start.1
      
      start.2 <- proc.time()
      
      ### 3.2.3. model fit ----
      
      message('\n  Distribution',
              paste0('\n   Count: ', family.count.label),
              '\n Fitting model.... \n ',
              appendLF = FALSE)
      
      fit <-
        INLA::inla(formula = formula,
                   family = family.count,
                   data = data.new,
                   E = E,
                   control.compute = control.compute,
                   control.predictor = list(A = INLA::inla.stack.A(stk.full), link = data.new$link, compute = TRUE),
                   control.inla = control.inla,
                   control.mode = control.mode,
                   inla.mode = inla.mode,
                   num.threads = round(parallel::detectCores()*0.8),
                   verbose = verbose,
                   ...)
      
      # for output
      shared.components <- NULL
      
      end.2 <- proc.time() - start.2
      
    }
    
    # 4. return ----
    
    message('\nTime take for model fitting',
            '\n Data sort (s): ', end.1[3] %>% as.numeric() %>% round(., digits = 2),
            '\n Model fit (s): ', end.2[3] %>% as.numeric() %>% round(., digits = 2),
            '\n Total (s): ', sum(end.1[3], end.2[3]) %>% as.numeric() %>% round(., digits = 2),
            '\n--------------------------------\n',
            appendLF = FALSE)
    
    return(list(newdata = data.new,
                stack = stk.full,
                formula = formula,
                fit = fit,
                aMat= aMat,
                family = c(family.zero, family.count),
                hyper.prior = list(pc.u = pc.u, pc.alpha = pc.alpha,
                                   pu.u.phi = pu.u.phi, pc.alpha.phi = pc.alpha.phi),
                prior.model = list(model.spatial = model.spatial, model.temporal = model.temporal),
                shared.components = shared.components))
  }

## 2.3. sampling ----

extract.observations.poi <- function(newdata, fit, ...){
  
  # 0. function arguments ----
  
  # newdata <- data.model
  # fit <- fit
  
  # 1. fixed effects linear predictor ----
  
  ## 1.1. variable names ----
  
  linear.predictor.fixed.effects.parameters.all <-
    fit$fit$summary.fixed %>% 
    rownames() %>% 
    data.frame(parameter = .) %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(value = get(parameter))
  
  ## 1.2. parameters and model matrix ----
  
  # unique fixed effects
  linear.predictor.fixed.effects.parameters.unique <-
    linear.predictor.fixed.effects.parameters.all %>%
    dplyr::select(parameter) %>%
    dplyr::mutate(parameter = parameter %>% stringr::str_replace(., '(?<=_id).*', '')) %>%
    dplyr::distinct() %>%
    dplyr::pull(parameter)
  
  # final model matrix
  fixed.effects.model.matrix <-
    linear.predictor.fixed.effects.parameters.unique %>%
    stringr::str_remove(., pattern = 'intercept') %>%
    stringr::str_c(., collapse = ' + ') %>%
    paste0('~ 1', .) %>%
    as.formula() %>%
    stats::model.matrix(., data = newdata)
  # add correct intercept name
  colnames(fixed.effects.model.matrix)[colnames(fixed.effects.model.matrix) == '(Intercept)'] <- 'intercept'
  
  # all fixed effects
  linear.predictor.fixed.effects.parameters <-
    linear.predictor.fixed.effects.parameters.all %>%
    dplyr::filter(parameter %in% colnames(fixed.effects.model.matrix)) %>%
    dplyr::mutate(order = match(parameter, colnames(fixed.effects.model.matrix))) %>%
    dplyr::arrange(order) %>%
    dplyr::pull(value)
  
  # linear predictor
  linear.predictor.fixed.effects <-
    fixed.effects.model.matrix %*% linear.predictor.fixed.effects.parameters
  
  # 2. random effects linear predictor ----
  
  ## 2.0. from data input ----
  
  space.index <- newdata$space_id
  year.index <- newdata$year_id
  spaceYear.index <- newdata$spaceYear_id
  
  ## 2.1. variable names ----
  
  random.effects.name <-
    fit$fit$.args$formula %>% 
    terms() %>% 
    attr(x = ., which = 'term.labels') %>% 
    grep('f\\(', ., value = TRUE) %>% 
    stringr::str_extract(., '(?<=f\\().*?(?=,|\\))')
  
  ## 2.2. individual parameters ----
  
  if(any(grepl('space_id', random.effects.name))){
    linear.predictor.random.effects.space <- space_id[space.index]
  } else {
    linear.predictor.random.effects.space <- rep(0, times = nrow(newdata))
  }
  
  if(any(grepl('year_id', random.effects.name))){
    linear.predictor.random.effects.year <- year_id[year.index]
  } else {
    linear.predictor.random.effects.year <- rep(0, times = nrow(newdata))
  }
  
  if(any(grepl('spaceYear_id', random.effects.name))){
    linear.predictor.random.effects.spaceYear <- spaceYear_id[spaceYear.index]
  } else {
    linear.predictor.random.effects.spaceYear <- rep(0, times = nrow(newdata))
  }
  
  ## 2.3. linear predictor ----
  
  linear.predictor.random.effects <-
    linear.predictor.random.effects.space +
    linear.predictor.random.effects.year +
    linear.predictor.random.effects.spaceYear
  
  # 3. full linear predictor ----
  
  ## 3.1. dataframe ----
  
  linear.predictor <- 
    data.frame(full = linear.predictor.fixed.effects + linear.predictor.random.effects,
               fixed = linear.predictor.fixed.effects,
               random = linear.predictor.random.effects,
               random.spatial = linear.predictor.random.effects.space,
               random.temporal = linear.predictor.random.effects.year,
               random.spatiotemporal = linear.predictor.random.effects.spaceYear)
  
  ## 3.2. linear predictor ----
  
  linear.predictor <- linear.predictor$full
  
  # 4. observational quantities ----
  
  ## 4.0. from data input ----
  
  Y <- newdata$Y
  E <- newdata$E
  
  ## 4.1. linear predictor quantities ----
  
  rho <- exp(linear.predictor)
  lambda <- rho * E
  expected <- lambda
  variance <- lambda
  
  ## 4.2. distributional quantities ----
  
  cpo <- stats::dpois(x = Y, lambda = lambda)
  
  ### 4.3. observed value draws ----
  
  set.seed(1234)
  predicted <- stats::rpois(n = length(lambda), lambda = lambda)
  
  ### 4.4. data frame ----
  
  prediction <-
    data.frame(Y = Y,
               E = E,
               lambda = lambda,
               rho = rho,
               expected = expected,
               variance = variance,
               cpo = cpo,
               predicted = predicted)
  
  # 5. return ----
  
  return(list(linear.predictor = linear.predictor,
              prediction = prediction))
  
}

extract.observations.zip <- function(newdata, fit, ...){
  
  # 0. function arguments ----
  
  # newdata <- data.model
  # fit <- fit
  
  # 1. fixed effects linear predictor ----
  
  ## 1.1. variable names ----
  
  linear.predictor.fixed.effects.parameters.all <-
    fit$fit$summary.fixed %>% 
    rownames() %>% 
    data.frame(parameter = .) %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(value = get(parameter))
  
  ## 1.2. parameters and model matrix ----
  
  # unique fixed effects
  linear.predictor.fixed.effects.parameters.unique <-
    linear.predictor.fixed.effects.parameters.all %>%
    dplyr::select(parameter) %>%
    dplyr::mutate(parameter = parameter %>% stringr::str_replace(., '(?<=_id).*', '')) %>%
    dplyr::distinct() %>%
    dplyr::pull(parameter)
  
  # final model matrix
  fixed.effects.model.matrix <-
    linear.predictor.fixed.effects.parameters.unique %>%
    stringr::str_remove(., pattern = 'intercept') %>%
    stringr::str_c(., collapse = ' + ') %>%
    paste0('~ 1', .) %>%
    as.formula() %>%
    stats::model.matrix(., data = newdata)
  # add correct intercept name
  colnames(fixed.effects.model.matrix)[colnames(fixed.effects.model.matrix) == '(Intercept)'] <- 'intercept'
  
  # all fixed effects
  linear.predictor.fixed.effects.parameters <-
    linear.predictor.fixed.effects.parameters.all %>%
    dplyr::filter(parameter %in% colnames(fixed.effects.model.matrix)) %>%
    dplyr::mutate(order = match(parameter, colnames(fixed.effects.model.matrix))) %>%
    dplyr::arrange(order) %>%
    dplyr::pull(value)
  
  # linear predictor
  linear.predictor.fixed.effects <-
    fixed.effects.model.matrix %*% linear.predictor.fixed.effects.parameters
  
  # 2. random effects linear predictor ----
  
  ## 2.0. from data input ----
  
  space.index <- newdata$space_id
  year.index <- newdata$year_id
  spaceYear.index <- newdata$spaceYear_id
  
  ## 2.1. variable names ----
  
  random.effects.name <-
    fit$fit$.args$formula %>% 
    terms() %>% 
    attr(x = ., which = 'term.labels') %>% 
    grep('f\\(', ., value = TRUE) %>% 
    stringr::str_extract(., '(?<=f\\().*?(?=,|\\))')
  
  ## 2.2. individual parameters ----
  
  if(any(grepl('space_id', random.effects.name))){
    linear.predictor.random.effects.space <- space_id[space.index]
  } else {
    linear.predictor.random.effects.space <- rep(0, times = nrow(newdata))
  }
  
  if(any(grepl('year_id', random.effects.name))){
    linear.predictor.random.effects.year <- year_id[year.index]
  } else {
    linear.predictor.random.effects.year <- rep(0, times = nrow(newdata))
  }
  
  if(any(grepl('spaceYear_id', random.effects.name))){
    linear.predictor.random.effects.spaceYear <- spaceYear_id[spaceYear.index]
  } else {
    linear.predictor.random.effects.spaceYear <- rep(0, times = nrow(newdata))
  }
  
  ## 2.3. linear predictor ----
  
  linear.predictor.random.effects <-
    linear.predictor.random.effects.space +
    linear.predictor.random.effects.year +
    linear.predictor.random.effects.spaceYear
  
  # 3. full linear predictor ----
  
  ## 3.1. dataframe ----
  
  linear.predictor <- 
    data.frame(full = linear.predictor.fixed.effects + linear.predictor.random.effects,
               fixed = linear.predictor.fixed.effects,
               random = linear.predictor.random.effects,
               random.spatial = linear.predictor.random.effects.space,
               random.temporal = linear.predictor.random.effects.year,
               random.spatiotemporal = linear.predictor.random.effects.spaceYear)
  
  ## 3.2. linear predictor ----
  
  linear.predictor <- linear.predictor$full
  
  # 4. observational quantities ----
  
  ## 4.0. from data input ----
  
  Y <- newdata$Y
  E <- newdata$E
  
  ## 4.1. linear predictor quantities ----
  
  prob <- theta[1]
  rho <- exp(linear.predictor)
  lambda <- rho * E
  expected <- (1 - prob) * lambda
  variance <- (1 - prob) * (lambda + prob * lambda^2)
  
  ## 4.2. distributional quantities ----
  
  cpo <- prob * (Y == 0) + (1 - prob) * stats::dpois(x = Y, lambda = lambda)
  
  ### 4.3. observed value draws ----
  
  rpois.zip <- function(n, lambda, prob){
    # binomial samples
    samples.rbinom <- stats::rbinom(n = n, size = 1, prob = prob)
    # poisson samples
    samples.rpois <- stats::rpois(n = n, lambda = lambda)
    # zip samples
    dplyr::if_else(samples.rbinom == 1, 0, samples.rpois)
  }
  
  set.seed(1234)
  predicted <- rpois.zip(n = length(lambda), lambda = lambda, prob = prob)
  
  ### 4.4. data frame ----
  
  prediction <-
    data.frame(Y = Y,
               E = E,
               lambda = lambda,
               rho = rho,
               expected = expected,
               variance = variance,
               cpo = cpo,
               predicted = predicted)
  
  # 5. return ----
  
  return(list(linear.predictor = linear.predictor,
              prediction = prediction))
  
}

extract.observations.hp <- function(newdata, fit, ...){
  
  # 0. function arguments ----
  
  # newdata <- data.model
  # fit <- fit
  
  # 1. fixed effects linear predictor ----
  
  ## 1.1. variable names ----
  
  linear.predictor.fixed.effects.parameters.all <-
    fit$fit$summary.fixed %>% 
    rownames() %>% 
    data.frame(parameter = .) %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(value = get(parameter))
  
  ## 1.2. parameters and model matrix ----
  
  if(any(grepl('z.', linear.predictor.fixed.effects.parameters.all$parameter)) & any(grepl('y.', linear.predictor.fixed.effects.parameters.all$parameter))){
    
    ## 1.2.a. z ----
    
    # unique fixed effects 
    linear.predictor.fixed.effects.parameters.z.unique <- 
      linear.predictor.fixed.effects.parameters.all %>% 
      dplyr::select(parameter) %>% 
      dplyr::filter(stringr::str_starts(parameter, 'z.')) %>% 
      dplyr::mutate(parameter = parameter %>% stringr::str_replace(., '(?<=_id).*', '')) %>% 
      dplyr::distinct() %>% 
      dplyr::pull(parameter)
    
    # rename data accordingly
    newdata.z <-
      newdata %>% 
      dplyr::select(dplyr::ends_with('_id')) %>% 
      dplyr::rename_with(~ paste0('z.', .), dplyr::everything())
    
    # final model matrix
    fixed.effects.model.matrix.z <-
      linear.predictor.fixed.effects.parameters.z.unique %>% 
      stringr::str_remove(., pattern = 'z.intercept') %>% 
      stringr::str_c(., collapse = ' + ') %>% 
      paste0('~ 1', .) %>% 
      as.formula() %>% 
      stats::model.matrix(., data = newdata.z)
    # add correct intercept name 
    colnames(fixed.effects.model.matrix.z)[colnames(fixed.effects.model.matrix.z) == '(Intercept)'] <- 'z.intercept'
    
    # all fixed effects
    linear.predictor.fixed.effects.parameters.z <-
      linear.predictor.fixed.effects.parameters.all %>% 
      dplyr::filter(parameter %in% colnames(fixed.effects.model.matrix.z)) %>% 
      dplyr::mutate(order = match(parameter, colnames(fixed.effects.model.matrix.z))) %>% 
      dplyr::arrange(order) %>% 
      dplyr::pull(value)
    
    # linear predictor
    linear.predictor.fixed.effects.z <- fixed.effects.model.matrix.z %*% linear.predictor.fixed.effects.parameters.z
    
    ## 1.2.b. y  ----
    
    # unique fixed effects 
    linear.predictor.fixed.effects.parameters.y.unique <- 
      linear.predictor.fixed.effects.parameters.all %>% 
      dplyr::select(parameter) %>% 
      dplyr::filter(stringr::str_starts(parameter, 'y.')) %>% 
      dplyr::mutate(parameter = parameter %>% stringr::str_replace(., '(?<=_id).*', '')) %>% 
      dplyr::distinct() %>% 
      dplyr::pull(parameter)
    
    # rename data accordingly
    newdata.y <-
      newdata %>% 
      dplyr::select(dplyr::ends_with('_id')) %>% 
      dplyr::rename_with(~ paste0('y.', .), dplyr::everything())
    
    # final model matrix
    fixed.effects.model.matrix.y <-
      linear.predictor.fixed.effects.parameters.y.unique %>% 
      stringr::str_remove(., pattern = 'y.intercept') %>% 
      stringr::str_c(., collapse = ' + ') %>% 
      paste0('~ 1', .) %>% 
      as.formula() %>% 
      stats::model.matrix(., data = newdata.y)
    # add correct intercept name 
    colnames(fixed.effects.model.matrix.y)[colnames(fixed.effects.model.matrix.y) == '(Intercept)'] <- 'y.intercept'
    
    # all fixed effects
    linear.predictor.fixed.effects.parameters.y <-
      linear.predictor.fixed.effects.parameters.all %>% 
      dplyr::filter(parameter %in% colnames(fixed.effects.model.matrix.y)) %>% 
      dplyr::mutate(order = match(parameter, colnames(fixed.effects.model.matrix.y))) %>% 
      dplyr::arrange(order) %>% 
      dplyr::pull(value)
    
    # linear predictor
    linear.predictor.fixed.effects.y <- fixed.effects.model.matrix.y %*% linear.predictor.fixed.effects.parameters.y
    
  } else {
    
    ## 1.2.c. all ----
    
    # unique fixed effects
    linear.predictor.fixed.effects.parameters.unique <-
      linear.predictor.fixed.effects.parameters.all %>%
      dplyr::select(parameter) %>%
      dplyr::mutate(parameter = parameter %>% stringr::str_replace(., '(?<=_id).*', '')) %>%
      dplyr::distinct() %>%
      dplyr::pull(parameter)
    
    # final model matrix
    fixed.effects.model.matrix <-
      linear.predictor.fixed.effects.parameters.unique %>%
      stringr::str_remove(., pattern = 'intercept') %>%
      stringr::str_c(., collapse = ' + ') %>%
      paste0('~ 1', .) %>%
      as.formula() %>%
      stats::model.matrix(., data = newdata)
    # add correct intercept name
    colnames(fixed.effects.model.matrix)[colnames(fixed.effects.model.matrix) == '(Intercept)'] <- 'intercept'
    
    # all fixed effects
    linear.predictor.fixed.effects.parameters <-
      linear.predictor.fixed.effects.parameters.all %>%
      dplyr::filter(parameter %in% colnames(fixed.effects.model.matrix)) %>%
      dplyr::mutate(order = match(parameter, colnames(fixed.effects.model.matrix))) %>%
      dplyr::arrange(order) %>%
      dplyr::pull(value)
    
    # linear predictor
    linear.predictor.fixed.effects.z <-
      linear.predictor.fixed.effects.y <-
      fixed.effects.model.matrix %*% linear.predictor.fixed.effects.parameters
    
  }
  
  # 2. random effects linear predictor ----
  
  ## 2.0. from data input ----
  
  space.index <- newdata$space_id
  year.index <- newdata$year_id
  spaceYear.index <- newdata$spaceYear_id
  
  ## 2.1. variable names ----
  
  random.effects.name <-
    fit$fit$.args$formula %>% 
    terms() %>% 
    attr(x = ., which = 'term.labels') %>% 
    grep('f\\(', ., value = TRUE) %>% 
    stringr::str_extract(., '(?<=f\\().*?(?=,|\\))')
  
  ## 2.2. individual parameters ----
  
  if(any(grepl('space_id', random.effects.name))){
    linear.predictor.random.effects.space.z <- z.space_id[space.index]
    linear.predictor.random.effects.space.y <- y.space_id[space.index]
    linear.predictor.random.effects.space.all <- linear.predictor.random.effects.space.z + linear.predictor.random.effects.space.y
  } else {
    linear.predictor.random.effects.space.z <-
      linear.predictor.random.effects.space.y <- 
      linear.predictor.random.effects.space.all <- 
      rep(0, times = nrow(newdata))
  }
  
  if(any(grepl('year_id', random.effects.name))){
    linear.predictor.random.effects.year.z <- z.year_id[year.index]
    linear.predictor.random.effects.year.y <- y.year_id[year.index]
    linear.predictor.random.effects.year.all <- linear.predictor.random.effects.year.z + linear.predictor.random.effects.year.y
  } else {
    linear.predictor.random.effects.year.z <-
      linear.predictor.random.effects.year.y <- 
      linear.predictor.random.effects.year.all <- 
      rep(0, times = nrow(newdata))
  }
  
  if(any(grepl('spaceYear_id', random.effects.name))){
    linear.predictor.random.effects.spaceYear.z <- z.spaceYear_id[spaceYear.index]
    linear.predictor.random.effects.spaceYear.y <- y.spaceYear_id[spaceYear.index]
    linear.predictor.random.effects.spaceYear.all <- linear.predictor.random.effects.spaceYear.z + linear.predictor.random.effects.spaceYear.y
  } else {
    linear.predictor.random.effects.spaceYear.z <-
      linear.predictor.random.effects.spaceYear.y <- 
      linear.predictor.random.effects.spaceYear.all <- 
      rep(0, times = nrow(newdata))
  }
  
  ## 2.3. linear predictor ----
  
  linear.predictor.random.effects.z <-
    linear.predictor.random.effects.space.z +
    linear.predictor.random.effects.year.z +
    linear.predictor.random.effects.spaceYear.z
  
  linear.predictor.random.effects.y <-
    linear.predictor.random.effects.space.y +
    linear.predictor.random.effects.year.y +
    linear.predictor.random.effects.spaceYear.y
  
  # 3. full linear predictor ----
  
  ## 3.1. dataframe ----
  
  linear.predictor <- 
    data.frame(full.z = linear.predictor.fixed.effects.z + linear.predictor.random.effects.z,
               full.y = linear.predictor.fixed.effects.y + linear.predictor.random.effects.y,
               fixed.z = linear.predictor.fixed.effects.z,
               fixed.y = linear.predictor.fixed.effects.y,
               random.z = linear.predictor.random.effects.z,
               random.y = linear.predictor.random.effects.y,
               random.spatial.z = linear.predictor.random.effects.space.z,
               random.spatial.y = linear.predictor.random.effects.space.y,
               random.temporal.z = linear.predictor.random.effects.year.z,
               random.temporal.y = linear.predictor.random.effects.year.y,
               random.spatiotemporal.z = linear.predictor.random.effects.spaceYear.z,
               random.spatiotemporal.y = linear.predictor.random.effects.spaceYear.y)
  
  ## 3.2. linear predictor ----
  
  linear.predictor.z <- linear.predictor$full.z
  linear.predictor.y <- linear.predictor$full.y
  
  # 4. observational quantities ----
  
  ## 4.0. from data input ----
  
  Y <- newdata$Y
  E <- newdata$E
  
  ## 4.1. linear predictor quantities ----
  
  # calculated quantities
  # prob <- stats::plogis(z.APredictor, location = 0, scale = 1)
  prob <- 1 - stats::pbinom(q = 0, size = 1, prob = expit(linear.predictor.z)) 
  rho <- exp(linear.predictor.y)
  lambda <- rho * E
  expected <- prob * lambda / (1 - exp(-lambda))
  variance <- expected * (1 - (exp(-lambda) * expected))
  
  ## 4.2. distributional quantities ----
  
  cpo <- 
    (1 - prob) * (Y == 0) + 
    (Y > 0) * prob * stats::dpois(x = Y, lambda = lambda * prob) / (1 - stats::dpois(x = 0, lambda = lambda * prob))
  
  ### 4.3. observed value draws ----
  
  rpois.hp <- function(n, lambda, prob){
    # zero truncated poisson samples
    zero.dens.dpois <- stats::dpois(x = 0, lambda = lambda)
    U.runif <- stats::runif(n = n, min = zero.dens.dpois, max = 1)
    samples.zt.rpois <- stats::qpois(p = U.runif, lambda = lambda)
    # binomial samples
    samples.rbinom <- stats::rbinom(n = n, size = 1, prob = prob)
    # hurdle poisson samples
    dplyr::if_else(samples.rbinom == 1, 0, samples.zt.rpois)
  }
  
  set.seed(1234)
  predicted <- rpois.hp(n = length(lambda), lambda = lambda, prob = 1 - prob)
  
  ### 4.4. data frame ----
  
  prediction <-
    data.frame(Y = Y,
               E = E,
               pi = prob,
               lambda = lambda,
               rho = rho,
               expected = expected,
               variance = variance,
               cpo = cpo,
               predicted = predicted)
  
  # 5. return ----
  
  return(list(linear.predictor = linear.predictor,
              prediction = prediction))
  
}

extract.parameters <- function(newdata, fit, ...){
  
  # 0. function arguments ----
  
  # newdata <- data.model
  # fit <- fit
  
  # 1. fixed effects ----
  
  fixed.effects <- 
    fit$fit$summary.fixed %>% 
    rownames() %>% 
    data.frame(parameter = .) %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(value = get(parameter))
  
  # 2. random effects ----
  
  random.effect.name <-
    fit$fit$summary.random %>% 
    names() %>% 
    setNames(as.list(.), .)
  
  random.effects <- 
    lapply(X = random.effect.name,
           FUN = function(x) {get(x)}) %>% 
    purrr::map2(.x = names(.),
                .y = .,
                .f = ~ data.frame(parameter = paste0(.x, ':', seq_along(.y)), 
                                  value = .y)) %>% 
    dplyr::bind_rows()
  
  # 3. hyperparemeters ----
  
  hyperparameters <- 
    data.frame(parameter = fit$fit$summary.hyperpar %>% rownames(),
               value = theta)
  
  # 4. return ----
  
  return(list(fixed.effects = fixed.effects,
              random.effects = random.effects,
              hyperparameters = hyperparameters))
  
}

# 3. plotting ----

## 3.1. themes ----

### 3.1.1. all plots ----

my.theme<-function(...){
  theme(panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(),
        axis.line = ggplot2::element_line(colour = 'black'),
        legend.text = ggplot2::element_text(hjust = 0),
        legend.key = ggplot2::element_rect(fill = NA, colour = NA),
        ...)
}

### 3.1.2. map plots ----

my.map.theme <- function(...){
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_blank(),
                 axis.ticks.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_blank(),
                 axis.ticks.y = ggplot2::element_blank(),
                 legend.text = ggplot2::element_text(hjust = 0),
                 legend.key = ggplot2::element_rect(fill = NA, colour = NA),
                 panel.background = ggplot2::element_blank(),
                 panel.grid.major = ggplot2::element_blank(),
                 panel.grid.minor = ggplot2::element_blank(),
                 ...)
}

## 3.2. radar plot ----

calculate.axis.path <- function(var.names, min, max) {
  # var.names <- c('v1','v2','v3','v4','v5')
  n.vars <- length(var.names) # number of vars (axes) required
  # Cacluate required number of angles (in radians)
  angles <- seq(from = 0, to = 2 * pi, by = (2 * pi) / n.vars)
  # calculate vectors of min and max x+y coords
  min.x <- min * sin(angles)
  min.y <- min * cos(angles)
  max.x <- max * sin(angles)
  max.y <- max * cos(angles)
  # Combine into a set of uniquely numbered paths (one per variable)
  data.axis <- NULL
  for (i in 1:n.vars) {
    a <- c(i, min.x[i], min.y[i])
    b <- c(i, max.x[i], max.y[i])
    data.axis <- rbind(data.axis, a, b)
  }
  # Add column names + set row names = row no. to allow conversion into a data frame
  colnames(data.axis) <- c('axis.no', 'x', 'y')
  rownames(data.axis) <- seq(1:nrow(data.axis))
  # Return calculated axis paths
  as.data.frame(data.axis)
}

calculate.group.path <- function(data) {
  # Drop dead levels. This might happen if the data is filtered on the way
  # into ggradar.
  path <- forcats::fct_drop(data[, 1])
  # set the name of the variable that will be used for grouping
  name.group <- colnames(data)[1]
  
  ## find increment
  n.path.points <- ncol(data) - 1
  angles <- seq(from = 0, to = 2 * pi, by = (2 * pi) / n.path.points)
  ## create graph data frame
  n.data.points <- ncol(data) * length(levels(path))
  data.graph <- data.frame(
    seg = rep('',n.data.points),
    x = rep(0, n.data.points),
    y = rep(0, n.data.points))
  colnames(data.graph)[1] <- name.group
  
  rowNum <- 1
  for (i in 1:length(levels(path))) {
    data.path <- subset(data, data[, 1] == levels(path)[i])
    for (j in c(2:ncol(data))) {
      data.graph[rowNum,name.group] <- levels(path)[i]
      data.graph$x[rowNum] <- data.path[, j] * sin(angles[j - 1])
      data.graph$y[rowNum] <- data.path[, j] * cos(angles[j - 1])
      rowNum <- rowNum + 1
    }
    ## complete the path by repeating first pair of coords in the path
    data.graph[rowNum,name.group] <- levels(path)[i]
    data.graph$x[rowNum] <- data.path[, 2] * sin(angles[1])
    data.graph$y[rowNum] <- data.path[, 2] * cos(angles[1])
    rowNum <- rowNum + 1
  }
  # Make sure that name of first column matches that of input data (in case !='group')
  data.graph[,1] <- factor(data.graph[,1], levels=levels(path) ) # keep group order
  data.graph # data frame returned by function
}

circle.coordinates <- function(center = c(0, 0), r = 1, n.points = 100) {
  tt <- seq(0, 2 * pi, length.out = n.points)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

generate.colour.values <- function(n.groups) {
  # Fallback colors for 1 or 2 groups
  colours.default <- c('#E41A1C', '#377EB8') # Adjust these colors as needed
  
  if (n.groups == 1) {
    # Return the first color if only one group is requested
    return(colours.default[1])
  } else if (n.groups == 2) {
    # Return the first two colors for two groups
    return(colours.default[1:2])
  } else if (n.groups <= max(RColorBrewer::brewer.pal.info$maxcolors)) {
    # Use RColorBrewer for 3 to max colors
    return(RColorBrewer::brewer.pal(n.groups, 'Set3'))
  } else {
    # For more than the maximum supported colors in RColorBrewer, use a color ramp
    return(colorRampPalette(RColorBrewer::brewer.pal(8, 'Set3'))(n.groups))
  }
}

ggradar <- function(data.plot,
                    data.gradient,
                    reverse.order = FALSE,
                    include.gradient = FALSE,
                    parse.labels = FALSE,
                    base.size = 15,
                    font.radar = 'sans',
                    values.radar = c('0%', '50%', '100%'),
                    axis.labels = colnames(data.plot)[-1],
                    grid.min = 0, # 10,
                    grid.mid = 0.5, # 50,
                    grid.max = 1, # 100,
                    centre.y = grid.min - ((1 / 9) * (grid.max - grid.min)),
                    plot.extent.x.sf = 1,
                    plot.extent.y.sf = 1.2,
                    x.centre.range = 0.02 * (grid.max - centre.y),
                    label.centre.y = FALSE,
                    grid.line.width = 0.5,
                    gridline.min.linetype = 'longdash',
                    gridline.mid.linetype = 'longdash',
                    gridline.max.linetype = 'longdash',
                    gridline.min.colour = 'grey',
                    gridline.mid.colour = '#007A87',
                    gridline.max.colour = 'grey',
                    grid.label.size = 6,
                    gridline.label.offset = -0.1 * (grid.max - centre.y),
                    label.gridline.min = TRUE,
                    label.gridline.mid = TRUE,
                    label.gridline.max = TRUE,
                    axis.label.offset = 1.15,
                    axis.label.size = 5,
                    axis.line.colour = 'grey',
                    group.line.width = 1.5,
                    group.point.size = 6,
                    group.colours = NULL,
                    background.circle.colour = '#D7D6D1',
                    background.circle.transparency = 0.2,
                    plot.legend = if (nrow(data.plot) > 1) TRUE else FALSE,
                    legend.title = '',
                    plot.title = '',
                    legend.text.size = 14,
                    legend.position = 'left',
                    fill = FALSE,
                    fill.alpha = 0.5,
                    draw.points = TRUE, # Whether to draw points
                    point.alpha = 1, # Alpha for points, can be a single value or vector
                    line.alpha = 1, # Alpha for lines, can be a single value or vector
                    ...) {
  
  data.plot <- as.data.frame(data.plot)
  
  if(reverse.order){
    
    data.plot <-
      data.plot %>% 
      dplyr::mutate(group = group %>% forcats::fct_rev())
    
    group.colours.original <- group.colours
    group.colours <- group.colours %>% rev()
    
  } else {
    
    group.colours.original <- group.colours
    
  }
  
  
  # if there are several groups in the first column with differing values
  # on the dimensions, we should aggregate them by taking the mean, otherwise
  # only the first row is taken into account in the function calculate.group.path.
  data.plot <- aggregate(
    x = data.plot[, -1], 
    by = list(data.plot[, 1]), 
    FUN = 'mean')
  
  if (!is.factor(data.plot[, 1])) {
    data.plot[, 1] <- as.factor(as.character(data.plot[, 1]))
  }
  
  var.names <- colnames(data.plot)[-1] # Short version of variable names
  # axis.labels [if supplied] is designed to hold 'long version' of variable names
  # with line-breaks indicated using \n
  
  # calculate total plot extent as radius of outer circle x a user-specifiable scaling factor
  plot.extent.x <- (grid.max + abs(centre.y)) * plot.extent.x.sf
  plot.extent.y <- (grid.max + abs(centre.y)) * plot.extent.y.sf
  
  # Check supplied data makes sense
  if (length(axis.labels) != ncol(data.plot) - 1) {
    stop('axis.labels contains the wrong number of axis labels', call. = FALSE)
  }
  if (min(data.plot[, -1]) < centre.y) {
    stop('data.plot contains value(s) < centre.y', call. = FALSE)
  }
  
  if (max(data.plot[, -1]) > grid.max) {
    data.plot[, -1] <- (data.plot[, -1]/max(data.plot[, -1]))*grid.max
    warning('data.plot contains value(s) > grid.max, data scaled to grid.max', call. = FALSE)
  }
  
  
  ### Convert supplied data into plottable format
  # (a) add abs(centre.y) to supplied plot data
  # [creates plot centroid of 0,0 for internal use, regardless of min. value of y
  # in user-supplied data]
  data.plot.offset <- data.plot
  data.plot.offset[, 2:ncol(data.plot)] <- data.plot[, 2:ncol(data.plot)] + abs(centre.y)
  # print(data.plot.offset)
  # (b) convert into radial coords
  group <- NULL
  group$path <- calculate.group.path(data.plot.offset)
  
  # print(group$path)
  # (c) Calculate coordinates required to plot radial variable axes
  axis <- NULL
  axis$path <- calculate.axis.path(var.names, grid.min + abs(centre.y), grid.max + abs(centre.y))
  # print(axis$path)
  # (d) Create file containing axis labels + associated plotting coordinates
  # Labels
  axis$label <- data.frame(
    text = axis.labels,
    x = NA,
    y = NA
  )
  # print(axis$label)
  # axis label coordinates
  n.vars <- length(var.names)
  angles <- seq(from = 0, to = 2 * pi, by = (2 * pi) / n.vars)
  axis$label$x <- sapply(1:n.vars, function(i, x) {
    ((grid.max + abs(centre.y)) * axis.label.offset) * sin(angles[i])
  })
  axis$label$y <- sapply(1:n.vars, function(i, x) {
    ((grid.max + abs(centre.y)) * axis.label.offset) * cos(angles[i])
  })
  # print(axis$label)
  # (e) Create Circular grid-lines + labels
  # caclulate the cooridinates required to plot circular grid-lines for three user-specified
  # y-axis values: min, mid and max [grid.min; grid.mid; grid.max]
  gridline <- NULL
  gridline$min$path <- circle.coordinates(c(0, 0), grid.min + abs(centre.y), n.points = 360)
  gridline$mid$path <- circle.coordinates(c(0, 0), grid.mid + abs(centre.y), n.points = 360)
  gridline$max$path <- circle.coordinates(c(0, 0), grid.max + abs(centre.y), n.points = 360)
  # print(head(gridline$max$path))
  # gridline labels
  gridline$min$label <- 
    data.frame(x = gridline.label.offset, 
               y = grid.min + abs(centre.y),
               text = as.character(grid.min))
  gridline$max$label <- 
    data.frame(x = gridline.label.offset,
               y = grid.max + abs(centre.y),
               text = as.character(grid.max))
  gridline$mid$label <- 
    data.frame(x = gridline.label.offset, 
               y = grid.mid + abs(centre.y),
               text = as.character(grid.mid))
  ### Start building up the radar plot
  
  if (plot.legend == FALSE) {
    legend.position <- 'none'
  }
  
  # Base-layer = axis labels + plot extent
  # [need to declare plot extent as well, since the axis labels don't always
  # fit within the plot area automatically calculated by ggplot, even if all
  # included in first plot; and in any case the strategy followed here is to first
  # plot right-justified labels for axis labels to left of Y axis for x< (-x.centre.range)],
  # then centred labels for axis labels almost immediately above/below x= 0
  # [abs(x) < x.centre.range]; then left-justified axis labels to right of Y axis [x>0].
  # This building up the plot in layers doesn't allow ggplot to correctly
  # identify plot extent when plotting first (base) layer]
  
  # base layer = axis labels for axes to left of central y-axis [x< -(x.centre.range)]
  plot.radar <- 
    ggplot2::ggplot(axis$label) +
    ggplot2::xlab(NULL) +
    ggplot2::ylab(NULL) +
    ggplot2::coord_equal() +
    ggplot2::geom_text(data = subset(axis$label, axis$label$x < (-x.centre.range)),
                       aes(x = x, y = y, label = stringr::str_wrap(text, width = 10)), 
                       size = axis.label.size, 
                       hjust = 1, 
                       family = font.radar, 
                       parse = parse.labels) +
    ggplot2::scale_x_continuous(limits = c(-1.5 * plot.extent.x, 1.5 * plot.extent.x)) +
    ggplot2::scale_y_continuous(limits = c(-plot.extent.y, plot.extent.y)) + 
    # circular grid-lines at 'min', 'mid' and 'max' y-axis values
    ## min
    ggplot2::geom_path(data = gridline$min$path, aes(x = x, y = y),
                       lty = gridline.min.linetype, 
                       colour = gridline.min.colour, 
                       linewidth = grid.line.width) +
    ggplot2::geom_path(data = gridline$mid$path, 
                       aes(x = x, y = y),
                       lty = gridline.mid.linetype, 
                       colour = gridline.mid.colour,
                       linewidth = grid.line.width) +
    ## max
    ggplot2::geom_path(data = gridline$max$path, aes(x = x, y = y),
                       lty = gridline.max.linetype, 
                       colour = gridline.max.colour, 
                       linewidth = grid.line.width) + 
    # axis labels for any vertical axes [abs(x)<=x.centre.range]
    ggplot2::geom_text(data = subset(axis$label, abs(axis$label$x) <= x.centre.range),
                       aes(x = x, y = y, label = stringr::str_wrap(text, width = 10)), 
                       size = axis.label.size, 
                       hjust = 0.5, 
                       family = font.radar, 
                       parse = parse.labels) + 
    # axis labels for any vertical axes [x>x.centre.range]
    ggplot2::geom_text(data = subset(axis$label, axis$label$x > x.centre.range),
                       aes(x = x, y = y, label = stringr::str_wrap(text, width = 10)), 
                       size = axis.label.size, 
                       hjust = 0, 
                       family = font.radar, 
                       parse = parse.labels) +
    # background circle against which to plot radar data
    ggplot2::geom_polygon(data = gridline$max$path, 
                          aes(x, y),
                          fill = background.circle.colour,
                          alpha = background.circle.transparency) + 
    # radial axes
    ggplot2::geom_path(data = axis$path, 
                       aes(x = x, y = y, group = axis.no),
                       colour = axis.line.colour)
  
  name.group <- names(group$path[1])
  
  if (length(line.alpha) == 1) {
    plot.radar <- 
      plot.radar + 
      ggplot2::geom_path(data = group$path, 
                         aes(x = .data[['x']], y = .data[['y']], group = .data[[name.group]], colour = .data[[name.group]]), 
                         linewidth = group.line.width, 
                         alpha = line.alpha)
  } else {
    # Assuming line.alpha is a vector with the same length as the number of groups
    # This will apply different alpha values to each line
    plot.radar <- 
      plot.radar + 
      ggplot2::geom_path(data = group$path, 
                         aes(x = .data[['x']], y = .data[['y']], group = .data[[name.group]], colour = .data[[name.group]]), 
                         linewidth = group.line.width) +
      ggplot2::scale_alpha_manual(values = line.alpha)
  }
  
  # group points (cluster data)
  # Modify point drawing logic based on draw.points
  if (draw.points) {
    # Check if point.alpha is a vector or single value
    if (length(point.alpha) == 1) {
      plot.radar <- 
        plot.radar + 
        ggplot2::geom_point(data = group$path, 
                            aes(x = .data[['x']], y = .data[['y']], group = .data[[name.group]], colour = .data[[name.group]]), 
                            size = group.point.size, 
                            alpha = point.alpha)
    } else {
      # Assuming point.alpha is a vector with the same length as the number of groups
      # This will apply different alpha values to each group
      plot.radar <- 
        plot.radar + 
        ggplot2::geom_point(data = group$path, 
                            aes(x = .data[['x']], y = .data[['y']], group = .data[[name.group]], colour = .data[[name.group]]), 
                            size = group.point.size) +
        ggplot2::scale_alpha_manual(values = point.alpha)
    }
  }
  
  # group (cluster) fills
  if (fill == TRUE) {
    plot.radar <- 
      plot.radar + 
      ggplot2::geom_polygon(data = group$path, 
                            aes(x = .data[['x']], y = .data[['y']], group = .data[[name.group]], fill = .data[[name.group]]), 
                            alpha = fill.alpha)
  }
  
  # amend Legend title
  if (plot.legend == TRUE) {
    plot.radar <- 
      plot.radar + 
      ggplot2::labs(colour = legend.title, size = legend.text.size)
  }
  
  # grid-line labels (max; mid; min)
  if (label.gridline.min == TRUE) {
    plot.radar <- 
      plot.radar + 
      ggplot2::geom_text(data = gridline$min$label, 
                         aes(x = x, y = y, label = values.radar[1]), 
                         size = grid.label.size * 0.8, 
                         hjust = 1, 
                         family = font.radar)
  }
  
  if (label.gridline.mid == TRUE) {
    plot.radar <- 
      plot.radar + 
      ggplot2::geom_text(data = gridline$mid$label, 
                         aes(x = x, y = y, label = values.radar[2]), 
                         size = grid.label.size * 0.8, 
                         hjust = 1, 
                         family = font.radar)
  }
  
  if (label.gridline.max == TRUE) {
    plot.radar <- 
      plot.radar + 
      ggplot2::geom_text(data = gridline$max$label, 
                         aes(x = x, y = y, label = values.radar[3]), 
                         size = grid.label.size * 0.8, 
                         hjust = 1, 
                         family = font.radar)
  }
  
  # centre.y label if required [i.e. value of y at centre of plot circle]
  if (label.centre.y == TRUE) {
    centre.y.label <- data.frame(x = 0, y = 0, text = as.character(centre.y))
    plot.radar <- 
      plot.radar + 
      ggplot2::geom_text(data = centre.y.label, 
                         aes(x = x, y = y, label = text), 
                         size = grid.label.size, 
                         hjust = 0.5, 
                         family = font.radar)
  }
  
  if (!is.null(group.colours)) {
    colour.values <- rep(group.colours, length(unique(data.plot[, 1])) / length(group.colours))
  } else {
    colour.values <- generate.color.values(length(unique(data.plot[, 1])))
  }
  
  plot.radar <- 
    plot.radar +
    # sort theme
    ggplot2::theme_bw(base_size = base.size) +
    ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank(),
                   legend.key = ggplot2::element_rect(linetype = 'blank'),
                   legend.key.width = unit(2, 'cm'),
                   text = ggplot2::element_text(size = 20, family = font.radar),
                   legend.text = element_text(size = legend.text.size),
                   legend.position = legend.position,
                   legend.key.height = unit(2, 'line'),
                   legend.title = ggplot2::element_blank()) +
    ggplot2::scale_colour_manual(values = colour.values)
  
  
  if (isTRUE(fill)) {
    plot.radar <- 
      plot.radar +
      ggplot2::scale_fill_manual(values = colour.values, 
                                 guide = 'none')
  }
  
  if (legend.title != '') {
    plot.radar <- 
      plot.radar + 
      ggplot2::theme(legend.title = ggplot2::element_text())
  }
  
  if (plot.title != '') {
    plot.radar <- 
      plot.radar + 
      ggplot2::ggtitle(plot.title)
  }
  
  if(include.gradient){
    
    data.gradient <- 
      data.gradient %>% 
      dplyr::mutate(x = 0, y = 0, group = group %>% as.numeric())
    
    
    # where change occurs
    data.gradient.midpoint <- 
      data.gradient %>% 
      dplyr::filter(dplyr::row_number() == which(mean > 1)[1]) %>% 
      dplyr::pull(group)
    
    # define colours
    colours.low <- group.colours.original[min(data.gradient$group)]
    colours.mid <- group.colours.original[data.gradient.midpoint]
    colours.high <- group.colours.original[max(data.gradient$group)]
    
    # labels for legend
    data.gradient.labels <-
      data.gradient %>% 
      dplyr::filter(group == min(group) |
                      group == data.gradient.midpoint |
                      group == max(group)) %>% 
      dplyr::mutate(label = mean %>% round(., digits = 1)) %>% 
      dplyr::mutate(label = dplyr::case_when(group == min(group) ~ floor(label),
                                             group == data.gradient.midpoint ~ 1,
                                             group == max(group) ~ ceiling(label)))
    
    
    plot.radar <-
      plot.radar +
      ggplot2::geom_point(data = data.gradient, 
                          aes(x = y, y = y, fill = group),
                          size = 0,
                          alpha = 0) +
      ggplot2::scale_fill_gradient2(name = '',
                                    low = colours.low,
                                    mid = colours.mid,
                                    high = colours.high,
                                    midpoint = data.gradient.midpoint,
                                    breaks = data.gradient.labels$group,
                                    labels = data.gradient.labels$label %>% as.character()) +
      ggplot2::guides(color = 'none',
                      fill = ggplot2::guide_colourbar())
    
  }
  
  
  return(plot.radar)
}

ggradar.original <- function(data.plot,
                             base.size = 15,
                             font.radar = 'sans',
                             values.radar = c('0%', '50%', '100%'),
                             axis.labels = colnames(data.plot)[-1],
                             grid.min = 0, # 10,
                             grid.mid = 0.5, # 50,
                             grid.max = 1, # 100,
                             centre.y = grid.min - ((1 / 9) * (grid.max - grid.min)),
                             plot.extent.x.sf = 1,
                             plot.extent.y.sf = 1.2,
                             x.centre.range = 0.02 * (grid.max - centre.y),
                             label.centre.y = FALSE,
                             grid.line.width = 0.5,
                             gridline.min.linetype = 'longdash',
                             gridline.mid.linetype = 'longdash',
                             gridline.max.linetype = 'longdash',
                             gridline.min.colour = 'grey',
                             gridline.mid.colour = '#007A87',
                             gridline.max.colour = 'grey',
                             grid.label.size = 6,
                             gridline.label.offset = -0.1 * (grid.max - centre.y),
                             label.gridline.min = TRUE,
                             label.gridline.mid = TRUE,
                             label.gridline.max = TRUE,
                             axis.label.offset = 1.15,
                             axis.label.size = 5,
                             axis.line.colour = 'grey',
                             group.line.width = 1.5,
                             group.point.size = 6,
                             group.colours = NULL,
                             background.circle.colour = '#D7D6D1',
                             background.circle.transparency = 0.2,
                             plot.legend = if (nrow(data.plot) > 1) TRUE else FALSE,
                             legend.title = '',
                             plot.title = '',
                             legend.text.size = 14,
                             legend.position = 'left',
                             fill = FALSE,
                             fill.alpha = 0.5,
                             draw.points = TRUE, # Whether to draw points
                             point.alpha = 1, # Alpha for points, can be a single value or vector
                             line.alpha = 1 # Alpha for lines, can be a single value or vector
) {
  
  data.plot <- as.data.frame(data.plot)
  # if there are several groups in the first column with differing values
  # on the dimensions, we should aggregate them by taking the mean, otherwise
  # only the first row is taken into account in the function calculate.group.path.
  data.plot <- aggregate(
    x = data.plot[, -1], 
    by = list(data.plot[, 1]), 
    FUN = 'mean')
  
  if (!is.factor(data.plot[, 1])) {
    data.plot[, 1] <- as.factor(as.character(data.plot[, 1]))
  }
  
  var.names <- colnames(data.plot)[-1] # Short version of variable names
  # axis.labels [if supplied] is designed to hold 'long version' of variable names
  # with line-breaks indicated using \n
  
  # calculate total plot extent as radius of outer circle x a user-specifiable scaling factor
  plot.extent.x <- (grid.max + abs(centre.y)) * plot.extent.x.sf
  plot.extent.y <- (grid.max + abs(centre.y)) * plot.extent.y.sf
  
  # Check supplied data makes sense
  if (length(axis.labels) != ncol(data.plot) - 1) {
    stop('axis.labels contains the wrong number of axis labels', call. = FALSE)
  }
  if (min(data.plot[, -1]) < centre.y) {
    stop('data.plot contains value(s) < centre.y', call. = FALSE)
  }
  
  if (max(data.plot[, -1]) > grid.max) {
    data.plot[, -1] <- (data.plot[, -1]/max(data.plot[, -1]))*grid.max
    warning('data.plot contains value(s) > grid.max, data scaled to grid.max', call. = FALSE)
  }
  
  
  ### Convert supplied data into plottable format
  # (a) add abs(centre.y) to supplied plot data
  # [creates plot centroid of 0,0 for internal use, regardless of min. value of y
  # in user-supplied data]
  data.plot.offset <- data.plot
  data.plot.offset[, 2:ncol(data.plot)] <- data.plot[, 2:ncol(data.plot)] + abs(centre.y)
  # print(data.plot.offset)
  # (b) convert into radial coords
  group <- NULL
  group$path <- calculate.group.path(data.plot.offset)
  
  # print(group$path)
  # (c) Calculate coordinates required to plot radial variable axes
  axis <- NULL
  axis$path <- calculate.axis.path(var.names, grid.min + abs(centre.y), grid.max + abs(centre.y))
  # print(axis$path)
  # (d) Create file containing axis labels + associated plotting coordinates
  # Labels
  axis$label <- data.frame(
    text = axis.labels,
    x = NA,
    y = NA
  )
  # print(axis$label)
  # axis label coordinates
  n.vars <- length(var.names)
  angles <- seq(from = 0, to = 2 * pi, by = (2 * pi) / n.vars)
  axis$label$x <- sapply(1:n.vars, function(i, x) {
    ((grid.max + abs(centre.y)) * axis.label.offset) * sin(angles[i])
  })
  axis$label$y <- sapply(1:n.vars, function(i, x) {
    ((grid.max + abs(centre.y)) * axis.label.offset) * cos(angles[i])
  })
  # print(axis$label)
  # (e) Create Circular grid-lines + labels
  # caclulate the cooridinates required to plot circular grid-lines for three user-specified
  # y-axis values: min, mid and max [grid.min; grid.mid; grid.max]
  gridline <- NULL
  gridline$min$path <- circle.coordinates(c(0, 0), grid.min + abs(centre.y), n.points = 360)
  gridline$mid$path <- circle.coordinates(c(0, 0), grid.mid + abs(centre.y), n.points = 360)
  gridline$max$path <- circle.coordinates(c(0, 0), grid.max + abs(centre.y), n.points = 360)
  # print(head(gridline$max$path))
  # gridline labels
  gridline$min$label <- 
    data.frame(x = gridline.label.offset, 
               y = grid.min + abs(centre.y),
               text = as.character(grid.min))
  gridline$max$label <- 
    data.frame(x = gridline.label.offset,
               y = grid.max + abs(centre.y),
               text = as.character(grid.max))
  gridline$mid$label <- 
    data.frame(x = gridline.label.offset, 
               y = grid.mid + abs(centre.y),
               text = as.character(grid.mid))
  ### Start building up the radar plot
  
  # Declare 'theme.clear', with or without a plot legend as required by user
  # [default = no legend if only 1 group [path] being plotted]
  theme.clear <- 
    ggplot2::theme_bw(base_size = base.size) +
    ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank(),
                   legend.key = ggplot2::element_rect(linetype = 'blank')
    )
  
  if (plot.legend == FALSE) {
    legend.position <- 'none'
  }
  
  # Base-layer = axis labels + plot extent
  # [need to declare plot extent as well, since the axis labels don't always
  # fit within the plot area automatically calculated by ggplot, even if all
  # included in first plot; and in any case the strategy followed here is to first
  # plot right-justified labels for axis labels to left of Y axis for x< (-x.centre.range)],
  # then centred labels for axis labels almost immediately above/below x= 0
  # [abs(x) < x.centre.range]; then left-justified axis labels to right of Y axis [x>0].
  # This building up the plot in layers doesn't allow ggplot to correctly
  # identify plot extent when plotting first (base) layer]
  
  # base layer = axis labels for axes to left of central y-axis [x< -(x.centre.range)]
  plot.radar <- 
    ggplot2::ggplot(axis$label) +
    ggplot2::xlab(NULL) +
    ggplot2::ylab(NULL) +
    ggplot2::coord_equal() +
    ggplot2::geom_text(
      data = subset(axis$label, axis$label$x < (-x.centre.range)),
      aes(x = x, y = y, label = text), size = axis.label.size, hjust = 1, family = font.radar
    ) +
    ggplot2::scale_x_continuous(limits = c(-1.5 * plot.extent.x, 1.5 * plot.extent.x)) +
    ggplot2::scale_y_continuous(limits = c(-plot.extent.y, plot.extent.y)) + 
    # circular grid-lines at 'min', 'mid' and 'max' y-axis values
    ## min
    ggplot2::geom_path(data = gridline$min$path, aes(x = x, y = y),
                       lty = gridline.min.linetype, 
                       colour = gridline.min.colour, 
                       linewidth = grid.line.width) +
    ggplot2::geom_path(data = gridline$mid$path, 
                       aes(x = x, y = y),
                       lty = gridline.mid.linetype, 
                       colour = gridline.mid.colour,
                       linewidth = grid.line.width) +
    ## max
    ggplot2::geom_path(data = gridline$max$path, aes(x = x, y = y),
                       lty = gridline.max.linetype, 
                       colour = gridline.max.colour, 
                       linewidth = grid.line.width) + 
    # axis labels for any vertical axes [abs(x)<=x.centre.range]
    ggplot2::geom_text(data = subset(axis$label, abs(axis$label$x) <= x.centre.range),
                       aes(x = x, y = y, label = text), 
                       size = axis.label.size, 
                       hjust = 0.5, 
                       family = font.radar) + 
    # axis labels for any vertical axes [x>x.centre.range]
    ggplot2::geom_text(data = subset(axis$label, axis$label$x > x.centre.range),
                       aes(x = x, y = y, label = text), 
                       size = axis.label.size, 
                       hjust = 0, 
                       family = font.radar) + 
    # theme.clear [to remove grey plot background, grid lines, axis tick marks and axis text]
    theme.clear + 
    # background circle against which to plot radar data
    ggplot2::geom_polygon(data = gridline$max$path, 
                          aes(x, y),
                          fill = background.circle.colour,
                          alpha = background.circle.transparency) + 
    # radial axes
    ggplot2::geom_path(data = axis$path, 
                       aes(x = x, y = y, group = axis.no),
                       colour = axis.line.colour)
  
  name.group <- names(group$path[1])
  
  if (length(line.alpha) == 1) {
    plot.radar <- 
      plot.radar + 
      ggplot2::geom_path(data = group$path, 
                         aes(x = .data[['x']], y = .data[['y']], group = .data[[name.group]], colour = .data[[name.group]]), 
                         linewidth = group.line.width, 
                         alpha = line.alpha)
  } else {
    # Assuming line.alpha is a vector with the same length as the number of groups
    # This will apply different alpha values to each line
    plot.radar <- 
      plot.radar + 
      ggplot2::geom_path(data = group$path, 
                         aes(x = .data[['x']], y = .data[['y']], group = .data[[name.group]], colour = .data[[name.group]]), 
                         linewidth = group.line.width) +
      ggplot2::scale_alpha_manual(values = line.alpha)
  }
  
  # group points (cluster data)
  # Modify point drawing logic based on draw.points
  if (draw.points) {
    # Check if point.alpha is a vector or single value
    if (length(point.alpha) == 1) {
      plot.radar <- 
        plot.radar + 
        ggplot2::geom_point(data = group$path, 
                            aes(x = .data[['x']], y = .data[['y']], group = .data[[name.group]], colour = .data[[name.group]]), 
                            size = group.point.size, 
                            alpha = point.alpha)
    } else {
      # Assuming point.alpha is a vector with the same length as the number of groups
      # This will apply different alpha values to each group
      plot.radar <- 
        plot.radar + 
        ggplot2::geom_point(data = group$path, 
                            aes(x = .data[['x']], y = .data[['y']], group = .data[[name.group]], colour = .data[[name.group]]), 
                            size = group.point.size) +
        ggplot2::scale_alpha_manual(values = point.alpha)
    }
  }
  
  # group (cluster) fills
  if (fill == TRUE) {
    plot.radar <- 
      plot.radar + 
      ggplot2::geom_polygon(data = group$path, 
                            aes(x = .data[['x']], y = .data[['y']], group = .data[[name.group]], fill = .data[[name.group]]), 
                            alpha = fill.alpha)
  }
  
  
  # amend Legend title
  if (plot.legend == TRUE) {
    plot.radar <- 
      plot.radar + 
      ggplot2::labs(colour = legend.title, size = legend.text.size)
  }
  
  # grid-line labels (max; mid; min)
  if (label.gridline.min == TRUE) {
    plot.radar <- 
      plot.radar + 
      ggplot2::geom_text(data = gridline$min$label, 
                         aes(x = x, y = y, label = values.radar[1]), 
                         size = grid.label.size * 0.8, 
                         hjust = 1, 
                         family = font.radar)
  }
  if (label.gridline.mid == TRUE) {
    plot.radar <- 
      plot.radar + 
      ggplot2::geom_text(data = gridline$mid$label, 
                         aes(x = x, y = y, label = values.radar[2]), 
                         size = grid.label.size * 0.8, 
                         hjust = 1, 
                         family = font.radar)
  }
  if (label.gridline.max == TRUE) {
    plot.radar <- 
      plot.radar + 
      ggplot2::geom_text(data = gridline$max$label, 
                         aes(x = x, y = y, label = values.radar[3]), 
                         size = grid.label.size * 0.8, 
                         hjust = 1, 
                         family = font.radar)
  }
  # centre.y label if required [i.e. value of y at centre of plot circle]
  if (label.centre.y == TRUE) {
    centre.y.label <- data.frame(x = 0, y = 0, text = as.character(centre.y))
    plot.radar <- 
      plot.radar + 
      ggplot2::geom_text(data = centre.y.label, 
                         aes(x = x, y = y, label = text), 
                         size = grid.label.size, 
                         hjust = 0.5, 
                         family = font.radar)
  }
  
  if (!is.null(group.colours)) {
    colour.values <- rep(group.colours, length(unique(data.plot[, 1])) / length(group.colours))
  } else {
    colour.values <- generate.color.values(length(unique(data.plot[, 1])))
  }
  
  plot.radar <- 
    plot.radar +
    ggplot2::theme(legend.key.width = unit(3, 'line'),
                   text = ggplot2::element_text(size = 20, family = font.radar),
                   legend.text = element_text(size = legend.text.size), 
                   legend.position = legend.position,
                   legend.key.height = unit(2, 'line'),
                   legend.title = ggplot2::element_blank()) +
    ggplot2::scale_colour_manual(values = colour.values)
  
  
  if (isTRUE(fill)) {
    plot.radar <- 
      plot.radar +
      ggplot2::scale_fill_manual(values = colour.values, 
                                 guide = 'none')
  }
  
  if (legend.title != '') {
    plot.radar <- 
      plot.radar + 
      ggplot2::theme(legend.title = ggplot2::element_text())
  }
  
  if (plot.title != '') {
    plot.radar <- 
      plot.radar + 
      ggplot2::ggtitle(plot.title)
  }
  
  return(plot.radar)
}
