# wrapper functions
doBayes = function(dat, y, r){
  ############Design matrices
  I <- max(dat$site)
  # Indicator variables for the full model, using effect coding 
  xk <- ifelse(dat$gender == "Female", -1/2, 1/2) #effect of target gender
  xl <- ifelse(dat$emotion == "Sad", -1/2, 1/2) #effect of target emotion
  xm <- ifelse(dat$culture == "West", -1/2, 1/2) #effect of culture 
  xkl <- xk*xl*2 #gender-by-emotion interaction effect
  xkm <- xk*xm*2 #gender-by-culture interaction effect
  xlm <- xl*xm*2 #emotion-by-culture interaction effect
  xklm <- xk*xl*xm*4 #gender-by-emotion-by-culture threeway interaction effect
  
  # Indicator variables for the culture change model, using effect coding 
  xkl.1 <- ifelse(dat$gender=="Female"&dat$emotion=="Sad", -1/2, 1/2)
  xkl.1 <- ifelse(dat$gender=="Male", 0, xkl.1)
  xkl.2 <- ifelse(dat$gender=="Female"&dat$emotion=="Sad", 0, 2/3)
  xkl.2 <- ifelse(dat$gender=="Male", -1/3, xkl.2)
  
  # Random intercepts for all models 
  xrand <- matrix(0, nrow = nrow(dat), ncol = I)
  for(i in 1:nrow(dat)){
    xrand[i, dat$site[i]] <- 1
  }
  
  Xmat <- cbind(xk, xl, xkl, xrand) #design matrix for the gender stereotype and status signalling models including random site intercept
  XmatC <- cbind(xkl.1, xkl.2, xrand) #design matrix for the culture change model including random site intercept
  XmatF <- cbind(xk, xl, xm, xkl, xkm, xlm, xklm, xrand) #design matrix for the full cultural differences model 
  Xnull <- xrand #design matrix for null model with random participant effect
  
  
  
  #sample from the unconstrained model gender stereotype and status signalling model and get BF vs. package null model
  sampMain <- nWayAOV(y, Xmat
                      , gMap = c(0:2, rep(3, I)), rscale = c(r[1], r[1], r[2], r[3])
                      , posterior = T)
  BFMain <- nWayAOV(y, Xmat
                    , gMap = c(0:2, rep(3, I)), rscale = c(r[1], r[1], r[2], r[3])
                    , posterior = F)
  #sample from the unconstrained culture change  model and get BF vs. package null model
  sampCulture <- nWayAOV(y, XmatC
                         , gMap = c(0:1, rep(2, I)), rscale = c(r[1], r[1], r[3])
                         , posterior = T)
  BFCulture <- nWayAOV(y, XmatC
                       , gMap = c(0:1, rep(2, I)), rscale = c(r[1], r[1], r[3])
                       , posterior = F)
  #sample from the unconstrained cultural differences  model and get BF vs. package null model
  sampFull <- nWayAOV(y, XmatF
                      , gMap = c(0:6, rep(7, I)), rscale = c(rep(r[1], 7), r[3])
                      , posterior = T)
  BFFull <- nWayAOV(y, XmatF
                    , gMap = c(0:6, rep(7, I)), rscale = c(rep(r[1], 7), r[3])
                    , posterior = F)
  # get BF for the null model
  BFNull <- nWayAOV(y, Xnull
                    , gMap = rep(0, I), rscale = c(r[3])
                    , posterior = F)
  
  ## Posterior samples for the cell means
  # Calculate estimated cell means for each iteration from the main model samples
  sad   <- -1/2
  angry <- 1/2
  women <- -1/2
  men   <- 1/2
  west  <- -1/2
  east  <- 1/2
  
  musw <- sampMain[, 1] + women * sampMain[, 2] + sad * sampMain[, 3] + women * sad * 2 *sampMain[, 4] 
  muaw <- sampMain[, 1] + women * sampMain[, 2] + angry * sampMain[, 3] + women * angry * 2 * sampMain[, 4] 
  musm <- sampMain[, 1] + men * sampMain[, 2] + sad * sampMain[, 3] + men * sad * 2 * sampMain[, 4] 
  muam <- sampMain[, 1] + men * sampMain[, 2] + angry * sampMain[, 3] + men * angry * 2 * sampMain[, 4] 
  musm <- cbind(musw, muaw, musm, muam)
  # colMeans(mus)
  
  # Parameters from the culture change model 
  delta <- sampCulture[, 2]
  eta   <- sampCulture[, 3]
  postcc  <- cbind(delta,eta)
  
  # Calculate estimated cell means for each iteration from the full model samples
  musww <- sampFull[,1] + women*sampFull[,2] + sad*sampFull[,3] + west*sampFull[,4] + women*sad*2*sampFull[,5] + women*west*2*sampFull[,6] +
    sad*west*2*sampFull[,7] + women*sad*west*4*sampFull[,8]
  muswe <- sampFull[,1] + women*sampFull[,2] + sad*sampFull[,3] + east*sampFull[,4] + women*sad*2*sampFull[,5] + women*east*2*sampFull[,6] +
    sad*east*2*sampFull[,7] + women*sad*east*4*sampFull[,8]
  muaww <- sampFull[,1] + women*sampFull[,2] + angry*sampFull[,3] + west*sampFull[,4] + women*angry*2*sampFull[,5] + women*west*2*sampFull[,6] +
    angry*west*2*sampFull[,7] + women*angry*west*4*sampFull[,8]
  muawe <- sampFull[,1] + women*sampFull[,2] + angry*sampFull[,3] + east*sampFull[,4] + women*angry*2*sampFull[,5] + women*east*2*sampFull[,6] +
    angry*east*2*sampFull[,7] + women*angry*east*4*sampFull[,8]
  musmw <- sampFull[,1] + men*sampFull[,2] + sad*sampFull[,3] + west*sampFull[,4] + men*sad*2*sampFull[,5] + men*west*2*sampFull[,6] +
    sad*west*2*sampFull[,7] + men*sad*west*4*sampFull[,8]
  musme <- sampFull[,1] + men*sampFull[,2] + sad*sampFull[,3] + east*sampFull[,4] + men*sad*2*sampFull[,5] + men*east*2*sampFull[,6] +
    sad*east*2*sampFull[,7] + men*sad*east*4*sampFull[,8]
  muamw <- sampFull[,1] + men*sampFull[,2] + angry*sampFull[,3] + west*sampFull[,4] + men*angry*2*sampFull[,5] + men*west*2*sampFull[,6] +
    angry*west*2*sampFull[,7] + men*angry*west*4*sampFull[,8]
  muame <- sampFull[,1] + men*sampFull[,2] + angry*sampFull[,3] + east*sampFull[,4] + men*angry*2*sampFull[,5] + men*east*2*sampFull[,6] +
    angry*east*2*sampFull[,7] + men*angry*east*4*sampFull[,8]
  musf <- cbind(musww, muaww, musmw, muamw, muswe, muawe,  musme, muame)
  
  ## Priors 
  # Simple logic would be that there are 24 possible ordering with four cell means and the gender stereotype, the status signalling and culture change models all allow for 6 orderings, so prior prob is 1/4.
  # For the cultural differences model, we sample from the prior distributions to get the prior prob. 
  Mprior <- 1000000
  mu <- rnorm(Mprior, 0, 100)
  mu.k <- rcauchy(Mprior,0,r[1])
  mu.l <- rcauchy(Mprior,0,r[1])
  mu.m <- rcauchy(Mprior,0,r[1])
  mu.kl <- rcauchy(Mprior,0,r[2])
  mu.km <- rcauchy(Mprior,0,r[2])
  mu.lm <- rcauchy(Mprior,0,r[2])
  mu.klm <- rcauchy(Mprior,0,r[2])
  
  priormusww <- mu + women*mu.k + sad*mu.l + west*mu.m + women*sad*2*mu.kl + women*west*2*mu.km + sad*west*2*mu.lm + women*sad*west*4*mu.klm
  priormuswe <- mu + women*mu.k + sad*mu.l + east*mu.m + women*sad*2*mu.kl + women*east*2*mu.km + sad*east*2*mu.lm + women*sad*east*4*mu.klm
  priormuaww <- mu + women*mu.k + angry*mu.l + west*mu.m + women*angry*2*mu.kl + women*west*2*mu.km + angry*west*2*mu.lm + women*angry*west*4*mu.klm
  priormuawe <- mu + women*mu.k + angry*mu.l + east*mu.m + women*angry*2*mu.kl + women*east*2*mu.km + angry*east*2*mu.lm + women*angry*east*4*mu.klm
  priormusmw <- mu + men*mu.k + sad*mu.l + west*mu.m + men*sad*2*mu.kl + men*west*2*mu.km + sad*west*2*mu.lm + men*sad*west*4*mu.klm
  priormusme <- mu + men*mu.k + sad*mu.l + east*mu.m + men*sad*2*mu.kl + men*east*2*mu.km + sad*east*2*mu.lm + men*sad*east*4*mu.klm
  priormuamw <- mu + men*mu.k + angry*mu.l + west*mu.m + men*angry*2*mu.kl + men*west*2*mu.km + angry*west*2*mu.lm + men*angry*west*4*mu.klm
  priormuame <- mu + men*mu.k + angry*mu.l + east*mu.m + men*angry*2*mu.kl + men*east*2*mu.km + angry*east*2*mu.lm + men*angry*east*4*mu.klm
  priormusf <- cbind(priormusww, priormuswe, priormuaww, priormuawe, priormusmw, priormusme, priormuamw, priormuame)
  
  ## Predicted ordering for gender stereotyping model
  stereotype <- function(x){
    t1 <- x[1] > x[2]
    t2 <- x[3] < x[4]
    t1 & t2
  }
  ## Predicted ordering for status signalling model
  signalling <- function(x){
    t1 <- x[1] < x[2]
    t2 <- x[3] < x[4]
    t1 & t2
  }
  change <- function(x){
    t1 <- x[1] > 0 
    t2 <- x[2] > 0
    t1 & t2
  }
  differences <- function(x){
    t1 <- x[1] < x[2]
    t2 <- x[3] < x[4]
    t3 <- x[5] > x[6]
    t4 <- x[7] > x[8]
    t1 & t2 & t3 & t4
  }
  # Prior and Posterior Probability of the predicted ordering for the gender stereotype model
  out <- apply(musm, 1, stereotype)
  postprob.gs <- mean(out)
  #out <- apply(priormusm, 1, stereotype)
  priorprob.gs <- 1/4
  BF_rgs <- postprob.gs/priorprob.gs
  ## Prior and Posterior Probability of the predicted ordering for the status signalling model
  out <- apply(musm, 1, signalling)
  postprob.ss <- mean(out)
  #out <- apply(priormusm, 1, signalling)
  priorprob.ss <- 1/4
  BF_rss <- postprob.ss/priorprob.ss
  ## Prior and Posterior Probability of the predicted ordering for the culture change model
  out <- apply(postcc, 1, change)
  postprob.cc <- mean(out)
  #out <- apply(priorcc, 1, change)
  priorprob.cc <- 1/4
  BF_rcc <- postprob.cc/priorprob.cc
  ## Prior and Posterior Probability of the predicted ordering for the cultural differences model
  out <- apply(musf, 1, differences)
  postprob.cd <- mean(out)
  out <- apply(priormusf, 1, differences)
  priorprob.cd <- mean(out)
  BF_rcd <- postprob.cd/priorprob.cd
  
  ## All Bayes factors as compared to unconstrained
  BF_0u <- 1/exp(BFFull$bf - BFNull$bf)
  BF_gsu <- BF_rgs * exp(BFMain$bf - BFFull$bf)
  BF_ssu <- BF_rss * exp(BFMain$bf - BFFull$bf)
  BF_ccu <- BF_rcc * exp(BFCulture$bf - BFFull$bf)
  BF_cdu <- BF_rcd
  bfsu <- c(BF_0u, BF_gsu, BF_ssu, BF_ccu, BF_cdu)
  modnames <- c("Null vs. Unconstrained", "Gender Stereotyping vs. Unconstrained", "Status Signalling vs. Unconstrained", "Culture Change vs. Unconstrained", "Cultural Differences vs. Unconstrained")
  names(bfsu) <- modnames
  
  ## All Bayes factors as compared to null
  BF_u0 <- exp(BFFull$bf - BFNull$bf)
  BF_gs0 <- BF_rgs * exp(BFMain$bf - BFNull$bf)
  BF_ss0 <- BF_rss * exp(BFMain$bf - BFNull$bf)
  BF_cc0 <- BF_rcc * exp(BFCulture$bf - BFNull$bf)
  BF_cd0 <- BF_rcd * exp(BFFull$bf - BFNull$bf)
  bfs <- c(BF_gs0, BF_ss0, BF_cc0, BF_cd0,BF_u0)
  bfs0 <- sapply(bfs, function(x) 1/x)
  modnames0 <- c("Null vs. Gender Stereotyping", "Null vs. Status Signalling", "Null vs. Culture Change", "Null vs. Cultural Differences","Null vs. Unconstrained")
  names(bfs0) <- modnames0
  
  return(list(bfsu=bfsu, bfs0=bfs0, sampFull=sampFull, musm = musm))
}

doRandom = function(dat, y, r){
  ############Design matrices
  site <- dat$site
  I <- max(site)
  # Indicator variables for the full model, using effect coding 
  xk <- ifelse(dat$gender == "Female", -1/2, 1/2) #effect of target gender
  xl <- ifelse(dat$emotion == "Sad", -1/2, 1/2) #effect of target emotion
  xkl <- xk*xl*2 #gender-by-emotion interaction effect
  
  # Random intercepts for all models 
  xrand <- xkrand <- xlrand <- xklrand <- matrix(0, nrow = nrow(dat), ncol = I)
  for(i in 1:nrow(dat)){
    xrand[i, site[i]] <- 1
    xkrand[i, site[i]] <- xk[i]
    xlrand[i, site[i]] <- xl[i]
    xklrand[i, site[i]] <- xkl[i]
  }
  
  Xmat <- cbind(xkrand, xlrand, xklrand, xk, xl, xkl, xrand) #design matrix for the gender stereotype and status signalling models including random site intercept

  #sample from the unconstrained model gender stereotype and status signalling model and get BF
  samp <- nWayAOV(y, Xmat
                  , gMap = c(rep(0:2, each=I), 3:5, rep(6, I)), rscale = c(r[4], r[4], r[4], r[1], r[1], r[2], r[3])
                  , posterior = T)
  BF <- nWayAOV(y, Xmat
                 , gMap = c(rep(0:2, each=I), 3:5, rep(6, I)), rscale = c(r[4], r[4], r[4], r[1], r[1], r[2], r[3])
                 , posterior = F)
  # get BF for the null model
  BFNull <- nWayAOV(y, xrand
                    , gMap = rep(0, I), rscale = c(r[3])
                    , posterior = F)
  
  ## Posterior samples for the cell means
  xk.i <- 1:I
  xl.i <- (I+1):(2*I)
  xkl.i <- (2*I+1):(3*I)
  xk0.i <- 3*I+1
  xl0.i <- 3*I+2
  xkl0.i <- 3*I+3
  xrand.i <- (3*I+4):(4*I+3)
  
  # Calculate estimated cell means for each iteration from the main model samples
  sad   <- -1/2
  angry <- 1/2
  women <- -1/2
  men   <- 1/2
  
  rand <- samp[, xrand.i+1] + samp[, 1]
  gen  <- samp[, xk.i+1] + samp[, xk0.i+1]
  emo  <- samp[, xl.i+1] + samp[, xl0.i+1]
  int  <- samp[, xkl.i+1] + samp[, xkl0.i+1]
  
  musw <- rand + women * gen + sad * emo + women * sad * 2 * int
  muaw <- rand + women * gen + angry * emo + women * angry * 2 * int
  musm <- rand + men * gen + sad * emo + men * sad * 2 * int
  muam <- rand + men * gen + angry * emo + men * angry * 2 * int
  musm <- list(musw, muaw, musm, muam)
  # colMeans(mus)
  
  BF_0r <- exp(BFNull$bf - BF$bf)
  
  return(list(BF_0r=BF_0r, samp=samp, musm = musm,
              alphas = rand, betas = gen, 
              gammas = emo, thetas = int))
}

doBayesModerators = function(dat, y, r, mod, pos=T){
  ############Design matrices
  I <- max(dat$site)
  # Indicator variables for the full model, using effect coding 
  xk <- ifelse(dat$gender == "Female", -1/2, 1/2) #effect of target gender
  xl <- ifelse(dat$emotion == "Sad", -1/2, 1/2) #effect of target emotion
  if(is.factor(mod)) xm <- (as.numeric(mod)-1.5)*-1 #code -1/2 = no, 1/2 = yes
  if(!is.factor(mod)) xm <- scale(mod,center = T, scale = T)[,1] #effect of moderator (scaled) 
  xkl <- xk*xl*2 #gender-by-emotion interaction effect
  xkm <- xk*xm #gender-by-moderator interaction effect
  xklm <- xk*xl*xm*2 #gender-by-emotion-by-moderator threeway interaction effect
  
  # Random intercepts for all models 
  xrand <- matrix(0, nrow = nrow(dat), ncol = I)
  for(i in 1:nrow(dat)){
    xrand[i, dat$site[i]] <- 1
  }
  
  XmatM <- cbind(xk, xl, xm, xkl, xrand) #design matrix for the main effects without moincluding random site intercept
  XmatF <- cbind(xk, xl, xm, xkl, xkm, xklm, xrand) #design matrix for the full cultural differences model
  XmatB <- cbind(xm, xrand) #only moderator and random intercepts
  Xnull <- xrand #design matrix for null model with random intercepts
  
  #sample from the unconstrained model gender stereotype and status signalling model and get BF vs. package null model
  sampMain <- nWayAOV(y, XmatM
                      , gMap = c(0:3, rep(4, I)), rscale = c(r[1], r[1], r[1], r[2], r[3])
                      , posterior = T)
  BFMain <- nWayAOV(y, XmatM
                    , gMap = c(0:3, rep(4, I)), rscale = c(r[1], r[1], r[1], r[2], r[3])
                    , posterior = F)
  #sample from the unconstrained culture change  model and get BF vs. package null model
  sampBaseline <- nWayAOV(y, XmatB
                         , gMap = c(0, rep(1, I)), rscale = c(r[1], r[3])
                         , posterior = T)
  BFBaseline <- nWayAOV(y, XmatB
                       , gMap = c(0, rep(1, I)), rscale = c(r[1], r[3])
                       , posterior = F)
  #sample from the unconstrained cultural differences  model and get BF vs. package null model
  sampFull <- nWayAOV(y, XmatF
                      , gMap = c(0:5, rep(6, I)), rscale = c(rep(r[1], 6), r[3])
                      , posterior = T)
  BFFull <- nWayAOV(y, XmatF
                    , gMap = c(0:5, rep(6, I)), rscale = c(rep(r[1], 6), r[3])
                    , posterior = F)
  # get BF for the null model
  BFNull <- nWayAOV(y, Xnull
                    , gMap = rep(0, I), rscale = c(r[3])
                    , posterior = F)
  
  ## Moderator predictions for gender-by-moderator effects 
  
  # Interaction parameter 
  gamma <- sampFull[, 6]
  # prediction is that higher scores on the moderator are related to more status conferral to women relative to men, so a negative interaction term (right?)
  if(pos) postprob <- mean(gamma > 0) 
  if(!pos) postprob <- mean(gamma < 0) 
  BF_r <- postprob/0.5

  ## All Bayes factors as compared to unconstrained
  BF_0u <- 1/exp(BFFull$bf - BFNull$bf)
  BF_mu <- exp(BFMain$bf - BFFull$bf)
  BF_iu <- exp(BFBaseline$bf - BFFull$bf)
  BF_ru <- BF_r
  bfsu <- c(BF_0u, BF_iu, BF_mu, BF_ru)
  modnames <- c("Null vs. Full", "Individual Differences vs. Full", "Experimental Effects vs. Full", "Moderator Interaction vs. Full")
  names(bfsu) <- modnames
  
  ## All Bayes factors as compared to null
  BF_0u <- exp(BFNull$bf - BFFull$bf)
  BF_mu <- exp(BFNull$bf - BFBaseline$bf)
  BF_iu <- exp(BFNull$bf - BFMain$bf)
  BF_ru <- BF_r
  bfs0 <- c(BF_0u, BF_iu, BF_mu, BF_ru)
  modnames0 <- c("Null vs. Full", "Null vs. Individual Differences", "Null vs. Experimental Effects", "Null vs. Moderator Interaction")
  names(bfs0) <- modnames0
  
  ## All Bayes factors as compared to Individual differences 
  BF_0i <- exp(BFNull$bf - BFBaseline$bf)
  BF_mi <- exp(BFMain$bf - BFBaseline$bf)
  BF_ri <- BF_r * exp(BFFull$bf - BFBaseline$bf)
  BF_fi <- exp(BFFull$bf - BFBaseline$bf)
  bfs <- c(BF_0i, BF_mi, BF_ri, BF_fi)
  bfsID <- sapply(bfs, function(x) 1/x)
  modnamesID <- c("Individual Differences vs. Null", "Individual Differences vs. Experimental Effects", "Individual Differences vs. Moderator Interaction", "Individual Differences vs. Full")
  names(bfsID) <- modnamesID
  
  return(list(bfsu=bfsu, bfs0=bfs0, bfsID = bfsID, 
              sampFull=sampFull, gamma=gamma))
}

plotter <- function(samp, range, countries, colors, main, labs, 
                    leg=NULL, cols=NULL, leg.pos=c(-1.5,27), 
                    order=NULL, ratings=NULL, effectcolors=FALSE,noylab=FALSE){
  m <- apply(samp, 2, mean)
  q <- apply(samp, 2, quantile, c(.025,.975))
  #cols <- RColorBrewer::brewer.pal(9, "Set1")
  ylabs="Material Set"
  if(noylab){ylabs=""}
  mar_1 <- ifelse(noylab, 6, 4)
  
  fullx <- pretty(range)
  atx <- c(labs[1], fullx[2:(length(fullx)-1)], labs[2])
  if(labs[2]=="Stereotype") atx <- c(labs[1], "", fullx[3:(length(fullx)-1)], labs[2])
  J=length(m)  
  o=order(m)
  if(!is.null(order)){o=order}

  sites <- as.numeric(as.factor(countries))
  
  if(effectcolors){
    exclude0 <- apply(q, 2, function(x) x[1]>0|x[2]<0)
    colors <- ifelse(exclude0, "firebrick","grey36")
  }

  par(las=1, font.main= 1
      , cex.axis = 0.8
      , cex.main = 1
      , mar=c(2, mar_1, 2, 1) + 0.2)
  plot(m[o], 1:J, xlim = range, ylim = c(0,J), axes=F, xlab="", ylab=ylabs, type = "n")
  abline(v=0, lty=2)
  arrows(q[1,][o],1:J,q[2,][o],1:J
         ,code=3,angle=90,length=.05
         ,col=colors[sites[o]])
  points(m[o],1:J
         , pch=19
         , col=colors[sites[o]]
         ,cex=1.1)
  axis(1,at=pretty(range), labels = FALSE
       ,padj=-1)
  axis(2,at=c(1:J), labels = countries[o])
  
  text(x = pretty(range),
       y = par("usr")[3] - 2,
       labels = atx,
       xpd = NA,
       cex = 0.8)
  
  title(main = main, font=1, line = 1)
  if(!is.null(leg)){
    legend(x=leg.pos[1],y=leg.pos[2], legend = leg, col = unique(colors), pch = 19,
           bty = "n", cex=0.8)
  }
  if(!is.null(ratings)){
    par(new = TRUE)
    plot(ratings[o], 1:J, axes = FALSE, xlim = c(30,90), ylim = c(0,J), 
         pch=4, ylab="")
    axis(3, at = pretty(c(30,90)), padj = 1)
  }
}

myformat = function(bfs){
  doformat = function(bf){
    if(bf > 99999) {bf <- sfsmisc::pretty10exp(bf, digits = 2, lab.type = "latex", lab.sep = "times")}
    else if (bf > 100) {bf <- printnum(bf, digits=0)} 
    else if (bf > 10) {bf <- printnum(bf, digits=1)}
    else {bf <- printnum(bf, digits=2)}
    return(bf)
  }
  bfs <- sapply(bfs, doformat)
  return(bfs)
}

#wrapper functions study 2
doBayes2 = function(dat, y, r){
  ############Design matrices
  I <- max(dat$site)
  J <- max(dat$design)
  # Indicator variables for the full model, using effect coding 
  xk <- ifelse(dat$gender == "female", -1/2, 1/2) #effect of target gender
  xl <- ifelse(dat$emotion == "neutral", -1/2, 1/2) #effect of target emotion
  xkl <- xk*xl*2 #gender-by-emotion interaction effect
  
  # Indicator variables for the culture change model, using effect coding 
  xkl.1 <- ifelse(dat$gender=="female"&dat$emotion=="neutral", -1/2, 1/2)
  xkl.1 <- ifelse(dat$gender=="male", 0, xkl.1)
  xkl.2 <- ifelse(dat$gender=="female"&dat$emotion=="neutral", 0, 2/3)
  xkl.2 <- ifelse(dat$gender=="male", -1/3, xkl.2)
  
  # Random intercepts for all models (for lab and for design)
  xrand <- matrix(0, nrow = nrow(dat), ncol = I)
  xdes  <- matrix(0, nrow = nrow(dat), ncol = J)
  for(i in 1:nrow(dat)){
    xrand[i, dat$site[i]] <- 1
  }
  for(j in 1:nrow(dat)){
    xdes[j, dat$design[j]] <- 1
  }
  
  Xmat <- cbind(xk, xl, xkl, xrand, xdes) #design matrix for the gender stereotype and status signalling models including random site intercept
  XmatC <- cbind(xkl.1, xkl.2, xrand, xdes) #design matrix for the culture change model including random site intercept
  Xbase <- cbind(xrand,xdes)
  Xnull <- xrand #design matrix for null model with random participant effect
  
  #sample from the unconstrained model gender stereotype and status signalling model and get BF vs. package null model
  sampMain <- nWayAOV(y, Xmat
                      , gMap = c(0:2, rep(3, I), rep(4, J)), rscale = c(r[1], r[1], r[2], r[3], r[3])
                      , posterior = T)
  BFMain <- nWayAOV(y, Xmat
                    , gMap = c(0:2, rep(3, I), rep(4, J)), rscale = c(r[1], r[1], r[2], r[3], r[3])
                    , posterior = F)
  #sample from the unconstrained culture change  model and get BF vs. package null model
  sampCulture <- nWayAOV(y, XmatC
                         , gMap = c(0:1, rep(2, I), rep(3, J)), rscale = c(r[1], r[1], r[3], r[3])
                         , posterior = T)
  BFCulture <- nWayAOV(y, XmatC
                       , gMap = c(0:1, rep(2, I), rep(3, J)), rscale = c(r[1], r[1], r[3], r[3])
                       , posterior = F)
  # get BF for baseline model
  BFBase <- nWayAOV(y, Xbase
                    , gMap = c(rep(0, I), rep(1,J)), rscale = c(r[3],r[3])
                    , posterior = F)
  # get BF for the null model
  BFNull <- nWayAOV(y, Xnull
                    , gMap = rep(0, I), rscale = c(r[3])
                    , posterior = F)
  
  ## Posterior samples for the cell means
  # Calculate estimated cell means for each iteration from the main model samples
  sad   <- -1/2
  angry <- 1/2
  women <- -1/2
  men   <- 1/2
  
  musw <- sampMain[, 1] + women * sampMain[, 2] + sad * sampMain[, 3] + women * sad * 2 *sampMain[, 4] 
  muaw <- sampMain[, 1] + women * sampMain[, 2] + angry * sampMain[, 3] + women * angry * 2 * sampMain[, 4] 
  musm <- sampMain[, 1] + men * sampMain[, 2] + sad * sampMain[, 3] + men * sad * 2 * sampMain[, 4] 
  muam <- sampMain[, 1] + men * sampMain[, 2] + angry * sampMain[, 3] + men * angry * 2 * sampMain[, 4] 
  musm <- cbind(musw, muaw, musm, muam)
  # colMeans(mus)
  
  # Parameters from the culture change model 
  delta <- sampCulture[, 2]
  eta   <- sampCulture[, 3]
  postcc  <- cbind(delta,eta)
  
  ## Priors 
  # Simple logic would be that there are 24 possible ordering with four cell means and the gender stereotype, the status signalling and culture change models all allow for 6 orderings, so prior prob is 1/4.
  # For the cultural differences model, we sample from the prior distributions to get the prior prob. 
  Mprior <- 1000000
  mu <- rnorm(Mprior, 0, 100)
  mu.k <- rcauchy(Mprior,0,r[1])
  mu.l <- rcauchy(Mprior,0,r[1])
  mu.kl <- rcauchy(Mprior,0,r[2])
  
  ## Predicted ordering for gender stereotyping model
  stereotype <- function(x){
    t1 <- x[1] > x[2]
    t2 <- x[3] < x[4]
    t1 & t2
  }
  ## Predicted ordering for status signalling model
  signalling <- function(x){
    t1 <- x[1] < x[2]
    t2 <- x[3] < x[4]
    t1 & t2
  }
  change <- function(x){
    t1 <- x[1] > 0 
    t2 <- x[2] > 0
    t1 & t2
  }
  warmth <- function(x){
    t1 <- x[1] > x[2]
    t2 <- x[3] > x[4]
    t1 & t2
  }
  
  # Prior and Posterior Probability of the predicted ordering for the gender stereotype model
  out <- apply(musm, 1, stereotype)
  postprob.gs <- mean(out)
  #out <- apply(priormusm, 1, stereotype)
  priorprob.gs <- 1/4
  BF_rgs <- postprob.gs/priorprob.gs
  ## Prior and Posterior Probability of the predicted ordering for the status signalling model
  out <- apply(musm, 1, signalling)
  postprob.ss <- mean(out)
  #out <- apply(priormusm, 1, signalling)
  priorprob.ss <- 1/4
  BF_rss <- postprob.ss/priorprob.ss
  ## Prior and Posterior Probability of the predicted ordering for the culture change model
  out <- apply(postcc, 1, change)
  postprob.cc <- mean(out)
  ## Prior and Posterior Probability of the predicted ordering for the anger suppresses warmth model
  out <- apply(musm, 1, warmth)
  postprob.wa <- mean(out)
  priorprob.wa <- 1/4
  BF_rwa <- postprob.wa/priorprob.wa
  
  priorprob.cc <- 1/4
  BF_rcc <- postprob.cc/priorprob.cc
  
  ## All Bayes factors as compared to unconstrained
  BF_0u <- 1/exp(BFMain$bf - BFNull$bf)
  BF_bu <- 1/exp(BFMain$bf - BFBase$bf)
  BF_gsu <- BF_rgs
  BF_ssu <- BF_rss
  BF_ccu <- BF_rcc * exp(BFCulture$bf - BFMain$bf)
  BF_wau <- BF_rwa
  bfsu <- c(BF_0u, BF_bu, BF_gsu, BF_ssu, BF_ccu, BF_wau)
  modnames <- c("Null vs. Unconstrained", "Baseline vs. Unconstrained", "Gender Stereotyping vs. Unconstrained", "Status Signalling vs. Unconstrained", "Culture Change vs. Unconstrained","Warmth vs. Unconstrained")
  names(bfsu) <- modnames
  
  ## All Bayes factors as compared to null
  BF_u0 <- exp(BFMain$bf - BFNull$bf)
  BF_b0 <- exp(BFBase$bf - BFNull$bf)
  BF_gs0 <- BF_rgs * exp(BFMain$bf - BFNull$bf)
  BF_ss0 <- BF_rss * exp(BFMain$bf - BFNull$bf)
  BF_cc0 <- BF_rcc * exp(BFCulture$bf - BFNull$bf)
  bfs <- c(BF_gs0, BF_b0, BF_ss0, BF_cc0, BF_u0)
  bfs0 <- sapply(bfs, function(x) 1/x)
  modnames0 <- c("Null vs. Gender Stereotyping", "Null vs. Baseline", "Null vs. Status Signalling", "Null vs. Culture Change","Null vs. Unconstrained")
  names(bfs0) <- modnames0
  
  ## All Bayes factors as compared to the baseline model
  BF_ub <- exp(BFMain$bf - BFBase$bf)
  BF_0b <- exp(BFNull$bf - BFBase$bf)
  BF_gsb <- BF_rgs * exp(BFMain$bf - BFBase$bf)
  BF_ssb <- BF_rss * exp(BFMain$bf - BFBase$bf)
  BF_ccb <- BF_rcc * exp(BFCulture$bf - BFBase$bf)
  BF_wab <- BF_rwa * exp(BFMain$bf - BFBase$bf)
  bfsb <- c(BF_gsb, BF_ssb, BF_ccb, BF_ub, BF_0b, BF_wab)
  modnamesb <- c("Gender Stereotyping vs. Baseline", "Satus Signalling vs. Baseline", "Culture Change vs. Baseline", "Unconstrained vs. Baseline",
                 "Null vs. Baseline", "Warmth vs. Baseline")
  names(bfsb) <- modnamesb
  
  return(list(bfsu=bfsu, bfs0=bfs0, bfsb=bfsb, sampFull=sampMain, musm = musm))
}

doRandom2 = function(dat, y, r){
  ############Design matrices
  design <- dat$design
  I <- max(design)
  # Indicator variables for the full model, using effect coding 
  xk <- ifelse(dat$gender == "female", -1/2, 1/2) #effect of target gender
  xl <- ifelse(dat$emotion == "neutral", -1/2, 1/2) #effect of target emotion
  xkl <- xk*xl*2 #gender-by-emotion interaction effect
  
  # Random intercepts for all models 
  xrand <- xkrand <- xlrand <- xklrand <- matrix(0, nrow = nrow(dat), ncol = I)
  for(i in 1:nrow(dat)){
    xrand[i, design[i]] <- 1
    xkrand[i, design[i]] <- xk[i]
    xlrand[i, design[i]] <- xl[i]
    xklrand[i, design[i]] <- xkl[i]
  }
  
  Xmat <- cbind(xkrand, xlrand, xklrand, xk, xl, xkl, xrand) #design matrix for the gender stereotype and status signalling models including random site intercept
  
  #sample from the unconstrained model gender stereotype and status signalling model and get BF
  samp <- nWayAOV(y, Xmat
                  , gMap = c(rep(0:2, each=I), 3:5, rep(6, I)), rscale = c(r[4], r[4], r[4], r[1], r[1], r[2], r[3])
                  , posterior = T)
  BF <- nWayAOV(y, Xmat
                , gMap = c(rep(0:2, each=I), 3:5, rep(6, I)), rscale = c(r[4], r[4], r[4], r[1], r[1], r[2], r[3])
                , posterior = F)
  # get BF for the null model
  BFNull <- nWayAOV(y, xrand
                    , gMap = rep(0, I), rscale = c(r[3])
                    , posterior = F)
  
  ## Posterior samples for the cell means
  xk.i <- 1:I
  xl.i <- (I+1):(2*I)
  xkl.i <- (2*I+1):(3*I)
  xk0.i <- 3*I+1
  xl0.i <- 3*I+2
  xkl0.i <- 3*I+3
  xrand.i <- (3*I+4):(4*I+3)
  
  # Calculate estimated cell means for each iteration from the main model samples
  neutral   <- -1/2
  angry <- 1/2
  women <- -1/2
  men   <- 1/2
  
  rand <- samp[, xrand.i+1] + samp[, 1]
  gen  <- samp[, xk.i+1] + samp[, xk0.i+1]
  emo  <- samp[, xl.i+1] + samp[, xl0.i+1]
  int  <- samp[, xkl.i+1] + samp[, xkl0.i+1]
  
  musw <- rand + women * gen + neutral * emo + women * neutral * 2 * int
  muaw <- rand + women * gen + angry * emo + women * angry * 2 * int
  musm <- rand + men * gen + neutral * emo + men * neutral * 2 * int
  muam <- rand + men * gen + angry * emo + men * angry * 2 * int
  musm <- list(musw, muaw, musm, muam)
  # colMeans(mus)
  
  BF_0r <- exp(BFNull$bf - BF$bf)
  
  return(list(BF_0r=BF_0r, samp=samp, musm = musm,
              alphas = rand, betas = gen, 
              gammas = emo, thetas = int))
}

doBayesModerators2 = function(dat, y, r, mod, pos=T){
  ############Design matrices
  I <- max(dat$site)
  J <- max(dat$design)
  # Indicator variables for the full model, using effect coding 
  xk <- ifelse(dat$gender == "female", -1/2, 1/2) #effect of target gender
  xl <- ifelse(dat$emotion == "neutral", -1/2, 1/2) #effect of target emotion
  if(is.factor(mod)) xm <- (as.numeric(mod)-1.5)*-1 #code -1/2 = no, 1/2 = yes
  if(!is.factor(mod)) xm <- scale(mod,center = T, scale = T)[,1] #effect of moderator (scaled) 
  xkl <- xk*xl*2 #gender-by-emotion interaction effect
  xkm <- xk*xm #gender-by-moderator interaction effect
  xklm <- xk*xl*xm*2 #gender-by-emotion-by-moderator threeway interaction effect
  
  # Random intercepts for all models (for lab and for design)
  xrand <- matrix(0, nrow = nrow(dat), ncol = I)
  xdes  <- matrix(0, nrow = nrow(dat), ncol = J)
  for(i in 1:nrow(dat)){
    xrand[i, dat$site[i]] <- 1
  }
  for(j in 1:nrow(dat)){
    xdes[j, dat$design[j]] <- 1
  }
  
  XmatM <- cbind(xk, xl, xm, xkl, xrand, xdes) #design matrix for the main effects without moincluding random site intercept
  XmatF <- cbind(xk, xl, xm, xkl, xkm, xklm, xrand, xdes) #design matrix for the full moderator model
  XmatB <- cbind(xm, xrand, xdes) #only moderator and random intercepts
  Xnull <- cbind(xrand, xdes) #design matrix for null model with random intercepts
  
  #sample from the unconstrained model gender stereotype and status signalling model and get BF vs. package null model
  sampMain <- nWayAOV(y, XmatM
                      , gMap = c(0:3, rep(4, I), rep(5, J)), rscale = c(r[1], r[1], r[1], r[2], r[3], r[3])
                      , posterior = T)
  BFMain <- nWayAOV(y, XmatM
                    , gMap = c(0:3, rep(4, I), rep(5, J)), rscale = c(r[1], r[1], r[1], r[2], r[3], r[3])
                    , posterior = F)
  #sample from the unconstrained culture change  model and get BF vs. package null model
  sampBaseline <- nWayAOV(y, XmatB
                          , gMap = c(0, rep(1, I), rep(2, J)), rscale = c(r[1], r[3], r[3])
                          , posterior = T)
  BFBaseline <- nWayAOV(y, XmatB
                        , gMap = c(0, rep(1, I), rep(2, J)), rscale = c(r[1], r[3], r[3])
                        , posterior = F)
  #sample from the unconstrained moderator interaction  model and get BF vs. package null model
  sampFull <- nWayAOV(y, XmatF
                      , gMap = c(0:5, rep(6, I), rep(7, J)), rscale = c(rep(r[1], 6), r[3], r[3])
                      , posterior = T)
  BFFull <- nWayAOV(y, XmatF
                    , gMap = c(0:5, rep(6, I), rep(7, J)), rscale = c(rep(r[1], 6), r[3], r[3])
                    , posterior = F)
  # get BF for the null model
  BFNull <- nWayAOV(y, Xnull
                    , gMap = c(rep(0, I), rep(1, J)), rscale = c(r[3],r[3])
                    , posterior = F)
  
  ## Moderator predictions for gender-by-moderator effects 
  
  # Interaction parameter 
  gamma <- sampFull[, 6]
  # prediction is that higher scores on the moderator are related to more status conferral to women relative to men, so a negative interaction term (right?)
  if(pos) postprob <- mean(gamma > 0) 
  if(!pos) postprob <- mean(gamma < 0) 
  BF_r <- postprob/0.5
  
  ## All Bayes factors as compared to unconstrained
  BF_0u <- 1/exp(BFFull$bf - BFNull$bf)
  BF_mu <- exp(BFMain$bf - BFFull$bf)
  BF_iu <- exp(BFBaseline$bf - BFFull$bf)
  BF_ru <- BF_r
  bfsu <- c(BF_0u, BF_iu, BF_mu, BF_ru)
  modnames <- c("Null vs. Full", "Individual Differences vs. Full", "Experimental Effects vs. Full", "Moderator Interaction vs. Full")
  names(bfsu) <- modnames
  
  ## All Bayes factors as compared to null
  BF_0u <- exp(BFNull$bf - BFFull$bf)
  BF_mu <- exp(BFNull$bf - BFBaseline$bf)
  BF_iu <- exp(BFNull$bf - BFMain$bf)
  BF_ru <- BF_r
  bfs0 <- c(BF_0u, BF_iu, BF_mu, BF_ru)
  modnames0 <- c("Null vs. Full", "Null vs. Individual Differences", "Null vs. Experimental Effects", "Null vs. Moderator Interaction")
  names(bfs0) <- modnames0
  
  ## All Bayes factors as compared to Individual differences 
  BF_0i <- exp(BFNull$bf - BFBaseline$bf)
  BF_mi <- exp(BFMain$bf - BFBaseline$bf)
  BF_ri <- BF_r * exp(BFFull$bf - BFBaseline$bf)
  BF_fi <- exp(BFFull$bf - BFBaseline$bf)
  bfs <- c(BF_0i, BF_mi, BF_ri, BF_fi)
  bfsID <- sapply(bfs, function(x) 1/x)
  modnamesID <- c("Individual Differences vs. Null", "Individual Differences vs. Experimental Effects", "Individual Differences vs. Moderator Interaction", "Individual Differences vs. Full")
  names(bfsID) <- modnamesID
  
  return(list(bfsu=bfsu, bfs0=bfs0, bfsID = bfsID, 
              sampFull=sampFull, gamma=gamma))
}

doBayesCultural = function(dat, y, r, mod, pos=T){
  ############Design matrices
  I <- max(dat$site)
  J <- max(dat$design)
  # Indicator variables for the full model, using effect coding (only emotion is relevant)
  xl <- ifelse(dat$emotion == "neutral", -1/2, 1/2) #effect of target emotion
  xm <- scale(mod, center = T, scale = T)[,1] #effect of cultural dimension (scaled) 
  xlm <- xl*xm #emotion-by-harmony interaction effect
  
  # Random intercepts for all models (for lab and for design)
  xrand <- matrix(0, nrow = nrow(dat), ncol = I)
  xdes  <- matrix(0, nrow = nrow(dat), ncol = J)
  for(i in 1:nrow(dat)){
    xrand[i, dat$site[i]] <- 1
  }
  for(j in 1:nrow(dat)){
    xdes[j, dat$design[j]] <- 1
  }
  
  XmatM <- cbind(xl, xm, xrand, xdes) #design matrix for the main effects without interaction including random site intercept
  XmatF <- cbind(xl, xm, xlm, xrand, xdes) #design matrix for the full moderator model
  XmatB <- cbind(xm, xrand, xdes) #only moderator and random intercepts
  Xnull <- cbind(xrand, xdes) #design matrix for null model with random intercepts
  
  #sample from the unconstrained model gender stereotype and status signalling model and get BF vs. package null model
  sampMain <- nWayAOV(y, XmatM
                      , gMap = c(0:1, rep(2, I), rep(3, J)), rscale = c(r[1], r[1], r[3], r[3])
                      , posterior = T)
  BFMain <- nWayAOV(y, XmatM
                    , gMap = c(0:1, rep(2, I), rep(3, J)), rscale = c(r[1], r[1], r[3], r[3])
                    , posterior = F)
  #sample from the unconstrained culture change  model and get BF vs. package null model
  sampBaseline <- nWayAOV(y, XmatB
                          , gMap = c(0, rep(1, I), rep(2, J)), rscale = c(r[1], r[3], r[3])
                          , posterior = T)
  BFBaseline <- nWayAOV(y, XmatB
                        , gMap = c(0, rep(1, I), rep(2, J)), rscale = c(r[1], r[3], r[3])
                        , posterior = F)
  #sample from the unconstrained moderator interaction  model and get BF vs. package null model
  sampFull <- nWayAOV(y, XmatF
                      , gMap = c(0:2, rep(3, I), rep(4, J)), rscale = c(rep(r[1], 3), r[3], r[3])
                      , posterior = T)
  BFFull <- nWayAOV(y, XmatF
                    , gMap = c(0:2, rep(3, I), rep(4, J)), rscale = c(rep(r[1], 3), r[3], r[3])
                    , posterior = F)
  # get BF for the null model
  BFNull <- nWayAOV(y, Xnull
                    , gMap = c(rep(0, I), rep(1, J)), rscale = c(r[3],r[3])
                    , posterior = F)
  
  ## Moderator predictions for gender-by-moderator effects 
  
  # Interaction parameter 
  gamma <- sampFull[, 4]
  # prediction is that higher scores on the harmony moderator are related less status for angry vs neutral (smaller emotion coefficient), so a negative interaction term (right?)
  if(pos) postprob <- mean(gamma > 0) 
  if(!pos) postprob <- mean(gamma < 0) 
  BF_r <- postprob/0.5
  
  ## All Bayes factors as compared to unconstrained
  BF_0u <- 1/exp(BFFull$bf - BFNull$bf)
  BF_mu <- exp(BFMain$bf - BFFull$bf)
  BF_iu <- exp(BFBaseline$bf - BFFull$bf)
  BF_ru <- BF_r
  bfsu <- c(BF_0u, BF_iu, BF_mu, BF_ru)
  modnames <- c("Null vs. Full", "Individual Differences vs. Full", "Main Effects vs. Full", "Culture Interaction vs. Full")
  names(bfsu) <- modnames
  
  ## All Bayes factors as compared to null
  BF_0u <- exp(BFNull$bf - BFFull$bf)
  BF_0m <- exp(BFNull$bf - BFBaseline$bf)
  BF_0i <- exp(BFNull$bf - BFMain$bf)
  BF_0r <- BF_r * exp(BFNull$bf - BFFull$bf)
  bfs0 <- c(BF_0u, BF_0m, BF_0i, BF_0r)
  modnames0 <- c("Null vs. Full", "Null vs. Individual Differences", "Null vs. Main Effects", "Null vs. Culture Interaction")
  names(bfs0) <- modnames0
  
  ## All Bayes factors as compared to Individual differences 
  BF_0i <- exp(BFNull$bf - BFBaseline$bf)
  BF_mi <- exp(BFMain$bf - BFBaseline$bf)
  BF_ri <- BF_r * exp(BFFull$bf - BFBaseline$bf)
  BF_fi <- exp(BFFull$bf - BFBaseline$bf)
  bfs <- c(BF_0i, BF_mi, BF_ri, BF_fi)
  bfsID <- sapply(bfs, function(x) 1/x)
  modnamesID <- c("Individual Differences vs. Null", "Individual Differences vs. Main Effects", "Individual Differences vs. Culture Interaction", "Individual Differences vs. Full")
  names(bfsID) <- modnamesID
  
  return(list(bfsu=bfsu, bfs0=bfs0, bfsID = bfsID, 
              sampFull=sampFull, gamma=gamma))
}

doRandomExtremity = function(dat, y, r){
  ############Design matrices
  site <- dat$site
  design <- dat$design
  I <- max(site)
  J <- max(design)
  # Indicator variables for the full model, using effect coding 
  xk <- ifelse(dat$extremity == "mild", -1/2, 1/2) #effect of extremity
  
  # Random intercepts for all models 
  xrand <- matrix(0, nrow = nrow(dat), ncol = I)
  for(i in 1:nrow(dat)){
    xrand[i, site[i]] <- 1
  }
  
  xdes <- matrix(0, nrow = nrow(dat), ncol = J)
  for(j in 1:nrow(dat)){
    xdes[j, design[j]] <- 1
  }
  
  Xmat <- cbind(xk, xrand, xdes) #design matrix for the gender stereotype and status signalling models including random site intercept
  
  #sample from the unconstrained model gender stereotype and status signalling model and get BF
  samp <- nWayAOV(y, Xmat
                  , gMap = c(0, rep(1, I), rep(2,J)), rscale = c(r[1], r[3], r[3])
                  , posterior = T)
  BF <- nWayAOV(y, Xmat
                , gMap = c(0, rep(1, I), rep(2,J)), rscale = c(r[1], r[3], r[3])
                , posterior = F)
  # get BF for the baseline model
  BFbase <- nWayAOV(y, cbind(xrand, xdes)
                    , gMap = c(rep(0, I), rep(1, J)), rscale = c(r[3],r[3])
                    , posterior = F)
  # get BF for the baseline model
  BFNull <- nWayAOV(y, xrand
                    , gMap = c(rep(0, I)), rscale = c(r[3])
                    , posterior = F)
  
  ## Posterior samples for the cell means
  xk0.i <- 1
  xrand.i <- 2:(1+I) 
  xdes.i <- (2+I):(J+1+I)
  
  x <- matrix(nrow=nrow(samp), ncol=J)
  i <- rep(c(-1/2,1/2), times=J/2)
  mat <- t(apply(x, 1, function(y) y=i))
  
  rand <- samp[, xdes.i+1] + samp[, 1]
  eff  <- samp[, xdes.i+1] + samp[, 1] + samp[, xk0.i+1]*mat
  
  BF_01 <- exp(BFNull$bf - BF$bf)
  BF_b1 <- exp(BFbase$bf - BF$bf)
  BF_b0 <- exp(BFbase$bf - BFNull$bf)
  
  return(list(BF_01=BF_01, BF_b1=BF_b1, BF_b0=BF_b0,
              samp=samp, alphas = rand, eff = eff))
}

