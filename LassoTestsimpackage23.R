###########################################################################################
#
#                 Functions to use in the testsims function - PPMLASSO code
#
###########################################################################################

# call the different function for the simulations


### Function ppmlassoMixEngine
#---------------------------------------------------------------------

makeMask = function(Qppp)
{
  
  q_df = data.frame(x = Qppp$x, y = Qppp$y)
  ux = sort(unique(q_df$x))
  uy = sort(unique(q_df$y))
  nx = length(ux)
  ny = length(uy)
  
  col.ref = match(q_df$x, ux)
  row.ref = match(q_df$y, uy)
  
  all.vec          = rep(0, max(row.ref)*max(col.ref))
  vec.ref          = (col.ref - 1)*max(row.ref) + row.ref
  all.vec[vec.ref] = 1
  mask.out         = matrix(all.vec, max(row.ref), max(col.ref), dimnames = list(uy, ux))
  mask.out
}

#------------------------------------------------------------------------------------
#  								function ppmlassoMixEngine
#------------------------------------------------------------------------------------

ppmlassoMixEngine = function(Known.ppp, Unknown.ppp, quadsenv, ppmform,
                             sp.scale, n.fits=50, initweights = c("knn", "kps", "kmeans", "random", "CoinF"),  
                             alpha = 1, k=1, nstart=nstart, classif = c("soft", "hard"), 
                             cov.bias=NULL, kVal =NULL, kAreaInt=NULL, maxit = 50, r=NULL,
                             verbose = TRUE, plots = FALSE, tol = 1e-6, max.it = 100, criterion="bic")
{
  #1# Fit initial point processes
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  sp_all.list = list(Known.ppp, Unknown.ppp)
  n.sp = length(unique(marks(sp_all.list[[1]])))
  mark.name = unique(marks(sp_all.list[[1]]))
  #win = owin(xrange = c(-0.5, 100.5), yrange = c(-0.5, 100.5))
  
  win.   = owin(xrange = c(min(quadsenv$X)-0.5, max(quadsenv$X)+0.5), 
                yrange = c(min(quadsenv$Y)-0.5, max(quadsenv$Y)+0.5))
  quads. = ppp(quadsenv$X, quadsenv$Y, window = win.)
  
  # prepare list of images of covariates
  cov.list = list()
  names.env = c()
  for (v in 1:(length(quadsenv)-2))
  {
    v.v = as.im(data.frame(x = quadsenv$X, y = quadsenv$Y, z = quadsenv[,v+2]))
    cov.list[[v]] = v.v
    names.env[v] = names(quadsenv)[v+2]
  }
  names(cov.list) = names.env
  
  if(all(sapply(sp_all.list, is.ppp))==TRUE){
    
    sp.xy = as.list(rep(NA, length(unique(marks(sp_all.list[[1]])))))
    for (i in 1:(length(unique(marks(sp_all.list[[1]]))))){
      sp.xy[[i]] = as.data.frame(cbind(sp_all.list[[1]][sp_all.list[[1]]$marks==mark.name[i]]$x, 
                                       sp_all.list[[1]][sp_all.list[[1]]$marks==mark.name[i]]$y))
      colnames(sp.xy[[i]]) = c("X", "Y")
    }
    
    # renaming and extracting from the list the different ppp or DF for known species
    for (i in 1:(length(unique(marks(sp_all.list[[1]]))))) {
      marks(sp_all.list[[1]][sp_all.list[[1]]$marks==mark.name[i]]) = mark.name[i]
      assign(paste(mark.name[i]), sp_all.list[[1]][sp_all.list[[1]]$marks==mark.name[i]])
    }
    
    unknown.xy=as.data.frame(cbind(sp_all.list[[2]]$x, sp_all.list[[2]]$y))
    
    colnames(unknown.xy)=c("X", "Y")
    marks(sp_all.list[[2]]) = "Unknown"
    assign(paste("ppp_Unknown"), sp_all.list[[2]])
    
    for (i in 1:(length(sp.xy)-1)) {
      assign(paste(mark.name[i]), sp.xy[[i]])
    }  
    
    coordsubx.list = coordsuby.list = marksub.list = list()
    for (l in 1:n.sp) {
      coordsubx.list[[l]] = sp_all.list[[1]][sp_all.list[[1]]$marks==mark.name[l]]$x
      coordsuby.list[[l]] = sp_all.list[[1]][sp_all.list[[1]]$marks==mark.name[l]]$y
      marksub.list[[l]] = rep(paste(mark.name[l]), 
                              sp_all.list[[1]][sp_all.list[[1]]$marks==mark.name[l]]$n)
      
      #l=l+1
    }
    
    all_true = ppp(x = c(unlist(coordsubx.list)), 
                   y = c(unlist(coordsuby.list)), window = win.,
                   marks = c(unlist(marksub.list)))
    
    sp.xy.all = do.call(rbind, sp.xy)
    datappp = superimpose(all_true, ppp_Unknown) #create a marked ppp 
    
    env.xy = as.data.frame(quadsenv)   #quads only needed in the context of ppp object implemented
    colnames(env.xy) = c(paste0(colnames(quadsenv)))
    
    
    # }else{
    #   if(all(sapply(sp_all.list[[1]], is.data.frame)) == TRUE){
    #     sp.xy = sp_all.list[[1]]
    #     for (i in 1:(length(sp_all.list[[1]]))){
    #       colnames(sp.xy[[i]]) = c("x", "y") 
    #     }
    #     
    #     # renaming and extracting from the list the different ppp or DF for known species
    #     for (i in 1:(length(sp_all.list[[1]])-1)) {
    #       sp.ppp = as.list(rep(NA, length(sp_all.list[[1]])-1))
    #       sp.ppp[[i]] = as.ppp(sp.xy[[i]])
    #       marks(sp_all.list[[1]][[i]]) = paste("Sp",i, sep="")
    #       assign(paste("sp", i, "_sub", sep = ""), sp.ppp[[i]])
    #     }
    #     for (i in 1:(length(sp.xy)-1)) {
    #       assign(paste("sp", i, ".xy", sep = ""), sp.xy[[i]])
    #     }
    #     
    #     sp_all.list[[2]] = as.ppp(sp_all.list[[2]], marks="Unknown")
    #     
    #     
    #     sp.xy.all = do.call(rbind, sp.xy)
    #     all_true = ppp(x = sp.xy.all$X, 
    #                    y = sp.xy.all$Y, window = win.,
    #                    marks = c(unlist(marksub.list)))
    #     
    #     unknown.xy=sp_all.list[[2]]
    #     colnames(unknown.xy)=c("X", "Y")
    #     ppp_Unknown = ppp(x=unknown.xy$X, y=unknown.xy$Y, window=win.)
    #     
    #     datappp = superimpose(all_true, ppp_Unknown) #create a marked ppp
    #     
    #     env.xy = as.data.frame(quadsenv)
    #     colnames(env.xy) = c(paste0(colnames(quadsenv)))
    #     
    #   }else{
    #     warning(paste("The format for each species coordinates and quads need to be ppp or dataframe and quadsenv needs to be a matrix of covariates with x and y corrdinates"),
    #             call.=FALSE)
    #     break
    #   }
  }
  
  X=quads.$x
  Y=quads.$y
  quad.xy = data.frame(X, Y)
  names(quad.xy) = c("X", "Y")
  iterppp = datappp
  
  datamarks = marks(datappp)
  uniquemarks = unique(datamarks)
  unknown = datamarks == "Unknown"
  nclust  = length(unique(datamarks)) - 1
  
  #1# Initialization of membership probabilities
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  # Set up otpion for initial weights  
  initweights <- match.arg(initweights)
  
  # for knn method
  if (initweights == "knn"){
    #1. Compute distances of unknown points to all known points
    if(k > all_true$n)
    {
      print("Impossible to consider so many points, the highest possible number will be used instead")
      k = all_true$n
    }
    
    knownmarks = datamarks[datamarks != "Unknown"] # marks of known points
    knownsp = sort(unique(knownmarks)) # names of known species
    n.sp = length(knownsp) # number of known species
    
    all_dists = as.matrix(dist(data.frame(datappp$x, datappp$y))) # all distances
    unknown_dists = all_dists[which(datamarks != "Unknown"), which(datamarks == "Unknown")] #distances between unkonwn points and known points
    
    ## If known and unknow location are on the same coordinates add some noise to avoid Inf values
    for (d in 1:sum(datamarks == "Unknown"))
    {
      for (e in 1:sum(datamarks != "Unknown"))
      {
        if (unknown_dists[e,d]==0){
          unknown_dists[e,d] = unknown_dists[e,d]+0.00001
        }
      }
    }
    
    #2. Compute unscaled membership weights for unknown species
    
    weight_first = matrix(NA, sum(datamarks == "Unknown"), n.sp) # initial unscaled weight
    
    for (i in 1:sum(datamarks == "Unknown"))
    {
      dist_i = unknown_dists[,i] # distances for ith unknown point
      shortest_k = sort(dist_i)[k] # find kth shortest distance
      for (j in 1:n.sp)
      {
        dist_i_sp_j = dist_i[knownmarks == knownsp[j]] # distances to species j
        within_shortest_k = dist_i_sp_j <= shortest_k # which are within the smallest k distances?
        shortdist_weights = 1/dist_i_sp_j*within_shortest_k # weight of 1/k for points within smallest k distances
        weight_first[i, j] = sum(shortdist_weights)
      }
    }
    
    #3. Rescale membership weights
    
    init_weight_unknown = prop.table(weight_first, 1) # rescale weights
    
    #4. Set up membership weights for points with known labels
    
    init_weight_known = matrix(0, length(knownmarks), n.sp) # weights for known species labels
    for (j in 1:n.sp)
    {
      init_weight_known[knownmarks == knownsp[j], j] = 1
    }
    
    #5. Combine
    
    init.weight = rbind(init_weight_known, init_weight_unknown) # combine
    colnames(init.weight) = knownsp
    
    iterweights = init.weight
    itermarks = datamarks
    itermarks = colnames(init.weight)[apply(init.weight, 1, which.max)]
    
  }
  
  if (initweights == "kps"){
    ks = k
    if(max(ks) > min(table(all_true$marks)))
    {
      print("Impossible to consider so many points, the highest possible number will be used instead")
      ks = 1:min(table(all_true$marks))
    }
    
    # calculate the nearest neighbours distances
    nndists  = nndist(datappp, k=ks, by = as.factor(marks(datappp)))
    if(length(ks)>1){
      nndists.new = matrix(ncol=n.sp,nrow=datappp$n)
      nndists  = nndists[1:(n.sp*length(ks))]  # only consider known sp yet
      
      splitdf <- function(df, n) {
        indx <- matrix(seq_len(ncol(df)), ncol = n)
        lapply(seq_len(n), function(x) df[, indx[, x]])
      }
      
      nndist.sub = splitdf(nndists, n.sp) # split the dataset in different dataset according to the number of species
      
      for (s in 1:n.sp) {
        nndists.new[,s] = apply(nndist.sub[[s]], 1, sum)/length(ks) #average the ks distances
      }
      
      nndists = nndists.new
      
      datamarks = marks(datappp)
      uniquemarks = unique(datamarks)
      colnames(nndists) = unique(datamarks[datamarks!="Unknown"])
    }else{
      nndists  = nndists[,-which(colnames(nndists) == "Unknown")]
    }
    
    # Calculate the weights according the distances
    weight_num = apply(nndists, 1, min)/nndists
    init.weight = weight_num/apply(weight_num, 1, sum)
    init.weight[datamarks != "Unknown"] = rep(0, nclust)
    
    # Set the weights to 1 for the known points species and 0 when it is not the known points species
    # For the unknwon points the weights are the calculated weights
    for (i in 1:nclust)
    {
      rowfill = which(datamarks == colnames(nndists)[i])
      init.weight[rowfill, i] = 1
    }
    
    init.weight[is.nan(init.weight)] <- 1
    
    iterweights = init.weight
    itermarks = datamarks
    itermarks = colnames(nndists)[apply(init.weight, 1, which.max)]
  }
  
  #for kmeans method
  if (initweights == "kmeans"){
    # Get the coordinates of the clusters centers
    xy = coords(datappp)
    ncenter = nclust
    comp_mean = kmeans(xy, ncenter, nstart = nstart)
    
    Ccenter = comp_mean$centers 
    
    # calculate a distance of each point to each center
    All_dist_center = matrix(data=NA, nrow=nrow(xy), ncol=nclust)
    
    for (i in 1:(nclust))
    {
      Di= sqrt((xy$x-Ccenter[[i]])^2 + (xy$y-Ccenter[[i+nclust]])^2) 
      All_dist_center[,i] = Di
    }
    
    colnames(All_dist_center) <- paste("D", 1:nclust, sep = "")
    
    marksknown = uniquemarks[-which(uniquemarks == "Unknown")]
    colnames(All_dist_center) = marksknown
    
    # Calculate the weights according to the distances
    weight_num = apply(All_dist_center, 1, min)/All_dist_center
    init.weight = weight_num/apply(weight_num, 1, sum)
    
    # Set the weights to 1 for the known points species and 0 when it is not the known points species
    # For the unknwon points the weights are the calculated weights
    init.weight[datamarks != "Unknown"] = rep(0, nclust)
    for (i in 1:nclust)
    {
      rowfill = which(datamarks == colnames(All_dist_center)[i])
      init.weight[rowfill, i] = 1
    }
    
    init.weight[is.nan(init.weight)] <- 1
    
    iterweights = init.weight
    itermarks = datamarks
    itermarks = colnames(All_dist_center)[apply(init.weight, 1, which.max)]
    
  }
  
  # for random method
  if (initweights == "random"){
    #BIC.mixt = c(rep(NA, nstart))
    #init.weight = matrix(NA, datappp$n, nclust)
    
    # We randomly generate the weights of each observation for across the different species
    #for (s in 1:nstart) {
    random.val = runif(nclust*datappp$n, min=0, max=1)
    weight_num = matrix(random.val, datappp$n, nclust)
    init.weight.rd = weight_num/apply(weight_num, 1, sum)  # make weights add up to 1
    
    colmarks = uniquemarks
    colmarks  = colmarks[-which(colmarks == "Unknown")]
    colnames(init.weight.rd) = colmarks
    
    # Set the weights to 1 for the known points species and 0 when it is not the known points species
    # For the unknown points the weights are the calculated weights
    init.weight.rd[datamarks != "Unknown"] = rep(0, nclust)
    for (i in 1:nclust)
    {
      rowfill = which(datamarks == colmarks[i])
      init.weight.rd[rowfill, i] = 1
    }
    init.weight.rd[is.nan(init.weight.rd)] <- 1
    
    iterweights = init.weight.rd
    itermarks = datamarks
    itermarks = colnames(init.weight.rd)[apply(init.weight.rd, 1, which.max)]
    
    # for random we fit a point process to choose through BIC criterion the best random starting values
    p = table(itermarks)/sum(table(itermarks))
    
    iterppp = datappp
    marks(iterppp) = as.factor(itermarks)
    Qmask = makeMask(quads.)
    
    Q = quadscheme(data = iterppp, dummy = quads., method = "grid",
                   ntile = c(dim(Qmask)[2], dim(Qmask)[1]), 
                   npix = c(dim(Qmask)[2], dim(Qmask)[1]))
    
    formchr = as.character(ppmform)[2]
    formsplit = strsplit(formchr, "\\+")
    markform = as.formula(paste("~", paste(paste(formsplit[[1]], "* marks"), collapse = " + ")))
    
    if(is.null(kAreaInt)){
      fit1 = ppm(Q, trend = markform, covariates = cov.list, 
                 gcontrol = list(epsilon = 1e-6, maxit = 100)) # including known and unknown points
    }else{
      fit1 = ppm(Q, trend = markform, covariates = cov.list, AreaInter(kAreaInt),
                 gcontrol = list(epsilon = 1e-6, maxit = 100)) # including known and unknown points
      
    }
    
    # BIC criterion (Maasaro 2017 - Poisson mixture model selection)
    #G = length(cov.list.)
    #BIC.mixt[s]= -2*(fit1$maxlogpl) + (2*G-1)*(log(datappp$n) + 1)
    #if(BIC.mixt[s] == min(BIC.mixt, na.rm = T)){
    #  init.weight = init.weight.rd
    #}else{
    #  init.weight = init.weight
    #}
    
    #if(verbose) 
    #  cat(paste("Iteration", s, "\tBIC =", BIC.mixt[s], "\n"))
    
    #s = s + 1
    
    #}
    
    iterweights = init.weight.rd
    itermarks = datamarks
    itermarks = colnames(init.weight.rd)[apply(init.weight.rd, 1, which.max)]
    
  }
  
  # random coin flip allocation system
  if (initweights == "CoinF"){
    itermarks = datamarks
    Lmarks  = length(datamarks[which(datamarks == "Unknown")])
    sp.marks = unique(datamarks[datamarks!="Unknown"])
    
    itermarks[which(datamarks == "Unknown")] = sample(sp.marks, Lmarks, replace=TRUE)
    
    colmarks = uniquemarks
    colmarks  = colmarks[-which(colmarks == "Unknown")]
    init.weight = matrix(0, nrow=length(itermarks) , ncol=nclust)
    for (i in 1:nclust)
    {
      rowfill = which(itermarks == colmarks[i])
      init.weight[rowfill, i] = 1
    }
    iterweights = init.weight
    colnames(iterweights)=sp.marks
  }
  
  if(classif == "hard"){
    #2# Fit point process models
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    # continue general script for all the methods
    p = table(itermarks)/sum(table(itermarks))
    
    length.ppp = as.list(rep(NA, nclust))
    for (n in 1:nclust) {
      length.ppp[n] = sp_all.list[[1]][sp_all.list[[1]]$marks == mark.name[n]]$n
    }
    
    sum.sub=sum(unlist(length.ppp))
    itermarks.sub = itermarks[(sum.sub+1):length(itermarks)]
    
    unk.df = as.data.frame(cbind(as.numeric(unknown.xy$X), as.numeric(unknown.xy$Y)))
    unk_sp = cbind(unk.df, itermarks.sub)
    
    sp_iter = list()
    for (i in 1:nclust) {
      sp_iter[[i]] = as.data.frame(cbind(c(sp_all.list[[1]][sp_all.list[[1]]$marks==mark.name[i]]$x, 
                                           unk_sp$V1[unk_sp$itermarks.sub == mark.name[i]]),
                                         c(sp_all.list[[1]][sp_all.list[[1]]$marks==mark.name[i]]$y, 
                                           unk_sp$V2[unk_sp$itermarks.sub == mark.name[i]])))
      colnames(sp_iter[[i]]) = c("X", "Y")
    }
    
    iterppp = datappp
    
    X=quads.$x
    Y=quads.$y
    quad.xy = data.frame(X, Y)
    
    # compute quadrature weights for each augmented species
    #scores for species with known label (weight =1) and for the new obs (test_wts)
    it_unk_wts = iterweights[unknown,]
    
    # sp_wts = keep_wt = keep_unk = ppp_list = ppp.df = list()
    # for (l in 1:n.sp) {
    # keep_unk[[l]] = sp_all.list[[2]][which(iterweights[unknown, l]!=0)]
    # keep_wt[[l]] = iterweights[unknown, i][which(iterweights[unknown, l]!=0)]
    # ppp_list[[l]] = superimpose(unmark(sp_all.list[[1]][[l]]), unmark(keep_unk[[l]]))
    # ppp.df[[l]] = as.data.frame(cbind(ppp_list[[l]]$x, ppp_list[[l]]$y))
    # colnames(ppp.df[[l]]) = c("X", "Y")
    # sp_wts[[l]] = scoreweights(ppp.df[[l]], quad.xy, scores = c(rep(1, sp_all.list[[1]][[l]]$n), keep_wt[[l]])) # generate quad weights for augmented species l
    # }
    
    
    if(is.null(cov.bias)){
      env.xy = env.xy
    }else{
      pred.env.xy = env.xy
      set.Val = cov.bias #Variables to set to a certain value
      for (b in set.Val){
        pred.env.xy[,b] = kVal*pred.env.xy[,b]
      }
    }
    
    # Fit Poisson PPMs
    ppm_sp = spenv.var = ppmprep = list()
    
    
    for (l in 1:nclust) {
      if(is.null(r) == TRUE){
        spenv.var[[l]] = env.var(sp.xy=sp_iter[[l]], env.grid=env.xy, coord = c("X", "Y"), 
                                 file.name = NA, env.scale = sp.scale)
        
        ppmprep[[l]] = ppmdat(sp.xy=sp_iter[[l]], back.xy = env.xy, sp.scale = sp.scale,
                              coord = c("X", "Y"), sp.dat = spenv.var[[l]], file.name = NA)
        
        ppm_sp[[l]] = ppmlasso(formula=ppmform, sp.xy=sp_iter[[l]], env.grid=env.xy, sp.scale = sp.scale,
                               coord = c("X", "Y"), n.fits = n.fits, alpha = alpha, data = ppmprep[[l]],
                               #ob.wt = sp_wts[[l]], 
                               tol = 1e-6, max.it = 100, criterion="bic")
      }else{
        spenv.var[[l]] = env.var(sp.xy=sp_iter[[l]], env.grid=env.xy, coord = c("X", "Y"), 
                                 file.name = NA, env.scale = sp.scale)
        
        ppmprep[[l]] = ppmdat(sp.xy=sp_iter[[l]], back.xy = env.xy, sp.scale = sp.scale,
                              coord = c("X", "Y"), sp.dat = spenv.var[[l]], file.name = NA)
        
        ppm_sp[[l]] = ppmlasso(ppmform, sp_iter[[l]], env.xy, sp.scale = sp.scale, data = ppmprep[[l]],
                               coord = c("X", "Y"), n.fits = n.fits, alpha = alpha,
                               family = "area.inter", r=r, tol = 1e-6, max.it = 100, 
                               #ob.wt = sp_wts[[l]], 
                               criterion="bic")
      }
    }
    
    #3# Compute predicted intensities
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    fitsp = list()
    
    for (i in 1:nclust) {
      if(is.null(cov.bias)){
        fitsp[[i]] = predict.ppmlasso(ppm_sp[[i]], newdata = env.xy)
        
      }else{
        fitsp[[i]] = predict.ppmlasso(ppm_sp[[i]], newdata = pred.env.xy)
      }
    }
    
    if (plots == TRUE)
    {
      for (i in 1:nclust) {
        plotfit(ppm_sp[[i]], main= paste("plot - fitsp", i, sep = "")) 
      }
    }
    
    pfit.sp=list()
    for (i in 1:nclust) {
      pfit.sp[[i]] = ppm_sp[[i]]$mu
    }
    
    loglik.old <- 0.
    loglik.new <- 1.
    
    #
    # Iterator starts here, 
    #
    
    is_known = which(datamarks != "Unknown")
    niter <- 0
    while(abs(loglik.new - loglik.old)/abs(loglik.old) > tol) {
      if(niter >= maxit) {
        warning(paste("E-M algorithm failed to converge in",
                      maxit, ngettext(maxit, "iteration", "iterations")),
                call.=FALSE)
        break
      }
      
      
      niter <- niter + 1
      
      #4# Get the predicted intensities at the location S
      #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      
      
      # E - step #-------------------------------------------------------------------------------#
      
      if(is.null(cov.bias)){
        unkcov.mat = env.var(unknown.xy, env.xy, env.scale = sp.scale, coord = c("X", "Y"))
        
        predint = matrix(NA, sum(unknown), nclust)
        for (m in 1:nclust) {
          predint[,m] = predict.ppmlasso(ppm_sp[[m]], newdata = unkcov.mat)  
        }
        
      }else{
        unkpred.mat = env.var(unknown.xy, pred.env.xy, env.scale = sp.scale, coord = c("X", "Y"))
        predint = matrix(NA, sum(unknown), nclust)
        for (m in 1:nclust) {
          predint[,m] = predict.ppmlasso(ppm_sp[[m]], newdata = unkpred.mat)
        }
        
      }
      
      p_mat = t(matrix(p, ncol(predint), nrow(predint)))
      predint_p = predint*p_mat
      
      #5# Caluclate New weights
      #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      
      iterweights[unknown,] = predint_p/apply(predint_p, 1, sum)
      
      # end E-step #-------------------------------------------------------------------------------#
      
      #6# We compute species proportions
      #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      
      # M - step #-------------------------------------------------------------------------------#
      #
      p = apply(iterweights, 2, sum)/sum(apply(iterweights, 2, sum))
      
      itermarks = datamarks
      itermarks = colnames(iterweights)[apply(iterweights, 1, which.max)] # assign marks based on new weights
      iterppp = datappp
      marks(iterppp) = as.factor(itermarks)
      
      unk_sp = cbind(unk.df, itermarks.sub)
      
      # for (i in 1:nclust) {
      #   sp_iter[[i]] = as.data.frame(cbind(c(sp.xy[[i]]$X, unk_sp$V1[unk_sp$itermarks.sub == mark.name[i]]),
      #                                  c(sp.xy[[i]]$Y, unk_sp$V2[unk_sp$itermarks.sub == mark.name[i]])))
      #   colnames(sp_iter[[i]]) = c("X", "Y")
      # }
      
      # weights
      it_unk_wts = iterweights[unknown,]
      
      
      sp_iter_op = list()
      for (i in 1:nclust) {
        sp_iter_op[[i]] = as.data.frame(cbind(c(sp_all.list[[1]][sp_all.list[[1]]$marks==mark.name[i]]$x, 
                                                unk_sp$V1[unk_sp$itermarks.sub == mark.name[i]]),
                                              c(sp_all.list[[1]][sp_all.list[[1]]$marks==mark.name[i]]$y, 
                                                unk_sp$V2[unk_sp$itermarks.sub == mark.name[i]])))
        colnames(sp_iter_op[[i]]) = c("X", "Y")
      }
      # sp_wts = keep_wt = keep_unk = ppp_list = ppp.df = list()
      # for (l in 1:n.sp) {
      # keep_unk[[l]] = sp_all.list[[2]][which(iterweights[unknown, l]!=0)]
      # keep_wt[[l]] = iterweights[unknown, i][which(iterweights[unknown, l]!=0)]
      # ppp_list[[l]] = superimpose(unmark(sp_all.list[[1]][[l]]), unmark(keep_unk[[l]]))
      # ppp.df[[l]] = as.data.frame(cbind(ppp_list[[l]]$x, ppp_list[[l]]$y))
      # colnames(ppp.df[[l]]) = c("X", "Y")
      # sp_wts[[l]] = scoreweights(ppp.df[[l]], quad.xy, scores = c(rep(1, sp_all.list[[1]][[l]]$n), keep_wt[[l]])) # generate quad weights for augmented species l
      # }
      
      #8# Fit new point process models
      #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      
      # Fit Poisson PPMs
      for (i in 1:nclust) {
        if(is.null(r) == TRUE){
          spenv.var[[i]] = env.var(sp.xy=sp_iter_op[[i]], env.grid=env.xy, coord = c("X", "Y"), 
                                   file.name = NA, env.scale = sp.scale)
          
          ppmprep[[i]] = ppmdat(sp.xy=sp_iter_op[[i]], back.xy = env.xy, sp.scale = sp.scale,
                                coord = c("X", "Y"), sp.dat = spenv.var[[i]], file.name = NA)
          
          ppm_sp[[i]]  = ppmlasso(ppmform, sp_iter_op[[i]], env.xy, sp.scale = sp.scale, data = ppmprep[[i]],
                                  n.fits = n.fits, alpha = alpha, tol = 1e-6, max.it = 100, 
                                  #ob.wt = sp_wts[[i]], 
                                  criterion="bic")
        }else{
          
          spenv.var[[i]] = env.var(sp.xy=sp_iter_op[[i]], env.grid=env.xy, coord = c("X", "Y"), 
                                   file.name = NA, env.scale = sp.scale)
          
          ppmprep[[i]] = ppmdat(sp.xy=sp_iter_op[[i]], back.xy = env.xy, sp.scale = sp.scale,
                                coord = c("X", "Y"), sp.dat = spenv.var[[i]], file.name = NA)
          
          ppm_sp[[i]]  = ppmlasso(ppmform, sp_iter_op[[i]], env.xy, sp.scale = sp.scale,
                                  n.fits = n.fits, family = "area.inter", r=r, alpha = alpha, 
                                  #yes
                                  ob.wt = sp_wts[[i]], data = ppmprep[[i]], 
                                  tol = 1e-6, max.it = 100, criterion="bic")
        }
        
        if(is.null(cov.bias)){
          fitsp[[i]] = predict.ppmlasso(ppm_sp[[i]], newdata = env.xy)
        }else{
          fitsp[[i]] = predict.ppmlasso(ppm_sp[[i]], newdata = pred.env.xy)
        }
        
        
        if (plots == TRUE)
        {
          plotfit(ppm_sp[[i]], main=paste("plot - fitsp", i, sep = ""))
          plotpath(ppm_sp[[i]])
        }
        
        pfit.sp[[i]] = ppm_sp[[i]]$mu
      }
      
      # end M-step
      
      #9# Stopping criterion
      #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      
      # evaluate marginal loglikelihood
      
      loglik.old = loglik.new
      #-## this is the loglik for the ppm with known and unknown points but not really of a mixture model here...  
      #  loglik.new = fit1$maxlogpl
      
      #-##       
      allp_mat = t(matrix(p, ncol(predint), nrow(iterweights)))
      loglik.new <- sum(log(apply(allp_mat * iterweights, 1, sum))) 
      
      loglik.new
      #sum(itermarks[unknown] == test_labels_sp)
      
      #loglik for mixture model : iterwights regroups weights for known and unknown observation
      Newpts_w = iterweights[unknown,]
      # Prepare weights for the plots
      Weight.df = as.data.frame(iterweights[unknown,])
      
      if(verbose) 
        cat(paste("Iteration", niter, "\tlogLik =", loglik.new,
                  "\tp =", signif(p,4), "\n"))
    }
    
  }
  
  if(classif == "soft"){
    #2# Fit point process models
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    # continue general script for all the methods
    p = apply(iterweights, 2, sum)/sum(apply(iterweights, 2, sum))
    
    length.ppp = as.list(rep(NA, nclust))
    for (n in 1:nclust) {
      length.ppp[n]=sp_all.list[[1]][sp_all.list[[1]]$marks==mark.name[i]]$n
    }
    
    # sum.sub=sum(unlist(length.ppp))
    # itermarks.sub = itermarks[(sum.sub+1):length(itermarks)]
    
    # unk.df = as.data.frame(cbind(as.numeric(unknown.xy$X), as.numeric(unknown.xy$Y)))
    # unk_sp = cbind(unk.df, itermarks.sub)
    
    # sp_iter = list()
    # for (i in 1:nclust) {
    # sp_iter[[i]] = as.data.frame(cbind(c(sp_all.list[[1]][[i]]$x, unk_sp$V1[unk_sp$itermarks.sub == mark.name[i]]),
    # c(sp_all.list[[1]][[i]]$y, unk_sp$V2[unk_sp$itermarks.sub == mark.name[i]])))
    # colnames(sp_iter[[i]]) = c("X", "Y")
    # }
    iterppp = datappp
    
    X=quads.$x
    Y=quads.$y
    quad.xy = data.frame(X, Y)
    
    # compute quadrature weights for each augmented species
    #scores for species with known label (weight =1) and for the new obs (test_wts)
    it_unk_wts = iterweights[unknown,]
    
    sp_wts = keep_wt = keep_unk = ppp_list = ppp.df = list()
    for (l in 1:n.sp) {
      keep_unk[[l]] = sp_all.list[[2]][which(iterweights[unknown, l]!=0)]
      keep_wt[[l]] = iterweights[unknown, l][which(iterweights[unknown, l]!=0)]
      ppp_list[[l]] = superimpose(unmark(sp_all.list[[1]][sp_all.list[[1]]$marks==paste("Sp", l, sep = "")]), 
                                  unmark(keep_unk[[l]]))
      ppp.df[[l]] = as.data.frame(cbind(ppp_list[[l]]$x, ppp_list[[l]]$y))
      colnames(ppp.df[[l]]) = c("X", "Y")
      sp_wts[[l]] = scoreweights(ppp.df[[l]], quad.xy, 
                                 scores = c(rep(1, sp_all.list[[1]][sp_all.list[[1]]$marks==paste("Sp", l, sep = "")]$n), 
                                            keep_wt[[l]])) # generate quad weights for augmented species l
    }
    
    
    if(is.null(cov.bias)){
      env.xy = env.xy
    }else{
      pred.env.xy = env.xy
      set.Val = cov.bias #Variables to set to a certain value
      for (b in set.Val){
        pred.env.xy[,b] = kVal*pred.env.xy[,b]
      }
    }
    
    # Fit Poisson PPMs
    ppm_sp=ppmprep=spenv.var=list()
    
    for (l in 1:nclust) {
      if(is.null(r) == TRUE){
        spenv.var[[l]] = env.var(sp.xy=ppp.df[[l]], env.grid=env.xy, coord = c("X", "Y"), 
                                 file.name = NA, env.scale = sp.scale)
        
        ppmprep[[l]] = ppmdat(sp.xy=ppp.df[[l]], back.xy = env.xy, sp.scale = sp.scale,
                              coord = c("X", "Y"), sp.dat = spenv.var[[l]], file.name = NA)
        
        ppm_sp[[l]] = ppmlasso(formula=ppmform, sp.xy=ppp.df[[l]], env.grid=env.xy, sp.scale = sp.scale,
                               coord = c("X", "Y"), n.fits = n.fits, alpha = alpha, data = ppmprep[[l]], 
                               ob.wt = sp_wts[[l]], tol = 1e-6, max.it = 100, criterion="bic")
      }else{
        spenv.var[[l]] = env.var(sp.xy=ppp.df[[l]], env.grid=env.xy, coord = c("X", "Y"), 
                                 file.name = NA, env.scale = sp.scale)
        
        ppmprep[[l]] = ppmdat(sp.xy=ppp.df[[l]], back.xy = env.xy, sp.scale = sp.scale,
                              coord = c("X", "Y"), sp.dat = spenv.var[[l]], file.name = NA)
        
        ppm_sp[[l]] = ppmlasso(ppmform, ppp.df[[l]], env.xy, sp.scale = sp.scale,
                               coord = c("X", "Y"), n.fits = n.fits, alpha = alpha, data = ppmprep[[l]],
                               family = "area.inter", r=r, tol = 1e-6, max.it = 100, 
                               ob.wt = sp_wts[[l]], criterion="bic")
      }
    }
    
    #3# Compute predicted intensities
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    fitsp = list()
    
    for (i in 1:nclust) {
      if(is.null(cov.bias)){
        fitsp[[i]] = predict.ppmlasso(ppm_sp[[i]], newdata = env.xy)
        
      }else{
        fitsp[[i]] = predict.ppmlasso(ppm_sp[[i]], newdata = pred.env.xy)
      }
    }
    
    if (plots == TRUE)
    {
      for (i in 1:nclust) {
        plotfit(ppm_sp[[i]], main= paste("plot - fitsp", i, sep = "")) 
      }
    }
    
    pfit.sp=list()
    for (i in 1:nclust) {
      pfit.sp[[i]] = ppm_sp[[i]]$mu
    }
    
    loglik.old <- 0.
    loglik.new <- 1.
    
    #
    # Iterator starts here, 
    #
    
    is_known = which(datamarks != "Unknown")
    niter <- 0
    while(abs(loglik.new - loglik.old)/abs(loglik.old) > tol) {
      if(niter >= maxit) {
        warning(paste("E-M algorithm failed to converge in",
                      maxit, ngettext(maxit, "iteration", "iterations")),
                call.=FALSE)
        break
      }
      
      
      niter <- niter + 1
      
      #4# Get the predicted intensities at the location S
      #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      
      
      # E - step #-------------------------------------------------------------------------------#
      
      if(is.null(cov.bias)){
        unkcov.mat = env.var(unknown.xy, env.xy, env.scale = sp.scale, coord = c("X", "Y"))
        
        predint = matrix(NA, sum(unknown), nclust)
        for (m in 1:nclust) {
          predint[,m] = predict.ppmlasso(ppm_sp[[m]], newdata = unkcov.mat)  
        }
        
      }else{
        unkpred.mat = env.var(unknown.xy, pred.env.xy, env.scale = sp.scale, coord = c("X", "Y"))
        predint = matrix(NA, sum(unknown), nclust)
        for (m in 1:nclust) {
          predint[,m] = predict.ppmlasso(ppm_sp[[m]], newdata = unkpred.mat)
        }
        
      }
      
      p_mat = t(matrix(p, ncol(predint), nrow(predint)))
      predint_p = predint*p_mat
      
      #5# Caluclate New weights
      #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      
      iterweights[unknown,] = predint_p/apply(predint_p, 1, sum)
      
      # end E-step #-------------------------------------------------------------------------------#
      
      #6# We compute species proportions
      #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      
      # M - step #-------------------------------------------------------------------------------#
      #
      p = apply(iterweights, 2, sum)/sum(apply(iterweights, 2, sum))
      
      # itermarks = datamarks
      # itermarks = colnames(iterweights)[apply(iterweights, 1, which.max)] # assign marks based on new weights
      # iterppp = datappp
      # marks(iterppp) = as.factor(itermarks)
      
      # unk_sp = cbind(unk.df, itermarks.sub)
      
      # for (i in 1:nclust) {
      #   sp_iter[[i]] = as.data.frame(cbind(c(sp.xy[[i]]$X, unk_sp$V1[unk_sp$itermarks.sub == mark.name[i]]),
      #                                  c(sp.xy[[i]]$Y, unk_sp$V2[unk_sp$itermarks.sub == mark.name[i]])))
      #   colnames(sp_iter[[i]]) = c("X", "Y")
      # }
      
      # weights
      it_unk_wts = iterweights[unknown,]
      
      sp_wts = keep_wt = keep_unk = ppp_list = ppp.df = list()
      for (l in 1:n.sp) {
        keep_unk[[l]] = sp_all.list[[2]][which(iterweights[unknown, l]!=0)]
        keep_wt[[l]] = iterweights[unknown, l][which(iterweights[unknown, l]!=0)]
        ppp_list[[l]] = superimpose(unmark(sp_all.list[[1]][sp_all.list[[1]]$marks==paste("Sp", l, sep = "")]), 
                                    unmark(keep_unk[[l]]))
        ppp.df[[l]] = as.data.frame(cbind(ppp_list[[l]]$x, ppp_list[[l]]$y))
        colnames(ppp.df[[l]]) = c("X", "Y")
        sp_wts[[l]] = scoreweights(ppp.df[[l]], quad.xy, 
                                   scores = c(rep(1, sp_all.list[[1]][sp_all.list[[1]]$marks==paste("Sp", l, sep = "")]$n), 
                                              keep_wt[[l]])) # generate quad weights for augmented species l
      }
      
      #8# Fit new point process models
      #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      
      # Fit Poisson PPMs
      for (l in 1:nclust) {
        if(is.null(r) == TRUE){
          spenv.var[[l]] = env.var(sp.xy=ppp.df[[l]], env.grid=env.xy, coord = c("X", "Y"), 
                                   file.name = NA, env.scale = sp.scale)
          
          ppmprep[[l]] = ppmdat(sp.xy=ppp.df[[l]], back.xy = env.xy, sp.scale = sp.scale,
                                coord = c("X", "Y"), sp.dat = spenv.var[[l]], file.name = NA)
          
          ppm_sp[[l]]  = ppmlasso(ppmform, ppp.df[[l]], env.xy, sp.scale = sp.scale, data = ppmprep[[l]],
                                  n.fits = n.fits, alpha = alpha, tol = 1e-6, max.it = 100, 
                                  ob.wt = sp_wts[[l]], criterion="bic")
        }else{
          spenv.var[[l]] = env.var(sp.xy=ppp.df[[l]], env.grid=env.xy, coord = c("X", "Y"), 
                                   file.name = NA, env.scale = sp.scale)
          
          ppmprep[[l]] = ppmdat(sp.xy=ppp.df[[l]], back.xy = env.xy, sp.scale = sp.scale,
                                coord = c("X", "Y"), sp.dat = spenv.var[[l]], file.name = NA)
          
          ppm_sp[[l]]  = ppmlasso(ppmform, ppp.df[[l]], env.xy, sp.scale = sp.scale, data = ppmprep[[l]],
                                  n.fits = n.fits, family = "area.inter", r=r, alpha = alpha, 
                                  ob.wt = sp_wts[[l]], tol = 1e-6, max.it = 100, criterion="bic")
        }
        
        if(is.null(cov.bias)){
          fitsp[[l]] = predict.ppmlasso(ppm_sp[[l]], newdata = env.xy)
        }else{
          fitsp[[l]] = predict.ppmlasso(ppm_sp[[l]], newdata = pred.env.xy)
        }
        
        
        if (plots == TRUE)
        {
          plotfit(ppm_sp[[l]], main=paste("plot - fitsp", l, sep = ""))
          plotpath(ppm_sp[[l]])
        }
        
        pfit.sp[[l]] = ppm_sp[[l]]$mu
      }
      
      # end M-step
      
      #9# Stopping criterion
      #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      
      # evaluate marginal loglikelihood
      
      loglik.old = loglik.new
      #-## this is the loglik for the ppm with known and unknown points but not really of a mixture model here...  
      #  loglik.new = fit1$maxlogpl
      
      #-##       
      allp_mat = t(matrix(p, ncol(predint), nrow(iterweights)))
      loglik.new <- sum(log(apply(allp_mat * iterweights, 1, sum))) 
      
      loglik.new
      #sum(itermarks[unknown] == test_labels_sp)
      
      #loglik for mixture model : iterwights regroups weights for known and unknown observation
      Newpts_w = iterweights[unknown,]
      # Prepare weights for the plots
      Weight.df = as.data.frame(iterweights[unknown,])
      
      if(verbose) 
        cat(paste("Iteration", niter, "\tlogLik =", loglik.new,
                  "\tp =", signif(p,4), "\n"))
    }
    
  }
  if(verbose) {
    cat("\nEstimated parameters:\n")
    cat(paste("p [cluster] =", signif(p, 5), "\n"))
    cat(paste("\nloglik.new:\n", signif(loglik.new), "\n"))
  }
  #
  if (plots == TRUE)
  {
    par(xpd=NA)
    known.marks = unique(iterppp$marks)
    plot(x=seq_along(Weight.df[,1]), y=Weight.df[,1], col = "orange", pch=16, ylim=c(0,1),
         xlab="observations", ylab="weight")
    for (i in 2:nclust) {
      colvect=c("purple", "turquoise3", "darkred", "green", "brown")[1:nclust-1]
      points(x=seq_along(Weight.df[,i]), y=Weight.df[,i], col = colvect[i-1], pch=16, ylim=c(0,1))
      i =i + 1
    }
    legend(110,1, c(known.marks), col = c("orange", colvect),
           pch = 16, xjust = 1, yjust = 0, merge = FALSE)
  }
  
  return(list(z = round(p, digits = 4),
              #probs = p,
              niter = niter, maxit = maxit,
              converged = (niter >= maxit),
              New_weights = round(iterweights, digits = 4),
              Newpts_w = Newpts_w,
              pred.loc = fitsp,
              classif = classif,
              pfit.sp = pfit.sp,
              fit.final = ppm_sp,
              iterppp = iterppp,
              sp_aug.list = NULL,
              Known.ppp = Known.ppp,
              Unknown.ppp =Unknown.ppp
              #hist=if(plothist) H else NULL
              # plot(x=seq_along(Weight.df$Sp1), y=Weight.df$Sp1, col = "orange", pch=16, ylim=c(0,1),
              #      xlab="observations", ylab="weight"),
              # points(x=seq_along(Weight.df$Sp2), y=Weight.df$Sp2, col = "purple", pch=18, ylim=c(0,1)),
              # points(x=seq_along(Weight.df$Sp3), y=Weight.df$Sp3, col = "Turquoise3", pch=17, ylim=c(0,1)),
              # legend(1,1, c("sp1", "sp2", "sp3"), col = c("orange", "purple", "Turquoise3"),
              #        pch = c(16, 18, 17), xjust = 1, yjust = 0, merge = FALSE)
  ))
}







### Function ppmlassoLoopEngine
#---------------------------------------------------------------------


scoreweights = function(sp.xy, quad.xy, coord = c("X", "Y"), scores = NULL)
{
  if (is.null(scores)){
    score.all = rep(1, (dim(sp.xy)[1]) + dim(quad.xy)[1])
  }else{
    score.all = c(scores, rep(1, dim(quad.xy)[1]))
  }
  
  sp.col   = c(which(names(sp.xy) == coord[1]), which(names(sp.xy) == coord[2]))
  quad.col = c(which(names(quad.xy) == coord[1]), which(names(quad.xy) == coord[2]))
  
  X.inc   = sort(unique(quad.xy[,quad.col[1]]))[2] - sort(unique(quad.xy[,quad.col[1]]))[1]
  Y.inc   = sort(unique(quad.xy[,quad.col[2]]))[2] - sort(unique(quad.xy[,quad.col[2]]))[1]
  quad.0X = min(quad.xy[,quad.col[1]]) - floor(min(quad.xy[,quad.col[1]])/X.inc)*X.inc
  quad.0Y = min(quad.xy[,quad.col[2]]) - floor(min(quad.xy[,quad.col[2]])/Y.inc)*Y.inc
  
  X = c(sp.xy[,quad.col[1]], quad.xy[,quad.col[1]])
  Y = c(sp.xy[,quad.col[2]], quad.xy[,quad.col[2]])
  
  round.X     = round((X - quad.0X)/X.inc)*X.inc
  round.Y     = round((Y - quad.0Y)/Y.inc)*Y.inc
  round.id    = paste(round.X, round.Y)
  round.tab   = aggregate(data.frame(score.all), list(ID = round.id), sum)
  #round.table = table(round.id)
  #wt          = X.inc*Y.inc/as.numeric(round.table[match(round.id, names(round.table))])
  scorewt     = X.inc*Y.inc*score.all/round.tab$score.all[match(round.id, round.tab$ID)]
  scorewt
}

#------------------------------------------------------------------------------------
#  								function ppmLoopEngine
#------------------------------------------------------------------------------------

ppmlassoLoopEngine = function(Known.ppp, Unknown.ppp, quadsenv, ppmform,
                              sp.scale, n.fits, addpt = c("LoopA","LoopT", "LoopE"),
                              alpha=1, delta_max=NULL, delta_min=NULL, delta_step =NULL, num.add = NULL,
                              cov.bias=NULL, kVal =NULL, kAreaInt=NULL, maxit = 50, r=NULL,
                              verbose = TRUE, plots = FALSE, tol = 1e-6, max.it = 100, criterion="bic")
{
  #1# Fit initial point processes
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  sp_all.list = list(Known.ppp, Unknown.ppp)
  n.sp = length(unique(marks(sp_all.list[[1]])))
  mark.name = unique(marks(sp_all.list[[1]]))
  #win = owin(xrange = c(-0.5, 100.5), yrange = c(-0.5, 100.5))
  
  win.   = owin(xrange = c(min(quadsenv$X)-0.5, max(quadsenv$X)+0.5), 
                yrange = c(min(quadsenv$Y)-0.5, max(quadsenv$Y)+0.5))
  quads. = ppp(quadsenv$X, quadsenv$Y, window = win.)
  
  # prepare list of images of covariates
  cov.list = list()
  names.env = c()
  for (v in 1:(length(quadsenv)-2))
  {
    v.v = as.im(data.frame(x = quadsenv$X, y = quadsenv$Y, z = quadsenv[,v+2]))
    cov.list[[v]] = v.v
    names.env[v] = names(quadsenv)[v+2]
  }
  names(cov.list) = names.env
  
  if(all(sapply(sp_all.list, is.ppp))==TRUE){
    
    sp.xy = as.list(rep(NA, length(unique(marks(sp_all.list[[1]])))))
    for (i in 1:(length(unique(marks(sp_all.list[[1]]))))){
      sp.xy[[i]] = as.data.frame(cbind(sp_all.list[[1]][sp_all.list[[1]]$marks==mark.name[i]]$x, 
                                       sp_all.list[[1]][sp_all.list[[1]]$marks==mark.name[i]]$y))
      colnames(sp.xy[[i]]) = c("X", "Y")
    }
    
    # renaming and extracting from the list the different ppp or DF for known species
    for (i in 1:(length(unique(marks(sp_all.list[[1]]))))) {
      marks(sp_all.list[[1]][sp_all.list[[1]]$marks==mark.name[i]]) = mark.name[i]
      assign(paste(mark.name[i]), sp_all.list[[1]][sp_all.list[[1]]$marks==mark.name[i]])
    }
    
    unknown.xy=as.data.frame(cbind(sp_all.list[[2]]$x, sp_all.list[[2]]$y))
    colnames(unknown.xy)=c("X", "Y")
    marks(sp_all.list[[2]]) = "Unknown"
    assign(paste("ppp_Unknown"), sp_all.list[[2]])
    
    for (i in 1:(length(sp.xy)-1)) {
      assign(paste(mark.name[i]), sp.xy[[i]])
    }  
    
    coordsubx.list = coordsuby.list = marksub.list = list()
    for (l in 1:n.sp) {
      coordsubx.list[[l]] = sp_all.list[[1]][sp_all.list[[1]]$marks==mark.name[l]]$x
      coordsuby.list[[l]] = sp_all.list[[1]][sp_all.list[[1]]$marks==mark.name[l]]$y
      marksub.list[[l]] = rep(paste(mark.name[l]), 
                              sp_all.list[[1]][sp_all.list[[1]]$marks==mark.name[l]]$n)
      
      #l=l+1
    }
    
    all_true = ppp(x = c(unlist(coordsubx.list)), 
                   y = c(unlist(coordsuby.list)), window = win.,
                   marks = c(unlist(marksub.list)))
    
    sp.xy.all = do.call(rbind, sp.xy)
    datappp = superimpose(all_true, ppp_Unknown) #create a marked ppp 
    
    env.xy = as.data.frame(quadsenv)   #quads only needed in the context of ppp object implemented
    colnames(env.xy) = c(paste0(colnames(quadsenv)))
    
    
    # }else{
    #   if(all(sapply(sp_all.list[[1]], is.data.frame)) == TRUE){
    #     sp.xy = sp_all.list[[1]]
    #     for (i in 1:(length(sp_all.list[[1]]))){
    #       colnames(sp.xy[[i]]) = c("x", "y") 
    #     }
    #     
    #     # renaming and extracting from the list the different ppp or DF for known species
    #     for (i in 1:(length(sp_all.list[[1]])-1)) {
    #       sp.ppp = as.list(rep(NA, length(sp_all.list[[1]])-1))
    #       sp.ppp[[i]] = as.ppp(sp.xy[[i]])
    #       marks(sp_all.list[[1]][[i]]) = paste("Sp",i, sep="")
    #       assign(paste("sp", i, "_sub", sep = ""), sp.ppp[[i]])
    #     }
    #     for (i in 1:(length(sp.xy)-1)) {
    #       assign(paste("sp", i, ".xy", sep = ""), sp.xy[[i]])
    #     }
    #     
    #     sp_all.list[[2]] = as.ppp(sp_all.list[[2]], marks="Unknown")
    #     
    #     
    #     sp.xy.all = do.call(rbind, sp.xy)
    #     all_true = ppp(x = sp.xy.all$X, 
    #                    y = sp.xy.all$Y, window = win.,
    #                    marks = c(unlist(marksub.list)))
    #     
    #     unknown.xy=sp_all.list[[2]]
    #     colnames(unknown.xy)=c("X", "Y")
    #     ppp_Unknown = ppp(x=unknown.xy$X, y=unknown.xy$Y, window=win.)
    #     
    #     datappp = superimpose(all_true, ppp_Unknown) #create a marked ppp
    #     
    #     env.xy = as.data.frame(quadsenv)
    #     colnames(env.xy) = c(paste0(colnames(quadsenv)))
    #     
    #   }else{
    #     warning(paste("The format for each species coordinates and quads need to be ppp or dataframe and quadsenv needs to be a matrix of covariates with x and y corrdinates"),
    #             call.=FALSE)
    #     break
    #   }
  }
  
  X=quads.$x
  Y=quads.$y
  quad.xy = data.frame(X, Y)
  names(quad.xy) = c("X", "Y")
  iterppp = datappp


  # Fit Poisson PPMs
  ppm_sp=spenv.var=ppmprep=list()
  for (l in 1:n.sp) {
    if(is.null(r) == TRUE){
      spenv.var[[l]] = env.var(sp.xy=sp.xy[[l]], env.grid=env.xy, coord = c("X", "Y"), 
                               file.name = NA, env.scale = sp.scale)
      
      ppmprep[[l]] = ppmdat(sp.xy=sp.xy[[l]], back.xy = env.xy, sp.scale = sp.scale,
                            coord = c("X", "Y"), sp.dat = spenv.var[[l]], file.name = NA)

      
      ppm_sp[[l]]  = ppmlasso(ppmform, sp.xy[[l]], env.xy, sp.scale = sp.scale,
                           coord = c("X", "Y"), n.fits = n.fits, alpha = alpha, tol = 1e-6, 
                           max.it = 100, criterion="bic", data = ppmprep[[l]])
    }else{
      spenv.var[[l]] = env.var(sp.xy=sp.xy[[l]], env.grid=env.xy, coord = c("X", "Y"), 
                               file.name = NA, env.scale = sp.scale)
      
      ppmprep[[l]] = ppmdat(sp.xy=sp.xy[[l]], back.xy = env.xy, sp.scale = sp.scale,
                            coord = c("X", "Y"), sp.dat = spenv.var[[l]], file.name = NA)
      
      ppm_sp[[l]]  = ppmlasso(ppmform, sp.xy[[l]], env.xy, sp.scale = sp.scale,
                           coord = c("X", "Y"), n.fits = n.fits, alpha = alpha, data = ppmprep[[l]],
                           family = "area.inter", r=r, tol = 1e-6, max.it = 100, criterion="bic")
    }
  }
  
  # 
  # 
  if(is.null(cov.bias)){
    env.xy = env.xy
  }else{
    pred.env.xy = env.xy
    set.Val = cov.bias #Variables to set to a certain value
    for (b in set.Val){
      pred.env.xy[,b] = kVal*pred.env.xy[,b]
    }
  }
  
  ppp_Unknown = datappp[datappp$marks == "Unknown"]
  
  unknown.xy = as.data.frame(cbind(ppp_Unknown$x, ppp_Unknown$y))
  colnames(unknown.xy) = c("X", "Y")
  
  datamarks = marks(datappp)
  uniquemarks = unique(datamarks)
  unknown = datamarks == "Unknown"
  nclust  = length(unique(datamarks)) - 1
  names.mark = uniquemarks[uniquemarks != "Unknown"]
  
  if(is.null(cov.bias)){
    unkcov.mat = env.var(unknown.xy, env.xy, env.scale = sp.scale, coord = c("X", "Y"))
  }else{
    unkpred.mat = env.var(unknown.xy, pred.env.xy, env.scale = sp.scale, coord = c("X", "Y"))
  }
  
  #2# Compute the predicted intensities
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Predict intensity at locations where labels are hidden
  
  
  niter <- 0
  logik.old.sp = loglik.new.sp = list()
  
  for (l in 1:n.sp) {
    logik.old.sp[[l]] = 1
    loglik.new.sp[[l]] = ppm_sp[[l]]$likelihood
  }
  
  Lcrit = 1.
  Lcrit.vec = rep(NA, maxit)
  breakloop = 0
  
  is_known = which(datamarks != "Unknown")
  
  all_wts = array(data = NA, dim = c((maxit + 1), ppp_Unknown$n, n.sp))
  
  while(breakloop == 0)
  {
    niter = niter + 1
    
    #2 Compute predicted intensities
    pr_unk_ppm = list()
    for (l in 1:n.sp) {
      if(is.null(cov.bias)){
        pr_unk_ppm[[l]] = predict.ppmlasso(ppm_sp[[l]], newdata = unkcov.mat)
      }else{
        pr_unk_ppm[[l]] = predict.ppmlasso(ppm_sp[[l]], newdata = unkpred.mat)
      }
    }
    
    
    #3# Compute the membership probabilities
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    sp_preds = matrix(NA, nrow=ppp_Unknown$n, ncol = n.sp)
    for (l in 1:n.sp) {
      sp_preds[,l]=pr_unk_ppm[[l]]
    }
    max_pred = apply(sp_preds, 1, which.max)
    
    max_pred.vec = vector()
    for (l in 1:n.sp) {
      max_pred.vec[l] = c(sum(max_pred == l))
    }

    pred.check = as.vector(max_pred.vec)
    
    # Assign weights
    
    all_preds = sp_preds
    test_wts  = all_preds/apply(all_preds, 1, sum)
    
    #4# create augmented point processes accordint to different methods
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    # Set up otpion for initial weights  
    addpt <- match.arg(addpt)
    
    #4 Augment points
    if (addpt == "LoopA")
    {
      addtosp = list()
      for (l in 1:n.sp) {
        addtosp[[l]] = (1:ppp_Unknown$n)
      }
    }
    if (addpt == "LoopT")
    {
      addtosp = list()
      for (l in 1:n.sp) {
        addtosp[[l]] = which(test_wts[,l] > delta_max)
      }
    }
    if (addpt == "LoopE")
    {
      if(num.add > ppp_Unknown$n/n.sp)
      {
        print("Impossible to add so many points, the highest possible number will be used instead")
        num.add = floor(ppp_Unknown$n/n.sp)
      }
      else
      {
        num.add = num.add
      }
      
      add_max = apply(test_wts, 2, sort, decreasing = TRUE)[num.add,]
      addtosp = list()
      for (l in 1:n.sp) {
        addtosp[[l]] = if(anyNA(ppp_Unknown$x[test_wts[,l] >= add_max[l]]) == TRUE) integer() else which(test_wts[,l] >= add_max[l])
      }
    }
    
    sp_aug = list()
    for (l in 1:n.sp) {
      
      sp.tmp = sp_all.list[[1]][sp_all.list[[1]]$marks == mark.name[l]]
      sp_aug[[l]] = data.frame(X = c(sp.tmp$x, ppp_Unknown$x[addtosp[[l]]]),
                               Y = c(sp.tmp$y, ppp_Unknown$y[addtosp[[l]]])) # add unknown points to known points of species 1
      
    }

    # compute quadrature weights for each augmented species
    #scores for species with known label (weight =1) and for the new obs (test_wts)
    sp_wts = list()
    for (l in 1:n.sp) {
      sp.tmp = sp_all.list[[1]][sp_all.list[[1]]$marks == mark.name[l]]
      sp_wts[[l]] = scoreweights(sp_aug[[l]], quad.xy, scores = c(rep(1, sp.tmp$n), test_wts[addtosp[[l]], l])) # generate quad weights for augmented species l
    }
    
    #6# Fit new point processes using the augmented points patterns and the quadrature weights
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    # Fit Poisson PPMs with the new weigths calculated
    # Replace quadrature weights with those calculated with the scoreweights function
    # This is necessary because spatstat's quadscheme function treats all points the same.
    # We want to treat the points with unknown labels as "fractional" points with weights coming from the single-species PPMs
    
    ppm_sp = spenv.var = ppmprep = list()
    fitsp = list()
    for (l in 1:n.sp) {
      if(is.null(r) == TRUE){
        spenv.var[[l]] = env.var(sp.xy=sp_aug[[l]], env.grid=env.xy, coord = c("X", "Y"), 
                                 file.name = NA, env.scale = sp.scale)
        
        ppmprep[[l]] = ppmdat(sp.xy=sp_aug[[l]], back.xy = env.xy, sp.scale = sp.scale,
                              coord = c("X", "Y"), sp.dat = spenv.var[[l]], file.name = NA)
      
        
        ppm_sp[[l]]  = ppmlasso(ppmform, sp_aug[[l]], env.xy, sp.scale = sp.scale,
                                coord = c("X", "Y"), n.fits = n.fits, data = ppmprep[[l]],
                                ob.wt = sp_wts[[l]], alpha = alpha, tol = 1e-6,
                                max.it = 100, criterion="bic")
      }else{
        spenv.var[[l]] = env.var(sp.xy=sp_aug[[l]], env.grid=env.xy, coord = c("X", "Y"), 
                                 file.name = NA, env.scale = sp.scale)
        
        ppmprep[[l]] = ppmdat(sp.xy=sp_aug[[l]], back.xy = env.xy, sp.scale = sp.scale,
                              coord = c("X", "Y"), sp.dat = spenv.var[[l]], file.name = NA)
        
        
        ppm_sp[[l]]  = ppmlasso(ppmform, sp_aug[[l]], env.xy, sp.scale = sp.scale,
                                coord = c("X", "Y"), n.fits = n.fits,
                                ob.wt = sp_wts[[l]], alpha = alpha,
                                family = "area.inter", r=r, tol = 1e-6,
                                max.it = 100, criterion="bic", data = ppmprep[[l]])
      }
      
      if(is.null(cov.bias)){
        fitsp[[l]] = predict.ppmlasso(ppm_sp[[l]], newdata = env.xy)
      }else{
        fitsp[[l]] = predict.ppmlasso(ppm_sp[[l]], newdata = pred.env.xy)
      }
      
      
      if (plots == TRUE){
        plotfit(ppm_sp[[l]], main="plot - fitsp1")
      }
    }
    
    
    # to get the weights
    if(is.null(cov.bias)){
      unkcov.mat = env.var(unknown.xy, env.xy, env.scale = sp.scale, coord = c("X", "Y"))
      
      predint = matrix(NA, sum(unknown), nclust)
      for (l in 1:n.sp) {
        predint[,l] = predict.ppmlasso(ppm_sp[[l]], newdata = unkcov.mat)
      }
      
    }else{
      predint = matrix(NA, sum(unknown), nclust)
      for (l in 1:n.sp) {
        predint[,l] = predict.ppmlasso(ppm_sp[[l]], newdata = unkpred.mat)
      }
    }
    
    pl.weights = predint/apply(predint, 1, sum)
    
    datamarks = marks(datappp)
    uniquemarks = unique(datamarks)
    colmarks = uniquemarks
    unknown = datamarks == "Unknown"
    colmarks  = colmarks[-which(colmarks == "Unknown")]
    nclust  = length(unique(datamarks)) - 1
    colnames(pl.weights) = colmarks
    
    # update the membership weights for the iteration #for ppmlasso we already consider only the unknowm points
    test_wts = pl.weights
    
    itermarks = colnames(pl.weights)[apply(pl.weights, 1, which.max)] # assign marks based on new weights
    
    iterppp = ppp_Unknown
    marks(iterppp) = as.factor(itermarks)
    
    all.weights = rbind(matrix(0, nrow = datappp[-which(datappp$marks=="Unknown")]$n,
                               ncol = nclust), pl.weights)
    
    #all.weights[datamarks != "Unknown",] = rep(0, nclust)
    
    for (i in 1:nclust)
    {
      rowfill = which(datamarks == colmarks[i])
      all.weights[rowfill, i] = 1
    }
    
    
    p = apply(all.weights, 2, sum)/sum(apply(all.weights, 2, sum))
    
    #7# Stopping criterion
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    # evaluate marginal loglikelihood
    
    loglik.old.sp = list()
    for (l in 1:n.sp) {
      loglik.old.sp[[l]] = loglik.new.sp[[l]]
      loglik.new.sp[[l]] = ppm_sp[[l]]$likelihood
      
      DiffL = sum(abs(loglik.new.sp[[l]] - loglik.old.sp[[l]]))
    }
    
    sumL.new = abs(do.call(sum, loglik.new.sp))
    
    Lcrit = DiffL/sumL.new 
    Lcrit.vec[niter]=Lcrit
    
    # break loop
    
    if (Lcrit < tol)
    {
      breakloop = 1
    }
    
    if (niter == maxit)
    {
      breakloop = 1
    }
    
    if (addpt == "LoopT")
    {
      delta_max = delta_max - delta_step
      if (delta_max < delta_min)
      {
        breakloop = 1
      }
    }
    if (addpt == "LoopE")
    {
      num.add = num.add + 1
      if (num.add > ppp_Unknown$n/n.sp)
      {
        breakloop = 1
      }
    }
    
    if(verbose) {
      cat("\nEstimated parameters:\n")
      cat(paste("p [cluster] =", signif(p, 5), "\n"))
      
    }
    
  }
  
  
  # Compute final predicted intensities
  pr_ppm = list()
  for (l in 1:n.sp) {
    if(is.null(cov.bias)){
      unkcov.mat = env.var(unknown.xy, env.xy, env.scale = sp.scale, coord = c("X", "Y"))
      
      pr_ppm[[l]] = predict.ppmlasso(ppm_sp[[l]], newdata = unkcov.mat)
      
    }else{
      pr_ppm[[l]] = predict.ppmlasso(ppm_sp[[l]], newdata = unkpred.mat)
    }
    
  }
  
  #3 Compute final membership probabilities
  all_preds = matrix(NA, nrow=ppp_Unknown$n, ncol = n.sp)
  for (l in 1:n.sp) {
    all_preds[,l]=pr_ppm[[l]]
  }
  
  test_wts  = all_preds/apply(all_preds, 1, sum)
  all_wts[niter + 1,,] = as.matrix(test_wts)
  
  #
  if (plots == TRUE)
  {
    par(xpd=NA)
    known.marks = unique(iterppp$marks)
    plot(x=seq_along(pl.weights[,1]), ypl.weights[,1], col = "orange", pch=16, ylim=c(0,1),
         xlab="observations", ylab="weight")
    for (l in 2:nclust) {
      colvect=c("purple", "turquoise3", "darkred", "green", "brown")[1:nclust-1]
      points(x=seq_along(pl.weights[,l]), y=pl.weights[,l], col = colvect, pch=16, ylim=c(0,1))
      l =l + 1
      legend(110,1, c(known.marks), col = c("orange", colvect),
             pch = 16, xjust = 1, yjust = 0, merge = FALSE)
    }

  }
  
  
  return(list(z = round(p, digits = 4),
              probs = p,
              New_weights = pl.weights,
              all_wts = all_wts,
              ppm_sp = ppm_sp,
              fitsp = fitsp,
              ppmlasso.pred = predint,
              sp_aug = sp_aug,
              itermarks
              #hist=if(plothist) H else NULL
              # plot(x=seq_along(Weight.df$Sp1), y=Weight.df$Sp1, col = "orange", pch=16, ylim=c(0,1),
              #      xlab="observations", ylab="weight"),
              # points(x=seq_along(Weight.df$Sp2), y=Weight.df$Sp2, col = "purple", pch=18, ylim=c(0,1)),
              # points(x=seq_along(Weight.df$Sp3), y=Weight.df$Sp3, col = "Turquoise3", pch=17, ylim=c(0,1)),
              # legend(1,1, c("sp1", "sp2", "sp3"), col = c("orange", "purple", "Turquoise3"),
              #        pch = c(16, 18, 17), xjust = 1, yjust = 0, merge = FALSE)
  ))
}





##------------------------------------------------------------------------------
#              functions and measures of performance
#------------------------------------------------------------------------------

###----------------- IMSE
IMSE = function(mu1, mu2, fun = "Else", mu.min = 1.e-5, rescale = TRUE)
{
  mu1.use = mu1
  mu1.use[mu1.use < mu.min] = mu.min
  mu2.use = mu2
  mu2.use[mu2.use < mu.min] = mu.min
  
  if (rescale == TRUE)
  {
    mu2.use = mu2.use*mean(mu1.use)/mean(mu2.use)
  }
  
  if (fun == "Else")
  {
    mu1.use = mu1.use
    mu2.use = mu2.use
  }
  
  if (fun == "log")
  {
    mu1.use = log(mu1.use)
    mu2.use = log(mu2.use)
  }
  if (fun == "sqrt")
  {
    mu1.use = sqrt(mu1.use)
    mu2.use = sqrt(mu2.use)
  }
  imse = sum((mu1.use - mu2.use)^2)/(var(mu2.use))
  imse
}

###----------------- corint for sumcor calculation

corint = function(mu1, mu2, fun = "Else", method=c("pearson", "kendall", "spearman"), mu.min = 1.e-5)
{
  mu1.use = mu1
  mu1.use[mu1.use < mu.min] = mu.min
  mu2.use = mu2
  mu2.use[mu2.use < mu.min] = mu.min
  
  if (fun == "log")
  {
    mu1.use = log(mu1.use)
    mu2.use = log(mu2.use)
  }
  
  if (fun == "Else")
  {
    mu1.use = mu1.use
    mu2.use = mu2.use
  }
  
  # Set up otpion for initial weights  
  addpt <- match.arg(method)
  if (method == "pearson"){
    corint.pea = cor(mu1.use, mu2.use, method = "pearson")
    return(corint.pea)
  }
  
  if(method == "kendall"){
    corint.kend = cor(mu1.use, mu2.use, method = "kendall")
    return(corint.kend)
  }
  
  if(method == "spearman"){
    corint.spea = cor(mu1.use, mu2.use, method = "spearman")
    return(corint.spea)
  }
  
}

###----------------- RSS

RSS = function(weightmatrix, truemarks)
{
  mark.cols = match(truemarks, colnames(weightmatrix))
  correctweights = weightmatrix[cbind(seq_along(mark.cols), mark.cols)]
  RSS = sum((correctweights - 1)^2)
  RSS
}

###----------------- Accuracy

Accuracy = function(all_true, New_weights, test_labels, n.sp){
  W.max = apply(New_weights[(1:nrow(New_weights)),], 1, max)
  C.id = apply(New_weights[(1:nrow(New_weights)),], 1, which.max)
  
  indiv_testlab = as.data.frame(cbind(test_labels, C.id, W.max))
  
  levels(indiv_testlab$test_labels) <- c(unique(test_labels))
  
  # New method for accuracy
  levels(indiv_testlab$test_labels)
  levels(factor(indiv_testlab$C.id))
  for (i in 1:n.sp) {
    levels(indiv_testlab$test_labels)[levels(indiv_testlab$test_labels)==mark.name[i]] <- paste(i, sep = "")
    
  }
  
  acc_tab <- table(indiv_testlab$test_labels,
                   factor(indiv_testlab$C.id, levels=1:n.sp))
  
  #dim(acc_tab)
  
  #CM.acc = confusionMatrix(acc_tab)
  m.acc= acc_tab
  
  # some usuful calc
  n = sum(m.acc) # number of instances
  nc = nrow(m.acc) # number of classes
  diag = diag(m.acc) # number of correctly classified instances per class 
  rowsums = apply(m.acc, 1, sum) # number of instances per class
  colsums = apply(m.acc, 2, sum) # number of predictions per class
  p = rowsums / n # distribution of instances over the actual classes
  q = colsums / n # distribution of instances over the predicted classes
  
  # accuracy measure
  accuracy = sum(diag) / n 
  accuracy 
  
}


