
# load each dataset
# let's try clustering 88141 basker which has some clear differences
setwd('~/Documents/WHOI/RData/BaskingSharks/2008/88141/')
data <- read.table('88141-Histos.csv', sep = ',', header = T, blank.lines.skip = F)
data <- data[,c(7,16:27)]
x <- data

formatCluster <- function(histos, locs, add){

  #' takes in -Histos.csv that's been read into R with read.wc
  #' checks for timing and bin extent
  #' formats accordingly:
  #' 1) temporal averaging (if req'd)
  #' 2) bin combination (if req'd)
  #'
  #' @param
  #'
  #' @return matrix of  days x bins
  #'
  #'

  # what time scheme do we have?
  # iterate through udates using as.Date
  histos$dateOnly <- as.Date(histos$date, format = findDateFormat(histos$date))
  udates <- unique(histos$dateOnly)

  for(t in 1:length(udates)){
    h.sel <- histos[which(histos$dateOnly == udates[t]),]
    len <- length(h.sel[,1])

    if(len > 1){
      # average bins
    } else if(len == 1){

    } else{
      stop(paste('Error in averaging at ', udates[t], '.', sep = ''))
    }

  }

  # is temporal averaging required

  # if so, do it and to what duration?

  # is averaging to certain bin extent req'd?
    # check that bins match OR load in metadata sheet indicating bin ranges

  # bin averaging (from -> to)

  # what relevant attributes do we keep for each individual to use later?
  #   -- need date, ptt, lat/lon,
  # combine  locs with histos
  if(any(is.na(histos$lon))){
    warning('Some locations are NA after merging with histos data.')
  }

  if(!is.null(add)){
    clust <- rbind(add, clust)
  }

  return(clust)

}


sharkCluster <- function(trim = 0.1, clustMethod = 'manhattan', aggMethod = 'ave'){

  #' small function to automate shark clustering
  #' @param trim is numeric threshold below which % time in given depth bin is
  #'        marked as 0 (Sal says this elminates lot of "noise" as sharks transition
  #'        between bins)
  #' @param clustMethod is character indicating which clustering method to use. see ?dist
  #' @param aggMethod is character indicating which aggregation method to use. see ?hclust
  #'
  #'
  #' @return
  #'
  #' need to add 1) pvclust for significance of clusters (uses bootstrapping)
  #'             2) plotting


  # do some trimming
  x[x < trim] = 0

  # calc dist
  d <- dist(x, clustMethod)

  # remove NA's
  # hclust can't handle NA apparently
  d[is.na(d)] <- 0

  # and cluster
  z <- hclust(d, aggMethod)

}





#Here is what you need to do :
# Compile your data into a single array with dimensions (# of days by # of bins of TAD)  e.g. (4000 x 12) - days in my case is 24hr binnned data for all sharks. Call it X.
# Then compile another array (or individual vectors) with the same number of rows additional  e.g columns: ID , date, sex, lat, long, etc
# the number of bins and length of your data is arbitrary

# let's try clustering 88141 basker which has some clear differences
setwd('~/Documents/WHOI/RData/BaskingSharks/2008/88141/')
data <- read.table('88141-Histos.csv', sep = ',', header = T, blank.lines.skip = F)
x <- data[,c(16:27)]
meta <- data[,c(2,6,7)] # should add lat, lon, etc...

# %%set low usage bins = 0; I found this really helped define the differences between clusters , this could be time th sharks spent moving from one depth bin to another but did not spend significant time there
# %%   X(X<0.1) = 0
x[x < 0.1] = 0
# %%cluster function parameters:
# %%     Y = pdist(X,'cityblock');
# %%   Z = linkage(Y,'average');
d <- dist(x, 'manhattan')

# hclust can't handle NA apparently
d[is.na(d)] <- 0

z <- hclust(d, 'ave')

# or use pvclust here to assign p values to # of clusters
# pvclust inputs for clustering data are diff from other methods. your data is typically clusterable groups (rows) x cluster characteristics (cols)
# for example, mtcars data has rownames as model of car and cols as attributes like engine size and weight
# you need to transpose this format of data
data(mtcars)
result <- pvclust(t(mtcars), method.dist="correlation", method.hclust="average", nboot=1000)
plot(result)
pvrect(result, alpha=0.95)

result.x <- pvclust(t(x), method.dist="correlation", method.hclust="average", nboot=1000)
plot(result.x)
pvrect(result.x, alpha=0.95)
pvpick(result.x)

# %%%Then choose number of clusters. - this part is exploratory and you should try lots of different numbers - read up on criteria for deciding how many to go with. I went with the fewest number with the greatest separation.
dgram <- as.dendrogram(z)
[H, T, Perm] = dendrogram(z,9); #%% in this case 9 clusters
XS = X(T,:);

# %%% Plot
set(gca, 'xtick', [])
imagesc(flipud((X(Perm,:)'))); #'
                #shading flat

                # %%% then explore your data by linking you cluster number assignment to your variables of lat long date, sex etc…


                # http://www.r-tutor.com/gpu-computing/clustering/hierarchical-cluster-analysis
                # https://stat.ethz.ch/R-manual/R-devel/library/stats/html/hclust.html
                # http://www.sigmath.es.osaka-u.ac.jp/shimo-lab/prog/pvclust/
