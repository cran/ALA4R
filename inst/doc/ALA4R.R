## ----setup, include=FALSE------------------------------------------------
library(knitr)
## center images: this only works for html output format
if (grepl("html",opts_knit$get("rmarkdown.pandoc.to")))
    opts_chunk$set(out.extra='style="display:block; margin: auto"', fig.align="center")

## ----eval=FALSE----------------------------------------------------------
#  install.packages("ALA4R")

## ----eval=FALSE----------------------------------------------------------
#  install.packages("devtools")
#  library(devtools)
#  install_github("AtlasOfLivingAustralia/ALA4R")

## ----eval=FALSE----------------------------------------------------------
#  install.packages(c("stringr","sp"))

## ----eval=FALSE----------------------------------------------------------
#  install.packages("data.table")

## ------------------------------------------------------------------------
library(ALA4R)

## ----eval=FALSE----------------------------------------------------------
#  ala_config(cache_directory <- file.path("c:","mydata","ala_cache")) ## Windows

## ----eval=FALSE----------------------------------------------------------
#  ala_config(cache_directory <- file.path("~","mydata","ala_cache")) ## Linux

## ----eval=FALSE----------------------------------------------------------
#  setHook(packageEvent("ALA4R", "attach"), function(...) ala_config(cache_directory=file.path("~","mydata","ala_cache")))

## ----eval=FALSE----------------------------------------------------------
#  ala_config(caching="off")

## ----eval=FALSE----------------------------------------------------------
#  ala_config()

## ----eval=FALSE----------------------------------------------------------
#  ala_config(verbose=TRUE)

## ----eval=FALSE----------------------------------------------------------
#  ala_config(download_reason_id=your_reason_id)

## ----eval=FALSE----------------------------------------------------------
#  ala_config(warn_on_empty=TRUE)

## ------------------------------------------------------------------------
to_install <- c("plyr","jpeg","phytools","ape","vegan","mgcv","geosphere","maps","mapdata","maptools")
to_install <- to_install[!sapply(to_install,requireNamespace,quietly=TRUE)]
if(length(to_install)>0) install.packages(to_install,repos="http://cran.us.r-project.org")

## ------------------------------------------------------------------------
library(plyr)

## ----message=FALSE-------------------------------------------------------
library(ape)
library(phytools)

## ------------------------------------------------------------------------
sx <- search_fulltext("penguins")
sx$data[,c("name","rank","commonName")]

## ----message=FALSE-------------------------------------------------------
tx <- taxinfo_download("rk_family:SPHENISCIDAE",fields=c("guid","rk_genus","scientificName","rank"))
tx <- tx[tx$rank %in% c("species","subspecies"),] ## restrict to species and subspecies

## ----results='hide',fig.width=6,fig.height=6-----------------------------
## as.phylo requires the taxonomic columns to be factors
temp <- colwise(factor, c("genus","scientificName"))(tx)
## create phylo object of Scientific.Name nested within Genus
ax <- as.phylo(~genus/scientificName,data=temp)
plotTree(ax,type="fan",fsize=0.7) ## plot it

## ------------------------------------------------------------------------
s <- search_guids(tx$guid)

## ------------------------------------------------------------------------
imfiles <- sapply(s$thumbnailUrl,function(z){
  ifelse(!is.na(z),ALA4R:::cached_get(z,type="binary_filename"),"")
})

## ----results='hide', dev.args=if (.Platform$OS.type=="unix") list(png=list(colortype="pseudo.cube")) else list()----
plotTree(ax,type="fan",ftype="off") ## plot tree without labels
tr <- get("last_plot.phylo",envir = .PlotPhyloEnv) ## get the tree plot object
## add each image
library(jpeg)
for (k in which(nchar(imfiles)>0))
        rasterImage(readJPEG(imfiles[k]),tr$xx[k]-1/10,tr$yy[k]-1/10,tr$xx[k]+1/10,tr$yy[k]+1/10)

## ----eval=FALSE----------------------------------------------------------
#  library(maptools)
#  shape_filename <- ALA4R:::cached_get("https://data.environment.sa.gov.au/NatureMaps/Documents/CONSERVATION_Npwsa_Reserves_shp.zip", type="binary_filename")
#  unzip(shape_filename,exdir=ala_config()$cache_directory) ## unzip this file
#  shape <- readShapePoly(file.path(ala_config()$cache_directory, "CONSERVATION_NpwsaReserves.shp"))
#  ## extract just the Morialta Conservation Park polygon
#  shape <- shape[shape$RESNAME=="Morialta",]

## ----include=FALSE-------------------------------------------------------
library(maptools)
#shape_filename <- ALA4R:::cached_get("https://data.environment.sa.gov.au/NatureMaps/Documents/CONSERVATION_Npwsa_Reserves_shp.zip",type="binary_filename")
#unzip(shape_filename,exdir=ala_config()$cache_directory) ## unzip this file
#shape <- readShapePoly(file.path(ala_config()$cache_directory,"CONSERVATION_NpwsaReserves.shp"))
#shape <- shape[shape$RESNAME=="Morialta",] ## extract just the Morialta Conservation Park polygon
#save(list=c("shape"),file="vignette_morialta_shape.RData")
load("vignette_morialta_shape.RData") ## use local file to speed up vignette and avoid download each build

## ----eval=FALSE----------------------------------------------------------
#  library(rgeos)
#  wkt <- writeWKT(shape)

## ------------------------------------------------------------------------
lonlat <- shape@polygons[[1]]@Polygons[[1]]@coords ## extract the polygon coordinates
## extract the convex hull of the polygon to reduce the length of the WKT string
temp <- chull(lonlat)
lonlat <- lonlat[c(temp,temp[1]),]
## create WKT string
wkt <- paste("POLYGON((",paste(apply(lonlat,1,function(z) paste(z,collapse=" ")),collapse=","),"))",sep="")

## ------------------------------------------------------------------------
x <- specieslist(wkt=wkt,fq="state_conservation:*")
(head(arrange(x,desc(occurrenceCount)),20))

## ------------------------------------------------------------------------
x <- occurrences(taxon="Amblyornis newtonianus", download_reason_id=10)
summary(x)

## ----eval=FALSE----------------------------------------------------------
#  occurrences_plot(x,qa="fatal")

## ----eval=FALSE----------------------------------------------------------
#  library(leaflet)
#  ## drop any records with missing lat/lon values
#  x$data <- x$data[!is.na(x$data$longitude) & !is.na(x$data$latitude),]
#  xa <- check_assertions(x)
#  ## columns of x corresponding to a fatal assertion
#  x_afcols <- which(names(x$data) %in% xa$occurColnames[xa$fatal])
#  ## rows of x that have a fatal assertion
#  x_afrows <- apply(x$data[,x_afcols],1,any)
#  ## which fatal assertions are present in this data?
#  these_assertions <- names(x$data)[x_afcols]
#  ## make a link to th web page for each occurrence
#  popup_link <- paste0("<a href=\"http://biocache.ala.org.au/occurrences/",x$data$id,"\">Link to occurrence record</a>")
#  ## colour palette
#  pal <- c(sub("FF$","",heat.colors(length(these_assertions))))
#  ## map each data row to colour, depending on its assertions
#  marker_colour <- rep("#00FF00",nrow(x$data))
#  for (k in 1:length(these_assertions)) marker_colour[x$data[,x_afcols[k]]] <- pal[k]
#  ## blank map, with imagery background
#  m <- addProviderTiles(leaflet(),"Esri.WorldImagery")
#  ## add markers
#  m <- addCircleMarkers(m,x$data$longitude,x$data$latitude,col=marker_colour,popup=popup_link)
#  print(m)

## ----message=FALSE-------------------------------------------------------
library(vegan)
library(mgcv)
library(geosphere)

## ----eval=FALSE----------------------------------------------------------
#  wkt <- "POLYGON((152.5 -35,152.5 -32,140 -32,140 -35,152.5 -35))"
#  x <- occurrences(taxon="family:Fabaceae",wkt=wkt,qa="none",download_reason_id=10)
#  x <- x$data ## just take the data component

## ----eval=FALSE----------------------------------------------------------
#  x$longitude <- round(x$longitude*2)/2
#  x$latitude <- round(x$latitude*2)/2

## ----eval=FALSE----------------------------------------------------------
#  ## discard genus- and higher-level records
#  xsub <- x$rank %in% c("species","subspecies","variety","form","cultivar")
#  unames <- unique(x[xsub,]$scientificName) ## unique names
#  ull <- unique(x[xsub,c("longitude","latitude")])
#  xgridded <- matrix(NA,nrow=nrow(ull),ncol=length(unames))
#  for (uli in 1:nrow(ull)) {
#      lidx <- xsub & x$longitude==ull[uli,]$longitude & x$latitude==ull[uli,]$latitude
#      xgridded[uli,] <- as.numeric(unames %in% x[lidx,]$scientificName)
#  }
#  xgridded <- as.data.frame(xgridded)
#  names(xgridded) <- unames
#  xgridded <- cbind(ull,xgridded)

## ----include=FALSE-------------------------------------------------------
## load data from a local copy so that vignette building doesn't require downloading a big chunk of data and slow sites-by-species processing
## this file generated by running the above unevaluated code blocks, then
## save(list=c("wkt","xgridded"),file="vignette_fabaceae.RData")
load("vignette_fabaceae.RData")

## ------------------------------------------------------------------------
plot(xgridded$longitude,apply(xgridded[,-c(1:2)],1,sum),ylab="Richness",
  xlab="Longitude",pch=20,col="grey25")

## ------------------------------------------------------------------------
D <- vegdist(xgridded[,-c(1:2)],'bray') ## Bray-Curtis dissimilarity
Dm <- as.matrix(D) ## convert to a matrix object
## calculate geographic distance from longitude and latitude
Dll <- apply(xgridded[,1:2],1,function(z){distVincentySphere(z,xgridded[,1:2])})
closeidx <- Dll>0 & Dll<100e3 ## find grid cells within 100km of each other
## create a matrix of longitudes that matches the size of the pairwise-D matrices
temp <- matrix(xgridded$longitude,nrow=nrow(xgridded),ncol=nrow(xgridded))
## plot dissimilarity as a function of transect position
plot(temp[closeidx],Dm[closeidx],xlab="Longitude",ylab="Dissimilarity",pch=20,col="grey85")
## add smooth fit via gam()
fit <- gam(d~s(tp,k=7),data=data.frame(tp=temp[closeidx],d=Dm[closeidx]))
tpp <- seq(from=min(xgridded$longitude),to=max(xgridded$longitude),length.out=100)
fitp <- predict(fit,newdata=data.frame(tp=tpp))
lines(tpp,fitp,col=1)

## ----fig.width=6,fig.height=6--------------------------------------------
cl <- hclust(D,method="ave") ## UPGMA clustering
plot(cl) ## plot dendrogram
grp <- cutree(cl,20) ## extract group labels at the 20-group level
## coalesce small (outlier) groups into a single catch-all group
sing <- which(table(grp)<5)
grp[grp %in% sing] <- 21 ## put these in a new combined group
grp <- sapply(grp,function(z)which(unique(grp)==z)) ## renumber groups
## plot
with(xgridded,plot(longitude,latitude,pch=21,col=grp,bg=grp))
## or slightly nicer map plot
library(maps)
library(mapdata)
map("worldHires","Australia", xlim=c(105,155), ylim=c(-45,-10), col="gray90", fill=TRUE)
thiscol <- c("#1f77b4","#ff7f0e","#2ca02c","#d62728","#9467bd","#8c564b","#e377c2","#7f7f7f","#bcbd22","#17becf") ## colours for clusters
with(xgridded,points(longitude,latitude,pch=21,col=thiscol[grp],bg=thiscol[grp],cex=0.75))

