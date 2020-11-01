## ----setup, include=FALSE---------------------------------------------------------------------------------------------
library(knitr)
options(width=120)

## ----eval=FALSE-------------------------------------------------------------------------------------------------------
#  install.packages("ALA4R")

## ----eval=FALSE-------------------------------------------------------------------------------------------------------
#  install.packages("devtools")
#  devtools::install_github("AtlasOfLivingAustralia/ALA4R")

## ---------------------------------------------------------------------------------------------------------------------
library(ALA4R)

## ----eval=FALSE-------------------------------------------------------------------------------------------------------
#  ala_config(cache_directory="c:/mydata/ala_cache") ## use forward slashes, not \

## ----eval=FALSE-------------------------------------------------------------------------------------------------------
#  ala_config(cache_directory="~/mydata/ala_cache")

## ----eval=FALSE-------------------------------------------------------------------------------------------------------
#  setHook(packageEvent("ALA4R", "attach"), function(...)
#      ala_config(cache_directory=file.path("~", "mydata", "ala_cache")))

## ----eval=FALSE-------------------------------------------------------------------------------------------------------
#  ala_config(caching="off")

## ----eval=FALSE-------------------------------------------------------------------------------------------------------
#  ala_config()

## ----eval=FALSE-------------------------------------------------------------------------------------------------------
#  ala_config(verbose=TRUE)

## ----eval=FALSE-------------------------------------------------------------------------------------------------------
#  ala_config(download_reason_id=your_reason_id)

## ----eval=FALSE-------------------------------------------------------------------------------------------------------
#  ala_config(warn_on_empty=TRUE)

## ----message=FALSE----------------------------------------------------------------------------------------------------
to_install <- c("ape", "dplyr", "ggplot2", "jpeg", "maps", "mapdata",
                "maptools", "phytools", "tidyr", "vegan")
to_install <- to_install[!sapply(to_install, requireNamespace, quietly=TRUE)]
if(length(to_install)>0)
    install.packages(to_install, repos="http://cran.us.r-project.org")

## In these examples we use the `dplyr` package to help with data manipulation.
library(dplyr)

## ----message=FALSE----------------------------------------------------------------------------------------------------
library(ape)
library(phytools)

## ---------------------------------------------------------------------------------------------------------------------
sx <- search_fulltext("penguins")
sx$data %>% dplyr::select(name, rank, commonName, family)

## ---------------------------------------------------------------------------------------------------------------------
tx <- taxinfo_download("rk_family:SPHENISCIDAE", fields=c("guid", "rk_genus", "scientificName", "rank"))
  
## keep only species and subspecies records
tx <- tx %>% dplyr::filter(rank %in% c("species","subspecies"))

## ---------------------------------------------------------------------------------------------------------------------
## as.phylo requires the taxonomic columns to be factors
tx <- tx %>% mutate_all(as.factor)

## create phylo object of Scientific.Name nested within Genus
ax <- as.phylo(~genus/scientificName, data=tx)

plotTree(ax, type="fan", fsize=0.7) ## plot it

## ---------------------------------------------------------------------------------------------------------------------
  s <- search_guids(tx$guid)

## ---------------------------------------------------------------------------------------------------------------------
imfiles <- sapply(s$thumbnailUrl, function(z) {
  ifelse(!is.na(z), ALA4R:::cached_get(z, type="binary_filename"), "")
})

## ----results="hide", fig.width=7.5, fig.height=7.5, dev.args=if (.Platform$OS.type=="unix") list(png=list(colortype="pseudo.cube")) else list()----
## plot tree without labels
plotTree(ax, type="fan", ftype="off")

## get the tree plot object
tr <- get("last_plot.phylo", envir = .PlotPhyloEnv)

## add each image
library(jpeg)
for (k in which(nchar(imfiles)>0))
        rasterImage(readJPEG(imfiles[k]), tr$xx[k]-1/10, tr$yy[k]-1/10, tr$xx[k]+1/10, tr$yy[k]+1/10)

## ----eval=FALSE-------------------------------------------------------------------------------------------------------
#  library(maptools)
#  shape_filename <- "https://data.environment.sa.gov.au/NatureMaps/Documents/CONSERVATION_Npwsa_Reserves_shp.zip"
#  
#  ## download to temporary file and unzip it to the ALA4R cache directory
#  tf <- tempfile()
#  download.file(shape_filename, tf)
#  unzip(tf, exdir=ala_config()$cache_directory)
#  
#  shape <- readShapePoly(file.path(ala_config()$cache_directory, "CONSERVATION_NpwsaReserves.shp"))
#  
#  ## extract just the Morialta Conservation Park polygon
#  shape <- shape[shape$RESNAME=="Morialta", ]

## ----include=FALSE----------------------------------------------------------------------------------------------------
## use local file to speed up vignette and avoid download each build
library(maptools)
## this is the code to fetch that data and save it locally
#shape_filename <- "https://data.environment.sa.gov.au/NatureMaps/Documents/CONSERVATION_Npwsa_Reserves_shp.zip"
#tf <- tempfile()
#download.file(shape_filename, tf)
#unzip(tf, exdir=ala_config()$cache_directory)
#shape <- maptools::readShapePoly(file.path(ala_config()$cache_directory, "CONSERVATION_NpwsaReserves.shp"))
#shape <- shape[shape$RESNAME=="Morialta", ]
#saveRDS(shape, file="vignette_morialta_shape.rds")
shape <- readRDS("vignette_morialta_shape.rds")

## ----eval=FALSE-------------------------------------------------------------------------------------------------------
#  library(rgeos)
#  wkt <- writeWKT(shape)

## ---------------------------------------------------------------------------------------------------------------------
lonlat <- shape@polygons[[1]]@Polygons[[1]]@coords ## extract the polygon coordinates

## extract the convex hull of the polygon to reduce the length of the WKT string
temp <- chull(lonlat)
lonlat <- lonlat[c(temp, temp[1]), ]

## create WKT string
## first join each lon-lat coordinate pair
temp <- apply(lonlat, 1, function(z) paste(z, collapse=" "))

## now build the WKT string
wkt <- paste("POLYGON((", paste(temp, collapse=","), "))", sep="")

## ----eval=FALSE-------------------------------------------------------------------------------------------------------
#  specieslist(wkt=wkt, fq="state_conservation:*") %>%
#      dplyr::arrange(desc(occurrenceCount)) %>%
#      dplyr::select(speciesName, commonName, occurrenceCount) %>%
#      head(10)

## ----echo=FALSE-------------------------------------------------------------------------------------------------------
tryCatch({
specieslist(wkt=wkt, fq="state_conservation:*") %>%
    dplyr::arrange(desc(occurrenceCount)) %>%
    dplyr::select(speciesName, commonName, occurrenceCount) %>%
    head(10)
}, error = function(e) { print(e$message)})

## ----eval=FALSE-------------------------------------------------------------------------------------------------------
#  x <- occurrences(taxon="taxon_name:\"Amblyornis newtonianus\"", download_reason_id="testing", email="ala4r@ala.org.au")
#  summary(x)

## ----echo=FALSE-------------------------------------------------------------------------------------------------------
tryCatch({
  x <- occurrences(taxon="taxon_name:\"Amblyornis newtonianus\"", download_reason_id="testing", email="ala4r@ala.org.au")
  summary(x)
},warning = function(w) {print(w$message)}
 ,error = function(e) { print(e$message)})

## ----eval=FALSE-------------------------------------------------------------------------------------------------------
#  occurrences_plot(x, qa="fatal")

## ----eval=FALSE-------------------------------------------------------------------------------------------------------
#  library(leaflet)
#  
#  ## drop any records with missing lat/lon values
#  x$data <- x$data[!is.na(x$data$longitude) & !is.na(x$data$latitude), ]
#  xa <- check_assertions(x)
#  
#  ## columns of x corresponding to a fatal assertion
#  x_afcols <- which(names(x$data) %in% xa$occurColnames[xa$fatal])
#  
#  ## rows of x that have a fatal assertion
#  x_afrows <- apply(x$data[, x_afcols], 1, any)
#  
#  ## which fatal assertions are present in this data?
#  these_assertions <- names(x$data)[x_afcols]
#  
#  ## make a link to th web page for each occurrence
#  popup_link <- paste0("<a href=\"https://biocache.ala.org.au/occurrences/", x$data$id, "\">Link to occurrence record</a>")
#  
#  ## colour palette
#  pal <- c(sub("FF$", "", heat.colors(length(these_assertions))))
#  
#  ## map each data row to colour, depending on its assertions
#  marker_colour <- rep("#00FF00", nrow(x$data))
#  if (length(these_assertions)>0) {
#    for (k in 1:length(these_assertions)) marker_colour[x$data[, x_afcols[k]]] <- pal[k]
#  }
#  
#  ## blank map, with imagery background
#  m <- addProviderTiles(leaflet(), "Esri.WorldImagery")
#  
#  ## add markers
#  m <- addCircleMarkers(m, x$data$longitude, x$data$latitude, col=marker_colour, popup=popup_link)
#  
#  print(m)

## ----message=FALSE----------------------------------------------------------------------------------------------------
library(vegan)

## ----eval=FALSE-------------------------------------------------------------------------------------------------------
#  wkt <- "POLYGON((152.5 -35,152.5 -32,140 -32,140 -35,152.5 -35))"
#  
#  ## define some environmental layers of interest [see ala_fields()]
#  env_layers <- c("Precipitation - annual","Temperature - annual max mean")
#  
#  ## Download the data.  We use the `occurrences()` function, adding environmental
#  ##   data via the 'extra' parameter. Note that method="offline" supports
#  ##   unlimited download size and more fields (but is slower).
#  
#  ## You should adjust the `download_reason_id` to match your purposes if using
#  ##   this function for your own analyses; see `ala_reasons()`
#  
#  x <- occurrences(taxon="family:Fabaceae", wkt=wkt, qa="none",
#                   download_reason_id="testing", extra=env_layers,
#                   email="ala4r@ala.org.au")

## ----include=FALSE----------------------------------------------------------------------------------------------------
## load data from a local copy so that vignette building doesn't require downloading a big chunk of data and slow sites-by-species processing
## this file generated by running the above unevaluated code blocks, then
## saveRDS(xgridded, file="vignette_fabaceae.rds")
xgridded <- readRDS("vignette_fabaceae.rds")
sppcols <- setdiff(names(xgridded), c("longitude", "latitude", "precipitationAnnual", "temperatureAnnualMaxMean", "richness"))

## ----eval=FALSE-------------------------------------------------------------------------------------------------------
#  xgridded <- x$data %>%
#      ## discard genus- and higher-level records
#      dplyr::filter(rank %in%
#                    c("species", "subspecies", "variety", "form", "cultivar")) %>%
#  
#      ## bin into 0.5-degree bins
#      mutate(longitude=round(longitude*2)/2, latitude=round(latitude*2)/2) %>%
#  
#      ## average environmental vars within each bin
#      group_by(longitude,latitude) %>%
#      mutate(precipitationAnnual=mean(precipitationAnnual, na.rm=TRUE),
#             temperatureAnnualMaxMean=mean(temperatureAnnualMaxMean, na.rm=TRUE)) %>%
#  
#      ## subset to vars of interest
#      dplyr::select(longitude, latitude, scientificName, precipitationAnnual,
#                    temperatureAnnualMaxMean) %>%
#  
#      ## take one row per cell per species (presence)
#      distinct() %>%
#  
#      ## calculate species richness
#      mutate(richness=n()) %>%
#  
#      ## convert to wide format (sites by species)
#      mutate(present=1) %>%
#      do(tidyr::spread(data=., key=scientificName, value=present, fill=0)) %>%
#      ungroup()
#  
#  ## where a species was not present, it will have NA: convert these to 0
#  sppcols <- setdiff(names(xgridded),
#                     c("longitude", "latitude", "precipitationAnnual", "temperatureAnnualMaxMean",
#                       "richness"))
#  xgridded <- xgridded %>% mutate_at(sppcols, function(z) ifelse(is.na(z), 0, z))

## ---------------------------------------------------------------------------------------------------------------------
xgridded

## ----warning=FALSE----------------------------------------------------------------------------------------------------
library(ggplot2)
ggplot(xgridded, aes(longitude, richness)) + geom_point() + theme_bw()

## ----warning=FALSE----------------------------------------------------------------------------------------------------
ggplot(xgridded, aes(temperatureAnnualMaxMean, precipitationAnnual, colour=richness)) +
    scale_colour_distiller(palette="Spectral") + geom_point(size=8) + theme_bw()

## ----fig.width=6, fig.height=6----------------------------------------------------------------------------------------
## Bray-Curtis dissimilarity
D <- vegdist(xgridded[, sppcols], "bray")

## UPGMA clustering
cl <- hclust(D, method="ave")

## plot the dendrogram
plot(cl)

## extract group labels at the 20-group level
grp <- cutree(cl, 20)

## coalesce small (outlier) groups into a single catch-all group
sing <- which(table(grp)<5)
grp[grp %in% sing] <- 21 ## put these in a new combined group
grp <- sapply(grp, function(z)which(unique(grp)==z)) ## renumber groups
xgridded$grp <- as.factor(grp)

## plot
## colours for clusters
thiscol <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2",
             "#7f7f7f", "#bcbd22", "#17becf")
ggplot(xgridded, aes(longitude, latitude, colour=grp)) + geom_point(size=5) +
    scale_colour_manual(values=thiscol) + theme_bw()

## or a slightly nicer map plot
library(maps)
library(mapdata)
map("worldHires", "Australia", xlim=c(105, 155), ylim=c(-45, -10), col="gray90", fill=TRUE)
with(xgridded, points(longitude, latitude, pch=21, col=thiscol[grp], bg=thiscol[grp], cex=0.75))

## ----eval=FALSE-------------------------------------------------------------------------------------------------------
#  magpie_occs <- ALA4R::occurrences(taxon="taxon_name:\"Gymnorhina tibicen\"",
#                              fq=c("multimedia:Image","license:\"CC0\""))

## ----eval=FALSE-------------------------------------------------------------------------------------------------------
#  magpie_occs_top5 <- magpie_occs$data %>%
#      dplyr::select(id,basisOfRecord,dataResourceName,state,licence,eventDate) %>%
#      head(5)

## ----eval=FALSE, echo=FALSE-------------------------------------------------------------------------------------------
#  tryCatch({
#    magpie_occs <- ALA4R::occurrences(taxon="taxon_name:\"Gymnorhina tibicen\"",
#                              fq=c("multimedia:Image","license:\"CC0\""),
#                              email = "ala4r@ala.org.au", download_reason_id = "testing")
#    # retain 5
#    magpie_occs_top5 <- magpie_occs$data %>% dplyr::arrange(desc(eventDate)) %>%
#      dplyr::mutate(occId=paste0(substring(id,0,10),"...")) %>%
#      dplyr::select(id,occId,basisOfRecord,dataResourceName,state,licence,eventDate) %>%
#      head(5)
#  
#    # display
#    if (!is.null(dim(magpie_occs_top5))) { magpie_occs_top5 %>%
#        dplyr::select(occId,basisOfRecord,dataResourceName,state,licence,eventDate)}
#  
#  },warning = function(w) {print(w$message)}
#   ,error = function(e) { print(e$message)})

## ----eval=FALSE-------------------------------------------------------------------------------------------------------
#  magpie_occ_images <- ALA4R::occurrence_images(magpie_occs_top5 %>%
#      dplyr::pull(id), download=FALSE)

## ----echo=FALSE,warning=FALSE, eval=FALSE-----------------------------------------------------------------------------
#  tryCatch({
#    magpie_occ_images <- ALA4R::occurrence_images(magpie_occs_top5 %>% dplyr::pull(id), download=FALSE)
#  
#    if (!is.null(dim(magpie_occ_images))) {
#      magpie_occ_images %>% dplyr::mutate(occurrenceID=paste0(substring(occurrenceID,0,10),"..."), imageID = paste0(substring(imageIdentifier,0,10),"...")) %>%
#        dplyr::select(occurrenceID, imageID, format, fileSize, width, height)
#    }
#  },error = function(e) { print(e$message)})
#  

## ----eval=FALSE-------------------------------------------------------------------------------------------------------
#  magpie_occ_images <- ALA4R::occurrence_images(magpie_occs_top5 %>%
#      dplyr::pull(id), download=TRUE, download_path = "my/local/directory")

