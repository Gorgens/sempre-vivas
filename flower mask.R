require(raster)
require(tmap)

site = stack('area_experimento.tif')
site

grayscale_colors <- gray.colors(100,            # number of different color levels 
                                start = 0.0,    # how black (0) to go
                                end = 1.0,      # how white (1) to go
                                gamma = 2.2,    # correction between how a digital 
                                # camera sees the world and how human eyes see it
                                alpha = NULL)   #Null=colors are not transparent

plot(site@layers[[1]], col=grayscale_colors)
plot(site@layers[[2]], col=grayscale_colors)
plot(site@layers[[3]], col=grayscale_colors)

siteBrick = brick(site@layers[[1]], site@layers[[2]], site@layers[[3]])
plotRGB(siteBrick)

# Teste baseado no valor numérico do pixel
refPixel = 130
a = site@layers[[1]]
a[a < refPixel] = 0
a[a >= refPixel] = 1

b = site@layers[[2]]
b[b < refPixel] = 0
b[b >= refPixel] = 1

c = site@layers[[3]]
c[c < refPixel] = 0
c[c >= refPixel] = 1

featureOfInterest = ceiling((a + b + c)/3)
tm_shape(featureOfInterest) + 
  tm_raster()

# teste da classificação não supervisionada
image.df = as.data.frame(site)  
cluster.image = kmeans(na.omit(image.df), centers=15)

image.df.factor = rep(NA, length(image.df[,1]))
image.df.factor[!is.na(image.df[,1])] = cluster.image$cluster

clusters = raster(site)   ## create an empty raster with same extent than "image"  
clusters = setValues(clusters, image.df.factor) ## fill the empty raster with the class results  

tm_shape(clusters) + 
  tm_raster()

tm_shape(a) + 
  tm_raster()
