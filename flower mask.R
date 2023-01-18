require(raster)
require(tmap)
require(dplyr)

vant = stack('./antes/orto_medio_4marcador_corrigido.tif')
tratamentos = shapefile('./antes/Tratamento_remocao.shp')

tratamentoEscolhido = subset(tratamentos, rotulo =="L1C1")
vantTratamentoEscolhido = crop(vant, tratamentoEscolhido)

plotRGB(vantTratamentoEscolhido)

# Baseado no valor numérico do pixel -------------------
refPixelMin = 220
refPixelMax = 230
banda1 = vantTratamentoEscolhido[[1]]
banda1[!((banda1 > refPixelMin) & (banda1 < refPixelMax))] = 0
banda1[(banda1 > refPixelMin) & (banda1 < refPixelMax)] = 1

refPixelMin = 220
refPixelMax = 230
banda2 = vantTratamentoEscolhido[[2]]
banda2[!((banda2 > refPixelMin) & (banda2 < refPixelMax))] = 0
banda2[(banda2 > refPixelMin) & (banda2 < refPixelMax)] = 1

refPixelMin = 220
refPixelMax = 230
banda3 = vantTratamentoEscolhido[[3]]
banda3[!((banda3 > refPixelMin) & (banda3 < refPixelMax))] = 0
banda3[(banda3 > refPixelMin) & (banda3 < refPixelMax)] = 1

pixelFlorais = (banda1 + banda2 + banda3)
pixelFlorais[pixelFlorais == 3] = 1
pixelFlorais[!(pixelFlorais == 1)] = 0

pixelFlorais.df = as.data.frame(pixelFlorais) 
sum(pixelFlorais.df$layer)

tm_shape(featureOfInterest) + 
  tm_raster()

# Classificação não supervisionada ------------------
vantTratamentoEscolhido.df = as.data.frame(vantTratamentoEscolhido)  
ajusteKmeans = kmeans(na.omit(vantTratamentoEscolhido.df), centers=15, algorithm="MacQueen", iter.max=1000)

ajusteKmeansFactor = rep(NA, length(vantTratamentoEscolhido.df[,1]))
ajusteKmeansFactor[!is.na(vantTratamentoEscolhido.df[,1])] = ajusteKmeans$cluster

imagemKmeans = raster(vantTratamentoEscolhido)   ## create an empty raster with same extent than "image"  
imagemKmeans = setValues(imagemKmeans, ajusteKmeansFactor) ## fill the empty raster with the class results  
tm_shape(imagemKmeans) + 
  tm_raster()

pixelFlorais = imagemKmeans
class = 15
pixelFlorais[!(pixelFlorais == class)] = 0
pixelFlorais[pixelFlorais == class] = 1

pixelFlorais.df = as.data.frame(pixelFlorais) 
sum(pixelFlorais.df$layer)

tm_shape(pixelFlorais) + 
  tm_raster()

# Classificação supervisionada ---------------------
pontosTreinamento = shapefile('./antes/pontosTreinamento.shp')

names(vant) = c("red", "green", 'blue', 'alpha')
vant = vant[[-4]]

sampvals = extract(vant, pontosTreinamento, df = TRUE)
sampvals = sampvals[, -1]
sampdata = data.frame(classvalue = pontosTreinamento@data$class, sampvals)

mediaGrupo = sampdata %>%
  group_by(classvalue) %>%
  summarise(red = mean(red),
            green = mean(green),
            blue = mean(blue))

ggplot(sampdata, aes(x = red, y = green)) + 
  geom_point(aes(colour = factor(classvalue))) + 
  geom_point(data = mediaGrupo, aes(red, green), shape = 4, size = 3) +
  scale_color_manual(values = c("1" = "green", "2" = "gray"))

ggplot(sampdata, aes(x = green, y = blue)) + 
  geom_point(aes(colour = factor(classvalue))) + 
  geom_point(data = mediaGrupo, aes(green, blue), shape = 4, size = 3) +
  scale_color_manual(values = c("1" = "green", "2" = "gray"))

ggplot(sampdata, aes(x = red, y = blue)) + 
  geom_point(aes(colour = factor(classvalue))) + 
  geom_point(data = mediaGrupo, aes(red, blue), shape = 4, size = 3) +
  scale_color_manual(values = c("1" = "green", "2" = "gray"))


mylogit = glm(classvalue ~ red + green + blue, data = sampdata, family = "binomial")
imaClass = predict(vant, mylogit)
mycolor <- c("gray", "green")
plot(imaClass, main = 'Supervised classification - Logistica', col=mycolor)