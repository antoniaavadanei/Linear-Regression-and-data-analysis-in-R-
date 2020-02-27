myQuakes = quakes
summary(quakes)

lats = quakes$lat             #lats = latitudinile
longs = quakes$long           #longs = lungitudinile
depths = quakes$depth         #depths = adancimile
mags = quakes$mag             #mags = magnitudinile
stationss = quakes$stations   #stationss = statiunile


# QUARTILES

latsQuantile = quantile(lats)
longsQuantile = quantile(longs)
depths_quantile = quantile(depths)
mags_quantile = quantile(mags)
stations_quantile = quantile(stationss)


# MEAN (media efectiva)

lats_mean = mean(lats)
longs_mean = mean(longs)
depths_mean = mean(depths)
mags_mean = mean(mags)
stations_mean = mean(stationss)

# VARIANCES

lats_var = var(lats)
longs_var = var(longs)
depths_var = var(depths)
mags_var = var(mags)
stations_var = var(stationss)

#BOXPLOTS 
par(mfrow=c(1,5))
lats_boxplot = boxplot(lats,main="latitude")
longs_boxplot = boxplot(longs,main="longitude")
depths_boxplot = boxplot(depths,main="depth")
mags_boxplot = boxplot(mags,main="magnitude")
stations_vboxplot = boxplot(stationss,main="stations")


#REGRESIA SIIMPLA


scatter.smooth(x=quakes$mag, y=quakes$stations, main="Stations ~ Magnitude")

#coreelatia dintre magnitudine si nr de statii
cor(quakes$mag, quakes$stations) # 0.8511824

#construim modelul
simple <- lm(stations~mag, data=quakes)  # build linear regression model on full data
AIC(simple) #7726.676
BIC(simple) #7741.399
print(simple)
abline(-180.42, 46.28, col="red", lwd = 2)

summary(simple)


#REGRESIE MULTIPLA


#generam cu rnorm datele unei noi variabile: intensity (Mercalli) ( variaza intre 1 si 12)
set.seed(100)
intensity<-rnorm(1000,6.5,1)
plot(density(intensity), main="Density Plot: Intensity", ylab="Frequency") 

#construim modelul
multiple <- lm(stations~intensity + depth, data=quakes)
AIC(multiple) #9010.728
BIC(multiple) #9030.359
print(multiple)
summary(multiple)


#preziceri pentru primul model


set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(quakes), 0.8*nrow(quakes))  # row indices for training data
trainingData <- quakes[trainingRowIndex, ]  # model training data
testData  <- quakes[-trainingRowIndex, ]   # test data
#print(testData)
#print(trainingData)

lmMod <- lm(stations~mag, data=trainingData)  # build the model
stationsPred <- predict(lmMod, testData)  # predict distance

summary (lmMod) 
correlation_accuracy <- cor(actuals_preds) #0.8666185
actuals_preds <- data.frame(cbind(actuals=testData$stations, predicteds=stationsPred))
head(actuals_preds)


#repartitia f

par(mfrow=c(1,2))
x_df <- seq(0, 5, by = 0.01)
y_df11 <- df(x_df, df1 = 1, df2=1)  
y_df100100 <- df(x_df, df1 = 100, df2=100)
y_df52 <- df(x_df, df1 = 5, df2=2)  
plot(y_df11, main="Functia de masa",type="l")    
lines(y_df100100, col="magenta")
lines(y_df52, col="blue")

x_pf <- seq(0, 5, by = 0.01)
y_pf11 <- pf(x_pf, df1 = 1, df2=1)      
y_pf100100 <- pf(x_pf, df1 = 100, df2=100)
y_pf52 <- pf(x_pf, df1 = 5, df2=2)
plot(y_pf11, main="Functia de rapartitie", type="l", ylim=c(0, 1.5))
lines(y_pf100100, col="magenta")
lines(y_pf52, col="blue")


