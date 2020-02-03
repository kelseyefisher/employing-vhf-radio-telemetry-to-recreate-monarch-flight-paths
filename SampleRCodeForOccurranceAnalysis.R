##################################################################################################
##read FORMATTED file
Monarch<-read.csv('RadioTagged_SummarizedEstimatedLocations.csv', header=TRUE)
head(Monarch)
#############################################################################################

library(ctmm)

##turn into a telemetry object
Monarch<-as.telemetry(Monarch)

###Plot all monarchs
plot(Monarch,
     col=rainbow(length(Monarch)))

######################
####2016 Monarch a####
######################

#plot only the one monarch
plot(Monarch$`a`)

#fit a variogram
vg <- variogram(Monarch$`a`)
#guess the best model fit
GUESS <- ctmm.guess(Monarch$`a`,
                    interactive = FALSE)
#include the error estimates in the calculation
GUESS$error<-TRUE

#plot the variogram and the error guess variogram
plot(vg, GUESS)
#see all of the model fit AIC values
FIT<-ctmm.select(Monarch$`a`,GUESS,verbose = TRUE)
summary(FIT)
#Select best model and store the fit model
FIT<-ctmm.select(Monarch$`a`,GUESS)
summary(FIT)


#####Occurrence#####

OCCU1<-occurrence(Monarch$`a`,FIT)
plot(OCCU1)

plot(Monarch$`a`, UD=OCCU1)
