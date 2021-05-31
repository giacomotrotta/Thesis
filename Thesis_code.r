###library
library(ggplot2)
library(visreg)
library(gridExtra)
library(grid)
library(readxl)
library(nlme)
library(effects)
library(vegan)
library(nlme)

open <- read.delim("/Users/giacomotrotta/Tesi/R_Personali/openfield.csv")
View(open)

boc <- read.delim("/Users/giacomotrotta/Tesi/R_Personali/bocage.csv")
View(boc)

forest <- read.delim("/Users/giacomotrotta/Tesi/R_Personali/forest.csv")
View(forest)


#total <- read.delim("/Users/giacomotrotta/Tesi/R_Personali/total.csv", header=TRUE, sep=";", na.strings="NA", dec=".", strip.white=TRUE)
#total[,17] <- NULL

total <- read_excel("/Users/giacomotrotta/Tesi/R_Personali/total.xlsx", col_types = c("text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
View(total)



#valutazione Gleason vs Arrhenius. NB la log.area nel dataset non è in realtà la log.area
#se guardo i datamatrix infatti l'area e log.area coincidono. Uso quindi la funzione log(log.area)
#cambiare il nome alla colonna in caso per migliore chiarezza


# data=open[1:30,] #OISE

A_open_lm_oise_tot<-lm(log(species.richness)~log(log.area), data=open[1:30,])
summary(A_open_lm_oise_tot)

G_open_lm_oise_tot<-lm(species.richness~log(log.area), data=open[1:30,])
#G_open_plot_oise_tot<-plot(log(log.area),species.richness, data=open[1:30,])
summary(G_open_lm_oise_tot)

A_open_lm_oise_nat<-lm(log(native.richness)~log(log.area), data=open[1:30,])
summary(A_open_lm_oise_nat)

G_open_lm_oise_nat<-lm(native.richness~log(log.area), data=open[1:30,])
summary(G_open_lm_oise_nat)

A_open_lm_oise_alien<-lm(log(alien.richness)~log(log.area), data=open[1:30,]) #aggiungo 1 perchè altrimenti ho log di zero
A_open_lm_oise_alien<-lm(log(alien.richness+1)~log(log.area), data=open[1:30,])
summary(A_open_lm_oise_alien)

G_open_lm_oise_alien<-lm(alien.richness~log(log.area), data=open[1:30,])
summary(G_open_lm_oise_alien)

A_open_lm_oise_FH<-lm(log(FH)~log(log.area), data=open[1:30,])
summary(A_open_lm_oise_FH)

G_open_lm_oise_FH<-lm(FH~log(log.area), data=open[1:30,])
summary(G_open_lm_oise_FH)

A_open_lm_oise_NFH<-lm(log(NFH)~log(log.area), data=open[1:30,])
summary(A_open_lm_oise_NFH)

G_open_lm_oise_NFH<-lm(NFH~log(log.area), data=open[1:30,])
summary(G_open_lm_oise_NFH)

A_open_lm_oise_W<-lm(log(W)~log(log.area), data=open[1:30,])
summary(A_open_lm_oise_W)

G_open_lm_oise_W<-lm(W~log(log.area), data=open[1:30,])
summary(G_open_lm_oise_W)

#data=open[31:60,] #SOMME

A_open_lm_somme_tot<-lm(log(species.richness)~log(log.area), data=open[31:60,])
summary(A_open_lm_somme_tot)

G_open_lm_somme_tot<-lm(species.richness~log(log.area), data=open[31:60,])
summary(G_open_lm_somme_tot)

A_open_lm_somme_nat<-lm(log(native.richness)~log(log.area), data=open[31:60,])
summary(A_open_lm_somme_nat)

G_open_lm_somme_nat<-lm(native.richness~log(log.area), data=open[31:60,])
summary(G_open_lm_somme_nat)

A_open_lm_somme_alien<-lm(log(alien.richness)~log(log.area), data=open[31:60,])
A_open_lm_somme_alien<-lm(log(alien.richness+1)~log(log.area), data=open[31:60,])
summary(A_open_lm_somme_alien)

G_open_lm_somme_alien<-lm(alien.richness~log(log.area), data=open[31:60,])
summary(G_open_lm_somme_alien)

A_open_lm_somme_FH<-lm(log(FH)~log(log.area), data=open[31:60,])
summary(A_open_lm_somme_FH)

G_open_lm_somme_FH<-lm(FH~log(log.area), data=open[31:60,])
summary(G_open_lm_somme_FH)

A_open_lm_somme_NFH<-lm(log(NFH)~log(log.area), data=open[31:60,])
summary(A_open_lm_somme_NFH)

G_open_lm_somme_NFH<-lm(NFH~log(log.area), data=open[31:60,])
summary(G_open_lm_somme_NFH)

A_open_lm_somme_W<-lm(log(W)~log(log.area), data=open[31:60,])
summary(A_open_lm_somme_W)

G_open_lm_somme_W<-lm(W~log(log.area), data=open[31:60,])
summary(G_open_lm_somme_W)


# data=open[61:89,] #AISNE

A_open_lm_aisne_tot<-lm(log(species.richness)~log(log.area), data=open[61:89,])
summary(A_open_lm_aisne_tot)

G_open_lm_aisne_tot<-lm(species.richness~log(log.area), data=open[61:89,])
summary(G_open_lm_aisne_tot)

A_open_lm_aisne_nat<-lm(log(native.richness)~log(log.area), data=open[61:89,])
summary(A_open_lm_aisne_nat)

G_open_lm_aisne_nat<-lm(native.richness~log(log.area), data=open[61:89,])
summary(G_open_lm_aisne_nat)


A_open_lm_aisne_alien<-lm(log(alien.richness)~log(log.area), data=open[61:89,])
A_open_lm_aisne_alien<-lm(log(alien.richness+1)~log(log.area), data=open[61:89,])
summary(A_open_lm_aisne_alien)

G_open_lm_aisne_alien<-lm(alien.richness~log(log.area), data=open[61:89,])
summary(G_open_lm_aisne_alien)

A_open_lm_aisne_FH<-lm(log(FH)~log(log.area), data=open[61:89,])
summary(A_open_lm_aisne_FH)

G_open_lm_aisne_FH<-lm(FH~log(log.area), data=open[61:89,])
summary(G_open_lm_aisne_FH)

A_open_lm_aisne_NFH<-lm(log(NFH)~log(log.area), data=open[61:89,])
summary(A_open_lm_aisne_NFH)

G_open_lm_aisne_NFH<-lm(NFH~log(log.area), data=open[61:89,])
summary(G_open_lm_aisne_NFH)

A_open_lm_aisne_W<-lm(log(W)~log(log.area), data=open[61:89,])
summary(A_open_lm_aisne_W)

G_open_lm_aisne_W<-lm(W~log(log.area), data=open[61:89,])
summary(G_open_lm_aisne_W)

# bocage uguale come sopra

# boc[1:60,] #OISE


A_boc_lm_oise_tot<-lm(log(species.richness)~log(log.area), data=boc[1:60,])
summary(A_boc_lm_oise_tot)

G_boc_lm_oise_tot<-lm(species.richness~log(log.area), data=boc[1:60,])
summary(G_boc_lm_oise_tot)


A_boc_lm_oise_nat<-lm(log(native.richness)~log(log.area), data=boc[1:60,])
summary(A_boc_lm_oise_nat)

G_boc_lm_oise_nat<-lm(native.richness~log(log.area), data=boc[1:60,])
summary(G_boc_lm_oise_nat)

A_boc_lm_oise_alien<-lm(log(alien.richness)~log(log.area), data=boc[1:60,])
A_boc_lm_oise_alien<-lm(log(alien.richness+1)~log(log.area), data=boc[1:60,])
summary(A_boc_lm_oise_alien)

G_boc_lm_oise_alien<-lm(alien.richness~log(log.area), data=boc[1:60,])
summary(G_boc_lm_oise_alien)

A_boc_lm_oise_FH<-lm(log(FH)~log(log.area), data=boc[1:60,])
summary(A_boc_lm_oise_FH)

G_boc_lm_oise_FH<-lm(FH~log(log.area), data=boc[1:60,])
summary(G_boc_lm_oise_FH)

A_boc_lm_oise_NFH<-lm(log(NFH)~log(log.area), data=boc[1:60,])
summary(A_boc_lm_oise_NFH)

G_boc_lm_oise_NFH<-lm(NFH~log(log.area), data=boc[1:60,])
summary(G_boc_lm_oise_NFH)

A_boc_lm_oise_W<-lm(log(W)~log(log.area), data=boc[1:60,])
summary(A_boc_lm_oise_W)

G_boc_lm_oise_W<-lm(W~log(log.area), data=boc[1:60,])
summary(G_boc_lm_oise_W)

# boc[61:92,] #SOMME

A_boc_lm_somme_tot<-lm(log(species.richness)~log(log.area), data=boc[61:92,])
summary(A_boc_lm_somme_tot)

ggplot(boc[61:92,], aes(x=log(log.area), y=log(species.richness))) + geom_point() + geom_smooth(method="lm")

G_boc_lm_somme_tot<-lm(species.richness~log(log.area), data=boc[61:92,])
summary(G_boc_lm_somme_tot)

A_boc_lm_somme_nat<-lm(log(native.richness)~log(log.area), data=boc[61:92,])
summary(A_boc_lm_somme_nat)

G_boc_lm_somme_nat<-lm(native.richness~log(log.area), data=boc[61:92,])
summary(G_boc_lm_somme_nat)

A_boc_lm_somme_alien<-lm(log(alien.richness)~log(log.area), data=boc[61:92,])
A_boc_lm_somme_alien<-lm(log(alien.richness+1)~log(log.area), data=boc[61:92,])
summary(A_boc_lm_somme_alien)

G_boc_lm_somme_alien<-lm(alien.richness~log(log.area), data=boc[61:92,])
summary(G_boc_lm_somme_alien)

A_boc_lm_somme_FH<-lm(log(FH)~log(log.area), data=boc[61:92,])
summary(A_boc_lm_somme_FH)

G_boc_lm_somme_FH<-lm(FH~log(log.area), data=boc[61:92,])
summary(G_boc_lm_somme_FH)

A_boc_lm_somme_NFH<-lm(log(NFH)~log(log.area), data=boc[61:92,])
summary(A_boc_lm_somme_NFH)

G_boc_lm_somme_NFH<-lm(NFH~log(log.area), data=boc[61:92,])
summary(G_boc_lm_somme_NFH)

A_boc_lm_somme_W<-lm(log(W)~log(log.area), data=boc[61:92,])
summary(A_boc_lm_somme_W)

G_boc_lm_somme_W<-lm(W~log(log.area), data=boc[61:92,])
summary(G_boc_lm_somme_W)

# boc[93:154,] #AISNE

A_boc_lm_aisne_tot<-lm(log(species.richness)~log(log.area), data=boc[93:154,])
summary(A_boc_lm_aisne_tot)

G_boc_lm_aisne_tot<-lm(species.richness~log(log.area), data=boc[93:154,])
summary(G_boc_lm_aisne_tot)

A_boc_lm_aisne_nat<-lm(log(native.richness)~log(log.area), data=boc[93:154,])
summary(A_boc_lm_aisne_nat)

G_boc_lm_aisne_nat<-lm(native.richness~log(log.area), data=boc[93:154,])
summary(G_boc_lm_aisne_nat)

A_boc_lm_aisne_alien<-lm(log(alien.richness)~log(log.area), data=boc[93:154,])
A_boc_lm_aisne_alien<-lm(log(alien.richness+1)~log(log.area), data=boc[93:154,])
summary(A_boc_lm_aisne_alien)

G_boc_lm_aisne_alien<-lm(alien.richness~log(log.area), data=boc[93:154,])
summary(G_boc_lm_aisne_alien)

A_boc_lm_aisne_FH<-lm(log(FH)~log(log.area), data=boc[93:154,])
summary(A_boc_lm_aisne_FH)

G_boc_lm_aisne_FH<-lm(FH~log(log.area), data=boc[93:154,])
summary(G_boc_lm_aisne_FH)

A_boc_lm_aisne_NFH<-lm(log(NFH)~log(log.area), data=boc[93:154,])
summary(A_boc_lm_aisne_NFH)

G_boc_lm_aisne_NFH<-lm(NFH~log(log.area), data=boc[93:154,])
summary(G_boc_lm_aisne_NFH)

A_boc_lm_aisne_W<-lm(log(W)~log(log.area), data=boc[93:154,])
summary(A_boc_lm_aisne_W)

G_boc_lm_aisne_W<-lm(W~log(log.area), data=boc[93:154,])
summary(G_boc_lm_aisne_W)


# forest 
#qui l'area è giusta, uso la colonna area. la colonna log.area ha i log in base 10
# data=forest[1:43,] #OISE

A_for_lm_oise_tot<-lm(log(species.richness)~log(area), data=forest[1:43,])
summary(A_for_lm_oise_tot)

G_for_lm_oise_tot<-lm(species.richness~log(area), data=forest[1:43,])
summary(G_for_lm_oise_tot)

A_for_lm_oise_nat<-lm(log(native.richness)~log(area), data=forest[1:43,])
summary(A_for_lm_oise_nat)

G_for_lm_oise_nat<-lm(native.richness~log(area), data=forest[1:43,])
summary(G_for_lm_oise_nat)

A_for_lm_oise_alien<-lm(log(alien.richness)~log(area), data=forest[1:43,])
A_for_lm_oise_alien<-lm(log(alien.richness+1)~log(area), data=forest[1:43,])
summary(A_for_lm_oise_alien)

G_for_lm_oise_alien<-lm(alien.richness~log(area), forest[1:43,])
summary(G_for_lm_oise_alien)

A_for_lm_oise_FH<-lm(log(FH)~log(area), data=forest[1:43,])
summary(A_for_lm_oise_FH)

G_for_lm_oise_FH<-lm(FH~log(area), data=forest[1:43,])
summary(G_for_lm_oise_FH)

A_for_lm_oise_NFH<-lm(log(NFH)~log(area), data=forest[1:43,])
summary(A_for_lm_oise_NFH)

G_for_lm_oise_NFH<-lm(NFH~log(area), data=forest[1:43,])
summary(G_for_lm_oise_NFH)

A_for_lm_oise_W<-lm(log(W)~log(area), data=forest[1:43,])
summary(A_for_lm_oise_W)

G_for_lm_oise_W<-lm(W~log(area), data=forest[1:43,])
summary(G_for_lm_oise_W)

# forest[44:74,] #SOMME

A_for_lm_somme_tot<-lm(log(species.richness)~log(area), data=forest[44:74,])
summary(A_for_lm_somme_tot)

G_for_lm_somme_tot<-lm(species.richness~log(area), data=forest[44:74,])
summary(G_for_lm_somme_tot)

A_for_lm_somme_nat<-lm(log(native.richness)~log(area), data=forest[44:74,])
summary(A_for_lm_somme_nat)

G_for_lm_somme_nat<-lm(native.richness~log(area), data=forest[44:74,])
summary(G_for_lm_somme_nat)

A_for_lm_somme_alien<-lm(log(alien.richness)~log(area), data=forest[44:74,])
A_for_lm_somme_alien<-lm(log(alien.richness+1)~log(area), data=forest[44:74,])
summary(A_for_lm_somme_alien)

G_for_lm_somme_alien<-lm(alien.richness~log(area), data=forest[44:74,])
summary(G_for_lm_somme_alien)

A_for_lm_somme_FH<-lm(log(FH)~log(area), data=forest[44:74,])
summary(A_for_lm_somme_FH)

G_for_lm_somme_FH<-lm(FH~log(area), data=forest[44:74,])
summary(G_for_lm_somme_FH)

A_for_lm_somme_NFH<-lm(log(NFH)~log(area), data=forest[44:74,])
summary(A_for_lm_somme_NFH)

G_for_lm_somme_NFH<-lm(NFH~log(area), data=forest[44:74,])
summary(G_for_lm_somme_NFH)

A_for_lm_somme_W<-lm(log(W)~log(area), data=forest[44:74,])
summary(A_for_lm_somme_W)

G_for_lm_somme_W<-lm(W~log(area), data=forest[44:74,])
summary(G_for_lm_somme_W)

# forest[75:115,]  #AISNE

A_for_lm_aisne_tot<-lm(log(species.richness)~log(area), data=forest[75:115,])
summary(A_for_lm_aisne_tot)

G_for_lm_aisne_tot<-lm(species.richness~log(area),  data=forest[75:115,])
summary(G_for_lm_aisne_tot)

A_for_lm_aisne_nat<-lm(log(native.richness)~log(area),  data=forest[75:115,])
summary(A_for_lm_aisne_nat)

G_for_lm_aisne_nat<-lm(native.richness~log(area),  data=forest[75:115,])
summary(G_for_lm_aisne_nat)

A_for_lm_aisne_alien<-lm(log(alien.richness)~log(area),  data=forest[75:115,])
A_for_lm_aisne_alien<-lm(log(alien.richness+1)~log(area),  data=forest[75:115,])
summary(A_for_lm_aisne_alien)

G_for_lm_aisne_alien<-lm(alien.richness~log(area),  data=forest[75:115,])
summary(G_for_lm_aisne_alien)

A_for_lm_aisne_FH<-lm(log(FH)~log(area),  data=forest[75:115,])
summary(A_for_lm_aisne_FH)

G_for_lm_aisne_FH<-lm(FH~log(area),  data=forest[75:115,])
summary(G_for_lm_aisne_FH)

A_for_lm_aisne_NFH<-lm(log(NFH)~log(area),  data=forest[75:115,])
summary(A_for_lm_aisne_NFH)

G_for_lm_aisne_NFH<-lm(NFH~log(area),  data=forest[75:115,])
summary(G_for_lm_aisne_NFH)

A_for_lm_aisne_W<-lm(log(W)~log(area),  data=forest[75:115,])
summary(A_for_lm_aisne_W)

G_for_lm_aisne_W<-lm(W~log(area),  data=forest[75:115,])
summary(G_for_lm_aisne_W)


#Ora ho riempito la tabella gleason/Arrhenius, faccio quanto fatto
#precedentemente, solo usando il modello migliore a seconda dell'area

#plot

#open

open_oise_SAR <- ggplot(data=open[1:30,], aes(x=log(log.area), y=native.richness)) + 
  geom_point(data=open[1:30,], aes(x=log(log.area), y=native.richness), color='blue') + geom_abline(intercept = -79.98808, slope = 14.52546, color="blue") +
  geom_point(data=open[1:30,], aes(x=log(log.area), y=alien.richness), color='red') + geom_abline(intercept = -11.7899, slope = 1.4389, color="red") + labs(y=" ", x = " ")


open_somme_SAR <- ggplot(data=open[31:60,], aes(x=log(log.area), y=native.richness)) + 
  geom_point(data=open[31:60,], aes(x=log(log.area), y=native.richness), color='blue') + geom_abline(intercept = -98.23267, slope = 16.49115, color="blue") +
  geom_point(data=open[31:60,], aes(x=log(log.area), y=alien.richness), color='red') + geom_abline(intercept = -8.277537, slope = 1.276600, color="red") + labs(y=" ", x = " ")

open_aisne_SAR <- ggplot(data=open[61:89,], aes(x=log(log.area), y=native.richness)) + 
  geom_point(data=open[61:89,], aes(x=log(log.area), y=native.richness), color='blue') + geom_abline(intercept = -104.85293, slope = 17.47058, color="blue") +
  geom_point(data=open[61:89,], aes(x=log(log.area), y=alien.richness), color='red') + geom_abline(intercept = -10.381978, slope = 1.244262, color="red") + labs(y=" ", x = " ")

#boc

boc_oise_SAR <- ggplot(data=boc[1:60,], aes(x=log(log.area), y=native.richness)) + 
  geom_point(data=boc[1:60,], aes(x=log(log.area), y=native.richness), color='blue') + geom_abline(intercept = -185.70367, slope = 27.32607, color="blue") +
  geom_point(data=boc[1:60,], aes(x=log(log.area), y=alien.richness), color='red') + geom_abline(intercept = -20.193394, slope = 2.505927, color="red")  + labs(y=" ", x = " ")

boc_somme_SAR <- ggplot(data=boc[61:92,], aes(x=log(log.area), y=native.richness)) + 
  geom_point(data=boc[61:92,], aes(x=log(log.area), y=native.richness), color='blue') + geom_abline(intercept = -122.08987, slope = 20.31059, color="blue") +
  geom_point(data=boc[61:92,], aes(x=log(log.area), y=alien.richness), color='red') + geom_abline(intercept = -18.394682, slope = 2.251798, color="red")  + labs(y=" ", x = " ")

boc_aisne_SAR <- ggplot(data=boc[93:154,], aes(x=log(log.area), y=native.richness)) + 
  geom_point(data=boc[93:154,], aes(x=log(log.area), y=native.richness), color='blue') + geom_abline(intercept = -84.72687, slope = 17.55288, color="blue") +
  geom_point(data=boc[93:154,], aes(x=log(log.area), y=alien.richness), color='red') + geom_abline(intercept = -0.6200729, slope = 0.2816629, color="red")  + labs(y=" ", x = " ")

#for

for_oise_SAR <- ggplot(data=forest[1:43,], aes(x=log(area), y=native.richness)) + 
  geom_point(data=forest[1:43,], aes(x=log(area), y=native.richness), color='blue') + geom_abline(intercept = -135.48046, slope = 21.02673, color="blue") +
  geom_point(data=forest[1:43,], aes(x=log(area), y=alien.richness), color='red') + geom_abline(intercept = -14.146779, slope = 1.682088, color="red")  + labs(y=" ", x = " ")

for_somme_SAR <- ggplot(data=forest[44:74,], aes(x=log(area), y=native.richness)) + 
  geom_point(data=forest[44:74,], aes(x=log(area), y=native.richness), color='blue') + geom_abline(intercept = -41.07203, slope = 10.70025, color="blue") +
  geom_point(data=forest[44:74,], aes(x=log(area), y=alien.richness), color='red') + geom_abline(intercept = -11.828772, slope = 1.427553, color="red")  + labs(y=" ", x = " ")

for_aisne_SAR <- ggplot(data=forest[75:115,], aes(x=log(area), y=native.richness)) + 
  geom_point(data=forest[75:115,], aes(x=log(area), y=native.richness), color='blue') + geom_abline(intercept = -60.86654, slope = 13.91062, color="blue") +
  geom_point(data=forest[75:115,], aes(x=log(area), y=alien.richness), color='red') + geom_abline(intercept = -6.2638452, slope = 0.8016998, color="red")  + labs(y=" ", x = " ")

grid.arrange(open_oise_SAR, open_somme_SAR, open_aisne_SAR, boc_oise_SAR, boc_somme_SAR, boc_aisne_SAR, for_oise_SAR, for_somme_SAR, for_aisne_SAR, ncol=3 )

#totale sar

tot_open_oise_SAR <- ggplot(data=open[1:30,], aes(x=log(log.area), y=species.richness)) + geom_point() + geom_smooth(method="lm") 
tot_open_somme_SAR <- ggplot(data=open[31:60,], aes(x=log(log.area), y=species.richness)) + geom_point() + geom_smooth(method="lm") 
tot_open_aisne_SAR <- ggplot(data=open[61:89,], aes(x=log(log.area), y=species.richness)) + geom_point() + geom_smooth(method="lm") 

tot_boc_oise_SAR <- ggplot(data=boc[1:60,], aes(x=log(log.area), y=species.richness)) + geom_point() + geom_smooth(method="lm") 
tot_boc_somme_SAR <- ggplot(data=boc[61:92,], aes(x=log(log.area), y=species.richness)) + geom_point() + geom_smooth(method="lm") 
tot_boc_aisne_SAR <- ggplot(data=boc[93:154,], aes(x=log(log.area), y=species.richness)) + geom_point() + geom_smooth(method="lm") 

tot_for_oise_SAR <- ggplot(data=forest[1:43,], aes(x=log(area), y=species.richness)) + geom_point() + geom_smooth(method="lm") 
tot_for_somme_SAR <- ggplot(data=forest[44:74,], aes(x=log(area), y=species.richness)) + geom_point() + geom_smooth(method="lm") 
tot_for_aisne_SAR <- ggplot(data=forest[75:115,], aes(x=log(area), y=species.richness)) + geom_point() + geom_smooth(method="lm") 

grid.arrange(tot_open_oise_SAR, tot_open_somme_SAR, tot_open_aisne_SAR, tot_boc_oise_SAR, tot_boc_somme_SAR, tot_boc_aisne_SAR, tot_for_oise_SAR, tot_for_somme_SAR, tot_for_aisne_SAR, ncol=3)

# intercetta e pendenza

# open[1:30,] #OISE

coef_open_oise_t<-as.data.frame(G_open_lm_oise_tot$coefficients)

coef_open_oise_n<-as.data.frame(G_open_lm_oise_nat$coefficients)

coef_open_oise_a<-as.data.frame(G_open_lm_oise_alien$coefficients)

coef_open_oise_FH<-as.data.frame(G_open_lm_oise_FH$coefficients)

coef_open_oise_NFH<-as.data.frame(G_open_lm_oise_NFH$coefficients)

coef_open_oise_W<-as.data.frame(G_open_lm_oise_W$coefficients)


# data=open[31:60,] #SOMME

coef_open_somme_t<-as.data.frame(G_open_lm_somme_tot$coefficients)

coef_open_somme_n<-as.data.frame(G_open_lm_somme_nat$coefficients)

coef_open_somme_a<-as.data.frame(G_open_lm_somme_alien$coefficients)

coef_open_somme_FH<-as.data.frame(G_open_lm_somme_FH$coefficients)

coef_open_somme_NFH<-as.data.frame(G_open_lm_somme_NFH$coefficients)

coef_open_somme_W<-as.data.frame(G_open_lm_somme_W$coefficients)


# data=open[61:89,] #AISNE

coef_open_aisne_t<-as.data.frame(G_open_lm_aisne_tot$coefficients)

coef_open_aisne_n<-as.data.frame(G_open_lm_aisne_nat$coefficients)

coef_open_aisne_a<-as.data.frame(G_open_lm_aisne_alien$coefficients)

coef_open_aisne_FH<-as.data.frame(G_open_lm_aisne_FH$coeffients)

coef_open_aisne_NFH<-as.data.frame(G_open_lm_aisne_NFH$coefficients)

coef_open_aisne_W<-as.data.frame(G_open_lm_aisne_W$coefficients)


# bocage

# boc[1:60,] #OISE

coef_boc_oise_t<-as.data.frame(G_boc_lm_oise_tot$coefficients)

coef_boc_oise_n<-as.data.frame(G_boc_lm_oise_nat$coefficients)

coef_boc_oise_a<-as.data.frame(G_boc_lm_oise_alien$coefficients)

coef_boc_oise_FH<-as.data.frame(G_boc_lm_oise_FH$coefficients)

coef_boc_oise_NFH<-as.data.frame(G_boc_lm_oise_NFH$coefficients)

coef_boc_oise_W<-as.data.frame(G_boc_lm_oise_W$coefficients)


# boc[61:92,] #SOMME

coef_boc_somme_t<-as.data.frame(G_boc_lm_somme_tot$coefficients)

coef_boc_somme_n<-as.data.frame(G_boc_lm_somme_nat$coefficients)

coef_boc_somme_a<-as.data.frame(G_boc_lm_somme_alien$coefficients)

coef_boc_somme_FH<-as.data.frame(G_boc_lm_somme_FH$coefficients)

coef_boc_somme_NFH<-as.data.frame(G_boc_lm_somme_NFH$coefficients)

coef_boc_somme_W<-as.data.frame(G_boc_lm_somme_W$coefficients)


# boc[93:154,] #AISNE

coef_boc_aisne_t<-as.data.frame(G_boc_lm_aisne_tot$coefficients)

coef_boc_aisne_n<-as.data.frame(G_boc_lm_aisne_nat$coefficients)

coef_boc_aisne_a<-as.data.frame(G_boc_lm_aisne_alien$coefficients)

coef_boc_aisne_FH<-as.data.frame(G_boc_lm_aisne_FH$coefficients)

coef_boc_aisne_NFH<-as.data.frame(G_boc_lm_aisne_NFH$coefficients)

coef_boc_aisne_W<-as.data.frame(G_boc_lm_aisne_W$coefficients)


# forest 

# data=forest[1:42,]

coef_for_oise_t<-as.data.frame(G_for_lm_oise_tot$coefficients)

coef_for_oise_n<-as.data.frame(G_for_lm_oise_nat$coefficients)

coef_for_oise_a<-as.data.frame(G_for_lm_oise_alien$coefficients)

coef_for_oise_FH<-as.data.frame(G_for_lm_oise_FH$coefficients)

coef_for_oise_NFH<-as.data.frame(G_for_lm_oise_NFH$coefficients)

coef_for_oise_W<-as.data.frame(G_for_lm_oise_W$coefficients)


# forest[44:74,]

coef_for_somme_t<-as.data.frame(G_for_lm_somme_tot$coefficients)

coef_for_somme_n<-as.data.frame(G_for_lm_somme_nat$coefficients)

coef_for_somme_a<-as.data.frame(G_for_lm_somme_alien$coefficients)

coef_for_somme_FH<-as.data.frame(G_for_lm_somme_FH$coefficients)

coef_for_somme_NFH<-as.data.frame(G_for_lm_somme_NFH$coefficients)

coef_for_somme_W<-as.data.frame(G_for_lm_somme_W$coefficients)


# forest[75:115,]

coef_for_aisne_t<-as.data.frame(G_for_lm_aisne_tot$coefficients)

coef_for_aisne_n<-as.data.frame(G_for_lm_aisne_nat$coefficients)

coef_for_aisne_a<-as.data.frame(G_for_lm_aisne_alien$coefficients)

coef_for_aisne_FH<-as.data.frame(G_for_lm_aisne_FH$coefficients)

coef_for_aisne_NFH<-as.data.frame(G_for_lm_aisne_NFH$coefficients)

coef_for_aisne_W<-as.data.frame(G_for_lm_aisne_W$coefficients)


# per visualizzare i coefficienti

# open[1:30,]
coef_open_oise_t

coef_open_oise_n

coef_open_oise_a

coef_open_oise_FH

coef_open_oise_NFH

coef_open_oise_W


# data=open[31:60,]

coef_open_somme_t

coef_open_somme_n

coef_open_somme_a

coef_open_somme_FH

coef_open_somme_NFH

coef_open_somme_W


# data=open[61:89,]

coef_open_aisne_t

coef_open_aisne_n

coef_open_aisne_a

coef_open_aisne_FH

coef_open_aisne_NFH

coef_open_aisne_W


# bocage

# boc[1:60,]

coef_boc_oise_t

coef_boc_oise_n

coef_boc_oise_a

coef_boc_oise_FH

coef_boc_oise_NFH

coef_boc_oise_W


# boc[61:92,]

coef_boc_somme_t

coef_boc_somme_n

coef_boc_somme_a

coef_boc_somme_FH

coef_boc_somme_NFH

coef_boc_somme_W


# boc[93:154,]

coef_boc_aisne_t

coef_boc_aisne_n

coef_boc_aisne_a

coef_boc_aisne_FH

coef_boc_aisne_NFH

coef_boc_aisne_W


# forest 

# data=forest[1:42,]

coef_for_oise_t

coef_for_oise_n

coef_for_oise_a

coef_for_oise_FH

coef_for_oise_NFH

coef_for_oise_W


# forest[44:74,]

coef_for_somme_t

coef_for_somme_n

coef_for_somme_a

coef_for_somme_FH

coef_for_somme_NFH

coef_for_somme_W


# forest[75:115,]

coef_for_aisne_t

coef_for_somme_n

coef_for_somme_a

coef_for_somme_FH

coef_for_somme_NFH

coef_for_somme_W

#Redisuals

# open[1:30,] #OISE

res_open_oise_t<-as.data.frame(G_open_lm_oise_tot$residuals)

res_open_oise_n<-as.data.frame(G_open_lm_oise_nat$residuals)

res_open_oise_a<-as.data.frame(G_open_lm_oise_alien$residuals)

res_open_oise_FH<-as.data.frame(G_open_lm_oise_FH$residuals)

res_open_oise_NFH<-as.data.frame(G_open_lm_oise_NFH$residuals)

res_open_oise_W<-as.data.frame(G_open_lm_oise_W$residuals)


# data=open[31:60,] #SOMME

res_open_somme_t<-as.data.frame(G_open_lm_somme_tot$residuals)

res_open_somme_n<-as.data.frame(G_open_lm_somme_nat$residuals)

res_open_somme_a<-as.data.frame(G_open_lm_somme_alien$residuals)

res_open_somme_FH<-as.data.frame(G_open_lm_somme_FH$residuals)

res_open_somme_NFH<-as.data.frame(G_open_lm_somme_NFH$residuals)

res_open_somme_W<-as.data.frame(G_open_lm_somme_W$residuals)


# data=open[61:89,] #AISNE

res_open_aisne_t<-as.data.frame(G_open_lm_aisne_tot$residuals)

res_open_aisne_n<-as.data.frame(G_open_lm_aisne_nat$residuals)

res_open_aisne_a<-as.data.frame(G_open_lm_aisne_alien$residuals)

res_open_aisne_FH<-as.data.frame(G_open_lm_aisne_FH$coeffients)

res_open_aisne_NFH<-as.data.frame(G_open_lm_aisne_NFH$residuals)

res_open_aisne_W<-as.data.frame(G_open_lm_aisne_W$residuals)


# bocage

# boc[1:60,] #OISE

res_boc_oise_t<-as.data.frame(G_boc_lm_oise_tot$residuals)

res_boc_oise_n<-as.data.frame(G_boc_lm_oise_nat$residuals)

res_boc_oise_a<-as.data.frame(G_boc_lm_oise_alien$residuals)

res_boc_oise_FH<-as.data.frame(G_boc_lm_oise_FH$residuals)

res_boc_oise_NFH<-as.data.frame(G_boc_lm_oise_NFH$residuals)

res_boc_oise_W<-as.data.frame(G_boc_lm_oise_W$residuals)


# boc[61:92,] #SOMME

res_boc_somme_t<-as.data.frame(G_boc_lm_somme_tot$residuals)

res_boc_somme_n<-as.data.frame(G_boc_lm_somme_nat$residuals)

res_boc_somme_a<-as.data.frame(G_boc_lm_somme_alien$residuals)

res_boc_somme_FH<-as.data.frame(G_boc_lm_somme_FH$residuals)

res_boc_somme_NFH<-as.data.frame(G_boc_lm_somme_NFH$residuals)

res_boc_somme_W<-as.data.frame(G_boc_lm_somme_W$residuals)


# boc[93:154,] #AISNE

res_boc_aisne_t<-as.data.frame(G_boc_lm_aisne_tot$residuals)

res_boc_aisne_n<-as.data.frame(G_boc_lm_aisne_nat$residuals)

res_boc_aisne_a<-as.data.frame(G_boc_lm_aisne_alien$residuals)

res_boc_aisne_FH<-as.data.frame(G_boc_lm_aisne_FH$residuals)

res_boc_aisne_NFH<-as.data.frame(G_boc_lm_aisne_NFH$residuals)

res_boc_aisne_W<-as.data.frame(G_boc_lm_aisne_W$residuals)


# forest 

# data=forest[1:42,]

res_for_oise_t<-as.data.frame(G_for_lm_oise_tot$residuals)

res_for_oise_n<-as.data.frame(G_for_lm_oise_nat$residuals)

res_for_oise_a<-as.data.frame(G_for_lm_oise_alien$residuals)

res_for_oise_FH<-as.data.frame(G_for_lm_oise_FH$residuals)

res_for_oise_NFH<-as.data.frame(G_for_lm_oise_NFH$residuals)

res_for_oise_W<-as.data.frame(G_for_lm_oise_W$residuals)


# forest[44:74,]

res_for_somme_t<-as.data.frame(G_for_lm_somme_tot$residuals)

res_for_somme_n<-as.data.frame(G_for_lm_somme_nat$residuals)

res_for_somme_a<-as.data.frame(G_for_lm_somme_alien$residuals)

res_for_somme_FH<-as.data.frame(G_for_lm_somme_FH$residuals)

res_for_somme_NFH<-as.data.frame(G_for_lm_somme_NFH$residuals)

res_for_somme_W<-as.data.frame(G_for_lm_somme_W$residuals)


# forest[75:115,]

res_for_aisne_t<-as.data.frame(G_for_lm_aisne_tot$residuals)

res_for_somme_n<-as.data.frame(G_for_lm_aisne_nat$residuals)

res_for_somme_a<-as.data.frame(G_for_lm_aisne_alien$residuals)

res_for_somme_FH<-as.data.frame(G_for_lm_aisne_FH$residuals)

res_for_somme_NFH<-as.data.frame(G_for_lm_aisne_NFH$residuals)

res_for_somme_W<-as.data.frame(G_for_lm_aisne_W$residuals)


# residui

# open[1:30,]
res_open_oise_t

res_open_oise_n

res_open_oise_a

res_open_oise_FH

res_open_oise_NFH

res_open_oise_W


# data=open[31:60,]

res_open_somme_t

res_open_somme_n

res_open_somme_a

res_open_somme_FH

res_open_somme_NFH

res_open_somme_W


# data=open[61:89,]

res_open_aisne_t

res_open_aisne_n

res_open_aisne_a

res_open_aisne_FH

res_open_aisne_NFH

res_open_aisne_W


# bocage

# boc[1:60,]

res_boc_oise_t

res_boc_oise_n

res_boc_oise_a

res_boc_oise_FH

res_boc_oise_NFH

res_boc_oise_W


# boc[61:92,]

res_boc_somme_t

res_boc_somme_n

res_boc_somme_a

res_boc_somme_FH

res_boc_somme_NFH

res_boc_somme_W


# boc[93:154,]

res_boc_aisne_t

res_boc_aisne_n

res_boc_aisne_a

res_boc_aisne_FH

res_boc_aisne_NFH

res_boc_aisne_W


# forest 

# data=forest[1:42,]

res_for_oise_t

res_for_oise_n

res_for_oise_a

res_for_oise_FH

res_for_oise_NFH

res_for_oise_W


# forest[44:74,]

res_for_somme_t

res_for_somme_n

res_for_somme_a

res_for_somme_FH

res_for_somme_NFH

res_for_somme_W


# forest[75:115,]

res_for_aisne_t

res_for_somme_n

res_for_somme_a

res_for_somme_FH

res_for_somme_NFH

res_for_somme_W

#plot

####summary

summary(lm(G_open_lm_oise_nat$residuals~G_open_lm_oise_alien$residuals))
summary(lm(G_open_lm_somme_nat$residuals~G_open_lm_somme_alien$residuals))
summary(lm(G_open_lm_aisne_nat$residuals~G_open_lm_aisne_alien$residuals))
summary(lm(G_boc_lm_oise_nat$residuals~G_boc_lm_oise_alien$residuals))
summary(lm(G_boc_lm_somme_nat$residuals~G_boc_lm_somme_alien$residuals))
summary(lm(G_boc_lm_aisne_nat$residuals~G_boc_lm_aisne_alien$residuals))
summary(lm(G_for_lm_oise_nat$residuals~G_for_lm_oise_alien$residuals))
summary(lm(G_for_lm_somme_nat$residuals~G_for_lm_somme_alien$residuals))
summary(lm(G_for_lm_aisne_nat$residuals~G_for_lm_aisne_alien$residuals))


#open

open_oise_plot <- ggplot(open[1:30,], aes(x=G_open_lm_oise_nat$residuals, y=G_open_lm_oise_alien$residuals)) + geom_point() + geom_smooth(method="lm")  + annotate("text", x = 1, y = 5, label = paste0("R Squared = 0.5639"))
open_somme_plot <- ggplot(open[31:60,], aes(x=G_open_lm_somme_nat$residuals, y=G_open_lm_somme_alien$residuals)) + geom_point() + geom_smooth(method="lm") + annotate("text", x = 1, y = 5, label = paste0("R Squared = 0.1437"))
open_aisne_plot <- ggplot(open[61:89,], aes(x=G_open_lm_aisne_nat$residuals, y=G_open_lm_aisne_alien$residuals)) + geom_point() + geom_smooth(method="lm") + annotate("text", x = 1, y = 5, label = paste0("R Squared = 0.3962"))

#bocage

boc_oise_plot <- ggplot(boc[1:60,], aes(x=G_boc_lm_oise_nat$residuals, y=G_boc_lm_oise_alien$residuals)) + geom_point() + geom_smooth(method="lm") + annotate("text", x = 1, y = 5, label = paste0("R Squared = 0.2732"))
boc_somme_plot <- ggplot(boc[61:92,], aes(x=G_boc_lm_somme_nat$residuals, y=G_boc_lm_somme_alien$residuals)) + geom_point() + geom_smooth(method="lm") + annotate("text", x = 1, y = 5, label = paste0("R Squared = 0.6321"))
boc_aisne_plot <- ggplot(boc[93:154,], aes(x=G_boc_lm_aisne_nat$residuals, y=G_boc_lm_aisne_alien$residuals)) + geom_point() + geom_smooth(method="lm") + annotate("text", x = 1, y = 5, label = paste0("R Squared = 0.0230"))

#forest

for_oise_plot <- ggplot(open[1:43,], aes(x=G_for_lm_oise_nat$residuals, y=G_for_lm_oise_alien$residuals)) + geom_point() + geom_smooth(method="lm") + annotate("text", x = 1, y = 5, label = paste0("R Squared = 0.2952"))
for_somme_plot <- ggplot(open[44:74,], aes(x=G_for_lm_somme_nat$residuals, y=G_for_lm_somme_alien$residuals)) + geom_point() + geom_smooth(method="lm") + annotate("text", x = 1, y = 5, label = paste0("R Squared = 0.1725"))
for_aisne_plot <- ggplot(open[75:115,], aes(x=G_for_lm_aisne_nat$residuals, y=G_for_lm_aisne_alien$residuals)) + geom_point() + geom_smooth(method="lm") + annotate("text", x = 1, y = 5, label = paste0("R Squared = 0.1550"))

#tutti
grid.arrange(open_oise_plot, open_somme_plot, open_aisne_plot, boc_oise_plot, boc_somme_plot, boc_aisne_plot, for_oise_plot, for_somme_plot, for_aisne_plot, ncol=3)


#faccio suddivisione per tipo

#open

A_open_lm_t<-lm(log(species.richness)~log(log.area), data = open)
summary(A_open_lm_t)

G_open_lm_t<-lm(species.richness~log(log.area), data = open)
summary(G_open_lm_t)

A_open_lm_n<-lm(log(native.richness)~log(log.area), data = open)
summary(A_open_lm_n)

G_open_lm_n<-lm(native.richness~log(log.area), data = open)
summary(G_open_lm_n)

A_open_lm_a<-lm(log(alien.richness+1)~log(log.area), data = open)
summary(A_open_lm_a)

G_open_lm_a<-lm(alien.richness~log(log.area), data = open)
summary(G_open_lm_a)

#bocage

A_boc_lm_t<-lm(log(species.richness)~log(log.area), data = boc)
summary(A_boc_lm_t)

G_boc_lm_t<-lm(species.richness~log(log.area), data = boc)
summary(G_boc_lm_t)

A_boc_lm_n<-lm(log(native.richness)~log(log.area), data = boc)
summary(A_boc_lm_n)

G_boc_lm_n<-lm(native.richness~log(log.area), data = boc)
summary(G_boc_lm_n)

A_boc_lm_a<-lm(log(alien.richness+1)~log(log.area), data = boc)
summary(A_boc_lm_a)

G_boc_lm_a<-lm(alien.richness~log(log.area), data = boc)
summary(G_boc_lm_a)


#forest

A_for_lm_t<-lm(log(species.richness)~log(area), data = forest)
summary(A_for_lm_t)

G_for_lm_t<-lm(species.richness~log(area), data = forest)
summary(G_for_lm_t)

A_for_lm_n<-lm(log(native.richness)~log(area), data = forest)
summary(A_for_lm_n)

G_for_lm_n<-lm(native.richness~log(area), data = forest)
summary(G_for_lm_n)

A_for_lm_a<-lm(log(alien.richness+1)~log(area), data = forest)
summary(A_for_lm_a)

G_for_lm_a<-lm(alien.richness~log(area), data = forest)
summary(G_for_lm_a)

#redisuals

res_open_t<-as.data.frame(G_open_lm_t$residuals)
res_open_n<-as.data.frame(G_open_lm_n$residuals)
res_open_a<-as.data.frame(G_open_lm_a$residuals)

res_boc_t<-as.data.frame(G_boc_lm_t$residuals)
res_boc_n<-as.data.frame(G_boc_lm_n$residuals)
res_boc_a<-as.data.frame(G_boc_lm_a$residuals)

res_for_t<-as.data.frame(G_for_lm_t$residuals)
res_for_n<-as.data.frame(G_for_lm_n$residuals)
res_for_a<-as.data.frame(G_for_lm_a$residuals)

#plot

open_plot <- ggplot(open, aes(x=G_open_lm_n$residuals, y=G_open_lm_a$residuals)) + geom_point() + geom_smooth(method="lm") + annotate("text", x = -10, y = 10, label = paste0("R Squared = 0.1947"))
boc_plot <- ggplot(boc, aes(x=G_boc_lm_n$residuals, y=G_boc_lm_a$residuals)) + geom_point() + geom_smooth(method="lm") + annotate("text", x = -10, y = 10, label = paste0("R Squared = 0.1932"))
for_plot <- ggplot(forest, aes(x=G_for_lm_n$residuals, y=G_for_lm_a$residuals)) + geom_point() + geom_smooth(method="lm") + annotate("text", x = -10, y = 10, label = paste0("R Squared = 0.1752"))


#tutti
grid.arrange(open_plot, boc_plot, for_plot, ncol=3)

####summary

summary(lm(G_open_lm_n$residuals~G_open_lm_a$residuals))
summary(lm(G_boc_lm_n$residuals~G_boc_lm_a$residuals))
summary(lm(G_for_lm_n$residuals~G_for_lm_a$residuals))

hab_lan<- lm(residuals_alien~residuals_native*window_type, data=total) #se metto + invece che * testa in modo separato i modelli
summary(hab_lan)
anova(hab_lan)
visreg(hab_lan, "residuals_native", by="window_type")
plot(allEffects(hab_lan))


#all data 

A_tot_lm_t<-lm(log(species.richness)~log(area), data=total)
summary(A_tot_lm_t)

G_tot_lm_t<-lm(species.richness~log(area), data=total)
summary(G_tot_lm_t)

A_tot_lm_n<-lm(log(native.richness)~log(area), data=total)
summary(A_tot_lm_n)

G_tot_lm_n<-lm(native.richness~log(area), data=total)
summary(G_tot_lm_n)

A_tot_lm_a<-lm(log(alien.richness+1)~log(area), data=total)
summary(A_tot_lm_a)

G_tot_lm_a<-lm(alien.richness~log(area), data=total)
summary(G_tot_lm_a)
#gleason sempre il modello migliore
#plot
plot_sp_tot_t <- ggplot(total, aes(x=log(area), y=species.richness)) + geom_point() + geom_smooth(method="lm") + coord_cartesian (ylim = c(-2, 280))
plot_sp_tot_t

tot_SAR <- ggplot(data=total, aes(x=log(area), y=native.richness)) + 
  geom_point(data=total, aes(x=log(area), y=native.richness), color='blue') + geom_abline(intercept = -95.9338, slope = 17.3177, color="blue") +
  geom_point(data=total, aes(x=log(area), y=alien.richness), color='red') + geom_abline(intercept = -11.7921, slope = 1.4950, color="red") +
  labs(y="Species richness", x = "log(Area)")

tot_SAR



#plot_sp_tot_n <- ggplot(total, aes(x=log(area), y=native.richness)) + geom_point() + geom_smooth(method="lm") + coord_cartesian (ylim = c(-2, 280))
#plot_sp_tot_a <- ggplot(total, aes(x=log(area), y=alien.richness)) + geom_point() + geom_smooth(method="lm") + coord_cartesian (ylim = c(-2, 280))
#grid.arrange(plot_sp_tot_n, plot_sp_tot_a, ncol=2)



#plot totale

tot_plot_res <- ggplot(total, aes(x=G_tot_lm_n$residuals, y=G_tot_lm_a$residuals)) + geom_point() + geom_smooth(method="lm") + annotate("text", x = -40, y = 8, label = paste0("R Squared = 0.2123")) + labs(y="Residual from alien SAR", x = "Residuals from native SAR")
tot_plot_res
tot_res <- lm(G_tot_lm_n$residuals~G_tot_lm_a$residuals)
summary(tot_res)


#varianti età

#open

open$residuals_alien <- G_open_lm_a$residuals
open$residuals_native <- G_open_lm_n$residuals
open_age<- lm(residuals_alien~residuals_native*Age, data=open) #se metto + invece che * testa in modo separato i modelli
summary(open_age)
anova(open_age)
visreg(open_age, "residuals_native")
visreg(open_age, "Age")
visreg(open_age, "residuals_native", by="Age")
plot(allEffects(open_age))
plot(residuals(open_age))
shapiro.test(residuals(open_age))

#boc

boc$residuals_alien <- G_boc_lm_a$residuals
boc$residuals_native <- G_boc_lm_n$residuals
boc_age<- lm(residuals_alien~residuals_native*Age, data=boc)
summary(boc_age) #non significativo
visreg(boc_age, "residuals_native", by="Age")
plot(allEffects(boc_age))


#forest

forest$residuals_alien <- G_for_lm_a$residuals
forest$residuals_native <- G_for_lm_n$residuals
for_age<- lm(residuals_alien~residuals_native*Age, data=forest)
summary(for_age) #non significativo
visreg(for_age, "residuals_native", by="Age")
plot(allEffects(for_age))

#totale
total$residuals_alien <- G_tot_lm_a$residuals
total$residuals_native <- G_tot_lm_n$residuals
tot_age<- lm(residuals_alien~residuals_native*Age, data=total) #se metto + invece che * testa in modo separato i modelli
summary(tot_age)
anova(tot_age)
visreg(tot_age, "residuals_native")
visreg(tot_age, "Age")
visreg(tot_age, "residuals_native", by="Age")
plot(allEffects(tot_age)) #da correggere il dataset, age index è giusto per bocage e openfield ma non va bene per forest
#nelle foreste c'è un'area totale e poi quella frammentata

#prova region
tot_reg<- lm(residuals_alien~residuals_native*region*window_type, data=total) 
summary(tot_reg)
anova(tot_reg)
plot(allEffects(tot_reg))

tot_reg_1<- lm(residuals_alien~residuals_native*region + region*window_type + residuals_native*window_type, data=total) 
summary(tot_reg_1)
anova(tot_reg_1)
plot(allEffects(tot_reg_1))

tot_reg_2<- lm(residuals_alien~residuals_native + region + window_type, data=total) 
summary(tot_reg_2)
anova(tot_reg_2)
plot(allEffects(tot_reg_2))

#### tab 1

MAM_R=lme(residuals_alien~residuals_native*window_type
          ,random=~1|region, data=total, method="REML", na.action = "na.fail")

anova(MAM_R) 


#variante woody
#quanto varia in base al numero di specie legnose presenti

#open

open_woody<- lm(residuals_alien~residuals_native*W, data=open)
summary(open_woody)
visreg(open_woody, "residuals_native", by="W")
plot(allEffects(open_woody))
cor.test(W, Age, data=open)

#boc

boc_woody<- lm(residuals_alien~residuals_native*W, data=boc)
summary(boc_woody) #non è significativo
visreg(boc_woody, "residuals_native", by="W")
plot(allEffects(boc_woody))
cor.test(boc$W, boc$Age) #vedo che all'aumentare dell'età ho anche aumento di specie legnose, ecco perchè sono due modelli uguali, stessa relazione


#forest

for_woody<- lm(residuals_alien~residuals_native*W, data=forest)
summary(for_woody)
visreg(for_woody, "residuals_native", by="W")
plot(allEffects(for_woody))


#variante NFH

open_NFH<- lm(residuals_alien~residuals_native*NFH, data=open)
summary(open_NFH) #non significativo
visreg(open_NFH, "residuals_native", by="NFH")
plot(allEffects(open_NFH))

#boc

boc_NFH<- lm(residuals_alien~residuals_native*NFH, data=boc)
summary(boc_NFH) 
visreg(boc_NFH, "residuals_native", by="NFH")
plot(allEffects(boc_NFH))


#forest

for_NFH<- lm(residuals_alien~residuals_native*NFH, data=forest)
summary(for_NFH) #non significativo
visreg(for_NFH, "residuals_native", by="NFH")
plot(allEffects(for_NFH))

#variante FH
#sembra significatica a volte ma secondo me è perchè ci sono troppe poche FH aliene

open_FH<- lm(residuals_alien~residuals_native*FH, data=open)
summary(open_FH)
visreg(open_FH, "residuals_native", by="FH")
plot(allEffects(open_FH))

#boc

boc_FH<- lm(residuals_alien~residuals_native*FH, data=boc)
summary(boc_FH) 
visreg(boc_FH, "residuals_native", by="FH")
plot(allEffects(boc_FH))


#forest

for_FH<- lm(residuals_alien~residuals_native*FH, data=forest)
summary(for_FH) #non significativo
visreg(for_FH, "residuals_native", by="FH")
plot(allEffects(for_FH))

###
#variante type
tot_res_type<- lm(residuals_alien~residuals_native*window_type, data=total)
summary(tot_res_type) #non significativo
anova(tot_res_type)
visreg(tot_res_type, "residuals_native", by="window_type")
plot(allEffects(tot_res_type))

#######
#estrapolo i dati per eliene e native di NFH, FH e W

#SP_MATRIX= read.csv(file.choose(),sep=";",header=T)
#diversity_index=matrix(c(specnumber(A_FH), 358) #358 è il numero di righe di SP_MATRIX
#write.csv(diversity_indices_tot, file = "diversity_indices_tot.csv")

#Analizzo redisuals di NFH, FH e W per aliene e native

#FH

A_nat_lm_FH<-lm(log(N_FH+1)~log(area), data=total)
summary(A_nat_lm_FH)

G_nat_lm_FH<-lm(N_FH~log(area),  data=total)
summary(G_nat_lm_FH)

A_al_lm_FH<-lm(log(A_FH+1)~log(area), data=total)
summary(A_al_lm_FH)

G_al_lm_FH<-lm(A_FH~log(area),  data=total)
summary(G_al_lm_FH)

#migliore gleason

#NFH

A_nat_lm_NFH<-lm(log(N_NFH+1)~log(area), data=total)
summary(A_nat_lm_NFH)

G_nat_lm_NFH<-lm(N_NFH~log(area),  data=total)
summary(G_nat_lm_NFH)

A_al_lm_NFH<-lm(log(A_NFH+1)~log(area), data=total)
summary(A_al_lm_NFH)

G_al_lm_NFH<-lm(A_NFH~log(area),  data=total)
summary(G_al_lm_NFH)

#gleason sempre meglio

#W

A_nat_lm_W<-lm(log(N_W)~log(area), data=total)
summary(A_nat_lm_W)

G_nat_lm_W<-lm(N_W~log(area),  data=total)
summary(G_nat_lm_W)

A_al_lm_W<-lm(log(A_W+1)~log(area), data=total)
summary(A_al_lm_W)

G_al_lm_W<-lm(A_W~log(area),  data=total)
summary(G_al_lm_W)

#Gleason sempre meglio

#redisuals

res_nat_FH<-as.data.frame(G_nat_lm_FH$residuals)

res_al_FH<-as.data.frame(G_al_lm_FH$residuals)

res_nat_NFH<-as.data.frame(G_nat_lm_NFH$residuals)

res_al_NFH<-as.data.frame(G_al_lm_NFH$residuals)

res_nat_W<-as.data.frame(G_nat_lm_W$residuals)

res_al_W<-as.data.frame(G_al_lm_W$residuals)

#plot

FH_plot <- ggplot(total, aes(x=G_nat_lm_FH$residuals, y=G_al_lm_FH$residuals)) + geom_point() + geom_smooth(method="lm")
NFH_plot <- ggplot(total, aes(x=G_nat_lm_NFH$residuals, y=G_al_lm_NFH$residuals)) + geom_point() + geom_smooth(method="lm")
W_plot <- ggplot(total, aes(x=G_nat_lm_W$residuals, y=G_al_lm_W$residuals)) + geom_point() + geom_smooth(method="lm")

#summary
summary(lm(G_nat_lm_FH$residuals~G_al_lm_FH$residuals))
summary(lm(G_nat_lm_NFH$residuals~G_al_lm_NFH$residuals))
summary(lm(G_nat_lm_W$residuals~G_al_lm_W$residuals))

#altre varianti

tot_N_NFH<- lm(residuals_alien~residuals_native*N_NFH, data=total)
summary(tot_N_NFH)
visreg(tot_N_NFH, "residuals_native", by="N_NFH")
plot(allEffects(tot_N_NFH))

####

total$residuals_N_NFH <- G_nat_lm_NFH$residuals
total$residuals_A_NFH <- G_al_lm_NFH$residuals

tot_NFH_type<- lm(residuals_A_NFH~residuals_N_NFH*window_type, data=total)
summary(tot_NFH_type)
visreg(tot_NFH_type, "residuals_N_NFH", by="window_type")
plot(allEffects(tot_NFH_type))

total$residuals_N_FH <- G_nat_lm_FH$residuals
total$residuals_A_FH <- G_al_lm_FH$residuals

tot_FH_type<- lm(residuals_A_FH~residuals_N_FH*window_type, data=total)
summary(tot_FH_type)
visreg(tot_FH_type, "residuals_N_FH", by="window_type")
plot(allEffects(tot_FH_type))
shapiro.test(residuals(tot_FH_type))
plot(residuals(tot_FH_type))

#####esperimeni sar
attach(total)
total$log_area <- log(area)
detach(total)
SAR_tot_n<- lm(native.richness~log_area*window_type, data=total) #se metto + invece che * testa in modo separato i modelli
summary(SAR_tot_n)
anova(SAR_tot_n)
visreg(SAR_tot_n, "log_area", by="window_type")
plot(allEffects(SAR_tot_n))

SAR_tot_a<- lm(alien.richness~log_area*window_type, data=total) #se metto + invece che * testa in modo separato i modelli
summary(SAR_tot_a)
anova(SAR_tot_a)
visreg(SAR_tot_a, "log_area", by="window_type")
plot(allEffects(SAR_tot_a))






