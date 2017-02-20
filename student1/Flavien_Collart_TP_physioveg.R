# permet de ne pas afficher le code et de ne montrer que le graphique + fig.cap permet de mettre une légende au  graphique
resulat <- read.csv("~/Desktop/TP-Bac3_2016/resulat.csv") #permet d'importer des données
croissance<- function (y){
  mean(resulat$longueur[resulat$jour==1 & resulat$lumière==y]- resulat$longueur[resulat$jour==0 & resulat$lumière==y])
} # fonction permettant de calculer la moyenne des différences des longueurs au jour 0 et au jour 1 pour un type de lumière
c1<-sapply(levels(resulat$lumière)[c(3, 1, 4, 5, 2)],croissance) #permet d'appliquer la fonction croissance à tous les types de lumière
ecarttype<- function (y){
  sd(resulat$longueur[resulat$jour==1 & resulat$lumière==y]- resulat$longueur[resulat$jour==0 & resulat$lumière==y])} #fonction permettant de calculer les écarts-types 
ec<-sapply(levels(resulat$lumière)[c(3, 1, 4, 5, 2)],ecarttype)
centres <- barplot(c1,xlab = "Type de lumière",ylab = "Croissance (cm)",col="royalblue3", ylim=c(0,0.8))#permet de générer un graphique de type barplot
segments(x0=centres, x1=centres, y0=c1-ec, y1=c1+ec) #permet de mettre les écarts-types dans le barplot