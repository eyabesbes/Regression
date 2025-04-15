library(here)
data_1<- read.csv2("/home/user/Bureau/diamonds.csv",sep=',')
library(tidyverse)
View(data_1)
x <- data_1 %>%
  select("carat","depth","table","x","y","z") %>%
  drop_na()
View(x)

y<- data_1 %>%
  select("price") %>%
  drop_na()
view(y)

regression1=lm(data_1$price~data_1$carat+data_1$depth+data_1$table+data_1$x+data_1$y+data_1$z,data=data_1)
regression1
summary(regression1)
df<- data.frame(x,y)
df
pairs(df)

#Evaluation des hypothèses de validité des résultats
acf(residuals(regression1), main="regression1") #Evaluation de l'hypothèse d'indépendance des résidus
#Les pointillées horizontaux, sont les intervalles de confiance du coefficient de corrélation égal à 0. Les traits verticaux représentent les coefficients de corrélation entre les résidus de chaque point et ceux des points de la ligne suivante (lag=1), ou ceux séparés de deux lignes (lag=2) etc…
#Evaluation de l'hypothèse de normalité des résidus
plot(regression1,2) 
#Cette hypothèse peut s’évaluer graphiquement à l’aide d’un QQplot. Si les résidus sont bien distribués le long de la droite figurant sur le plot, alors l’hypothèse de normalité est acceptée. A l’inverse, s’ils s’en écartent, alors l’hypothèse de normalité est rejetée.
confint(regression1) #Intervalles de confiance des coefficients des paramètres
#Predictions
my_df <- data.frame(carat=c(0.23),depth=c(61.5),table=c(55),x=c(3.95),y=c(3.98),z=c(2.43))
predict(regression1, newdata=my_df)
#Manipulation des composants de la régression
fitted(regression1)
#Les fitted, correspondent, aux prédictions du modèle de régression, mais pour les valeurs observées de la variable prédictive :
#La matrice de variance - covariance:
vcov(regression1)
#Représentation finale de la régression
ggplot(df, aes(y=y, x=x))+
  geom_point()+
  geom_smooth(colour="red", method="lm", fill="red") +
  ylab("diamonds")+
  xlab("caractéristiques") +
  theme_classic()
