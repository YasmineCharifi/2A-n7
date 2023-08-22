#Question 1:

data = read.table("Data.txt",header=TRUE)
summary(data)
as.factor

# Question 2: Regression simple

lm.out1=lm(O3~O3v,data=data)
summary(lm.out1)

x11()
plot(data$O3,data$O3v,xlab="O3v",ylab="O3",
     main="PREVISIONS",pch='+')
abline(lm.out1,col="red")

# -> la regression simple explique 46% de la variance totale du predictand
#comme p_value<0.05 pour le parametre O3v ceci signifie que son comportement
#est different à celui de 03


#Question 3: Modèles sans interaction

lm.out2=lm(O3~O3v+T+N+FF+DD+RR,data=data)
summary(lm.out2)

#-> On remarque que ce modèle explique 65% de la variance totale du predictant

# Analyse du modèle lm.out2 :
#-> Comme p_value = 0.0002<0.05 alors on remarque que la variable N a un impact
#significatif sur le predictant

#-> certaines modalites du facteur DD menent a des effets differentiels jugés
# significatifs. O3 se comporte differemment à DDNord et DDSud par rapport à
# DDEst alors que DDOuest a des comportements moyens trés proches de DDEst.

#->Vérifions les hypthèses du cadre théorique du modèle linéaire gaussien :
# +Homoscédasticité:
x11()
plot(fitted(lm.out2),residuals(lm.out2))
#Vu le graphe obtenu, l'homoscédatsticité est vérifiée; les points sont potentiellement proches
# du zero
# +Normalité:
x11()
hist(residuals(lm.out2)) ; qqnorm(residuals(lm.out2))
## les queues de distribution s'eloignent de la normalite, mais globalement 
#l'hypothèse de normalite ne pose pas probleme sur une large plage puisque en depit
"
# +Indépendance :
x11()
acf(residuals(lm.out2))
#D'apres le graphe obtenu, on peut conclure que l'independance est bien vérifiée
#les acf sont bien dans La plage d'independance
# +Linéarité :
x11()
plot(fitted(lm.out2),data$O3)
# l'hypothese de linearite est adaptée

# -> Les prédicteurs qu'on conserverait sont : O3v, T, N, DD puisque
# ils sont les seuls predicteurs qui ont un impact sur le modèle et ceci est
# du au fait que leurs p_value est bien inférieure à alpha = 0.05

# Estimation issu d'une selection exploitant l'indice BIC
library(MASS)
lm.outBIC=stepAIC(lm.out2,k=log(nrow(data)))
summary(lm.outBIC)
formula(lm.outBIC)
# la sélection automatique descendante utilisant le critere BIC ne retire 
#que les facteurs FF et RR,comme la selection manuelle precedente.
 
# Question 4: Modèles avec interactions

regint=lm(O3~.*.,data)
summary(regint)
lm.outBICint=stepAIC(regint,k=log(nrow(data)))
formula(lm.outBICint)
summary(lm.outBICint)
#-> La dimension de ce nouveau modèle d'apres les summary est 7
#-> On remarque que le pourcentagge d'explication a augmenté par rapport aux
# autres modèles (85%) ce qui montre que ce modèle s'avère plus pertinent que
# ceux d'avant mais R augmente en augmentant le nombre de prédicteurs


#Question 5: Visualisation des prévisions

x11()
plot(data$O3,type ="l",lwd=2,main="Concentration d'ozone � Aix",xlab="Date",ylab="[O3]")
points(fitted(lm.outBICint),col="blue",pch="+")
points(fitted(lm.out1),col="red",pch="+")
legend(0,256,pch="+",col=c("blue","red"),legend=c("BICint","out1"),bty="n") 
# -> pour la regression linéaire, les prevision sont tirées vers la moyenne 
# du predictand mais est incapable d'atteindre les valeurs extremes
# la variabilité du predictand est mal reproduite, il faut enrichir
# la regression pour obtenir une flexibilite plus forte du modele.
# mais par contre on voit que le modele lm.outBICint en bleu ameliore la 
# regression simple en rouge en permettant une plus grande variabilite
# aux previsions statistiques (modele plus flexible)


#Question 6: Evalusation des modèles

# Creation des fichiers d'apprentissage et de test
nappr=ceiling(0.9*nrow(data))
ii=sample(1:nrow(data),nappr)
jj=setdiff(1:nrow(data),ii)
datatest=data[jj,]
datapp=data[ii,]

# Entrainement des modeles sur le fichier datapp
lm.out1=lm(O3~O3v,datapp)
lm.outBIC=lm(formula(lm.outBIC),datapp)
lm.outBICint=lm(formula(lm.outBICint),datapp)

# fonction calculant le biais et le RMSE
score=function(obs,prev) {
rmse=sqrt(mean((prev-obs)**2))
biais=mean(prev-obs)
print("Biais  RMSE") 
return(round(c(biais,rmse),3))
}


score(datapp$O3,fitted(lm.out1))
score(datapp$O3,fitted(lm.outBIC))
score(datapp$O3,fitted(lm.outBICint))
# Sans surprise plus le modele est complexe, plus il est performant sur les donnees d'apprentissage,
# le biais est nul par construction.



score(datatest$O3,predict(lm.out1,datatest))
score(datatest$O3,predict(lm.outBIC,datatest))
score(datatest$O3,predict(lm.outBICint,datatest))
# Les scores se degradent sur test, des biais apparaissent, le modele le plus
# complexe etant le moins robuste.

source("CV.R") 

# Sur le graphe genere, scores RMSE sur fichier d'apprentissage en bleu puis 
#sur fichier test en rouge, le modele simple mois efficace, bic et bicint sont robustes
# les ecarts etant jugés peu significatifs puisque les boites ont des zones communes
# Si plusieurs modeles menaient a des performances proches (ecart non significatif) 
#C'est bien le cas de outBIC et outBICint, aucun modele ne se distingue 
# il faudrait alors proposer le modele de plus faible complexite, c'est un gage de robustesse
#le bicint etant significativement le meilleur sur test c'est le modele a proposer au final.
