final.beta.1 <- read.table("~/Cours/IMC-5101B/TP2/data/run1/final.beta", quote="\"", comment.char="")
final.beta.2 <- read.table("~/Cours/IMC-5101B/TP2/data/run2/final.beta", quote="\"", comment.char="")

final.gamma.1 <- read.table("~/Cours/IMC-5101B/TP2/data/run1/final.gamma", quote="\"", comment.char="")
final.gamma.2 <- read.table("~/Cours/IMC-5101B/TP2/data/run2/final.gamma", quote="\"", comment.char="")

#Détermination des thèmes stables
# Pour cela on réalise deux run de la fonction LDA
# On test ensuite pour chaque mot du premier run quels sont les mots du deuxieme
# run les plus proches. Cela permet de mettre en valeur les thèmes les plus stable.
# On boucle sur les lignes du premier fichier beta et sur les mots du deuxieme fichier.
# On récupère ensuite les thèmes les pus pertinents.

for (i in 1:2275){
  print(i)
  for(j in 1:2275){
    print(j)
    final.gamma.1.sorted = sort(final.gamma.1[i,], decreasing=TRUE)
    final.gamma.2.sorted = sort(final.gamma.2[j,], decreasing=TRUE)
    
    vect.gamma.1 = final.gamma.1[i,]
    vect.gamma.2 = final.gamma.2[j,]
    for(k in 1:10){
      best.theme.gamma.1 = which(match(vect.gamma.1 ,final.gamma.1.sorted[k])==1)[1]
      best.theme.gamma.2 = which(match(vect.gamma.2 ,final.gamma.2.sorted[k])==1)[1]
      beta.list.theme1 = final.beta.1[best.theme.gamma.1,]
      beta.list.theme2 = final.beta.2[best.theme.gamma.2,]
      res = sum(beta.list.theme1*log(beta.list.theme1/beta.list.theme2))
      
    }
  }
}
