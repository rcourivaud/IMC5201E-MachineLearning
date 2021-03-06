#Import eds fichiers beta et gamme et conversion en data frame


create_list_score <- function(path_file_beta, path_file_gamma, use_cor){
  
  print(use_cor)
  
  final.beta = read.table(path_file_beta)
  final.gamma = read.table(path_file_gamma)
  df.beta = as.data.frame(final.beta)
  df.beta[is.na(df.beta)] <- 0
  
  df.gamma = as.data.frame(final.gamma)
  #Conversion de beta en exponentiel de chaque élement
  df.beta.exp = exp(df.beta )
  
  
  #Transposition de beta
  df.beta.exp.t = as.data.frame(t(df.beta.exp))
  if(ncol(df.beta.exp.t)==10){
    colnames(df.beta.exp.t ) <- c("K1","K2","K3","K4","K5","K6","K7","K8","K9","K10")
  }
  else if(ncol(df.beta.exp.t)==20){
    colnames(df.beta.exp.t ) <- c("K1","K2","K3","K4","K5","K6","K7","K8","K9","K10","K11","K12","K13","K14","K15","K16","K17","K18","K19","K20")
  }
  
  
  
  #Application de la régression linéaire de chaque colonne( vu qu'on a transposé),
  #en fonction des colonnes restantes
  coefficients.k1 = lm(K1~., data = df.beta.exp.t)$coefficients[2:10]
  coefficients.k2 = lm(K2~., data = df.beta.exp.t)$coefficients[2:10]
  coefficients.k3 = lm(K3~., data = df.beta.exp.t)$coefficients[2:10]
  coefficients.k4 = lm(K4~., data = df.beta.exp.t)$coefficients[2:10]
  coefficients.k5 = lm(K5~., data = df.beta.exp.t)$coefficients[2:10]
  coefficients.k6 = lm(K6~., data = df.beta.exp.t)$coefficients[2:10]
  coefficients.k7 = lm(K7~., data = df.beta.exp.t)$coefficients[2:10]
  coefficients.k8 = lm(K8~., data = df.beta.exp.t)$coefficients[2:10]
  coefficients.k9 = lm(K9~., data = df.beta.exp.t)$coefficients[2:10]
  coefficients.k10 = lm(K10~., data = df.beta.exp.t)$coefficients[2:10]
  
  if(ncol(df.beta.exp.t)==20){
    coefficients.k11 = lm(K11~., data = df.beta.exp.t)$coefficients[2:10]
    coefficients.k12 = lm(K12~., data = df.beta.exp.t)$coefficients[2:10]
    coefficients.k13 = lm(K13~., data = df.beta.exp.t)$coefficients[2:10]
    coefficients.k14 = lm(K14~., data = df.beta.exp.t)$coefficients[2:10]
    coefficients.k15 = lm(K15~., data = df.beta.exp.t)$coefficients[2:10]
    coefficients.k16 = lm(K16~., data = df.beta.exp.t)$coefficients[2:10]
    coefficients.k17 = lm(K17~., data = df.beta.exp.t)$coefficients[2:10]
    coefficients.k18 = lm(K18~., data = df.beta.exp.t)$coefficients[2:10]
    coefficients.k19 = lm(K19~., data = df.beta.exp.t)$coefficients[2:10]
    coefficients.k20 = lm(K20~., data = df.beta.exp.t)$coefficients[2:10]
  }
  
  # Creation des top 3 pour le fichier beta pour chaque régression
  top3.K1 = sort(abs(lm(K1~., data = df.beta.exp.t)$coefficients[2:10]), decreasing = TRUE)[1:3]
  top3.K2 = sort(abs(lm(K2~., data = df.beta.exp.t)$coefficients[2:10]), decreasing = TRUE)[1:3]
  top3.K3 = sort(abs(lm(K3~., data = df.beta.exp.t)$coefficients[2:10]), decreasing = TRUE)[1:3]
  top3.K4 = sort(abs(lm(K4~., data = df.beta.exp.t)$coefficients[2:10]), decreasing = TRUE)[1:3]
  top3.K5 = sort(abs(lm(K5~., data = df.beta.exp.t)$coefficients[2:10]), decreasing = TRUE)[1:3]
  top3.K6 = sort(abs(lm(K6~., data = df.beta.exp.t)$coefficients[2:10]), decreasing = TRUE)[1:3]
  top3.K7 = sort(abs(lm(K7~., data = df.beta.exp.t)$coefficients[2:10]), decreasing = TRUE)[1:3]
  top3.K8 = sort(abs(lm(K8~., data = df.beta.exp.t)$coefficients[2:10]), decreasing = TRUE)[1:3]
  top3.K9 = sort(abs(lm(K9~., data = df.beta.exp.t)$coefficients[2:10]), decreasing = TRUE)[1:3]
  top3.K10 = sort(abs(lm(K10~., data = df.beta.exp.t)$coefficients[2:10]), decreasing = TRUE)[1:3]
  
  
  if(ncol(df.beta.exp.t)==20){
    top3.K11 = sort(abs(lm(K11~., data = df.beta.exp.t)$coefficients[2:10]), decreasing = TRUE)[1:3]
    top3.K12 = sort(abs(lm(K12~., data = df.beta.exp.t)$coefficients[2:10]), decreasing = TRUE)[1:3]
    top3.K13 = sort(abs(lm(K13~., data = df.beta.exp.t)$coefficients[2:10]), decreasing = TRUE)[1:3]
    top3.K14 = sort(abs(lm(K14~., data = df.beta.exp.t)$coefficients[2:10]), decreasing = TRUE)[1:3]
    top3.K15 = sort(abs(lm(K15~., data = df.beta.exp.t)$coefficients[2:10]), decreasing = TRUE)[1:3]
    top3.K16 = sort(abs(lm(K16~., data = df.beta.exp.t)$coefficients[2:10]), decreasing = TRUE)[1:3]
    top3.K17 = sort(abs(lm(K17~., data = df.beta.exp.t)$coefficients[2:10]), decreasing = TRUE)[1:3]
    top3.K18 = sort(abs(lm(K18~., data = df.beta.exp.t)$coefficients[2:10]), decreasing = TRUE)[1:3]
    top3.K19 = sort(abs(lm(K19~., data = df.beta.exp.t)$coefficients[2:10]), decreasing = TRUE)[1:3]
    top3.K20 = sort(abs(lm(K20~., data = df.beta.exp.t)$coefficients[2:10]), decreasing = TRUE)[1:3]
  }
  #Création d'une liste nommée qui contient tous les top 3
  l.pwk  = list(top3.K1,top3.K2,top3.K3,top3.K4, top3.K5,top3.K6,top3.K7,top3.K8, top3.K9,top3.K10)
  names(l.pwk) <- c("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10")
  
  if(ncol(df.beta.exp.t)==10){
    l.pwk  = list(top3.K1,top3.K2,top3.K3,top3.K4, top3.K5,top3.K6,top3.K7,top3.K8, top3.K9,top3.K10)
    names(l.pwk) <- c("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10")
  }
  else if(ncol(df.beta.exp.t)==20){
    l.pwk  = list(top3.K1,top3.K2,top3.K3,top3.K4, top3.K5,top3.K6,top3.K7,top3.K8, top3.K9,top3.K10,
                  top3.K11,top3.K12,top3.K13,top3.K14, top3.K15,top3.K16,top3.K17,top3.K18, top3.K19,top3.K20)
    names(l.pwk) <- c("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10",
                      "V11","V12","V13","V14","V15","V16","V17","V18","V19","V20")  
  }
  
  
  #Fichier gamma
  head(df.gamma)
  
  df.gamma.test = df.gamma
  
  #Normalisation de chaque élement en fonction de la somme de la ligne
  for (i in 1:nrow(df.gamma.test)){
    df.gamma.test[i,] = df.gamma.test[i,] /sum(df.gamma.test[i,])
  }
  
  #Calcul du score avec la fonction Score = Somme (liste.normalisée*meilleursthèmes)
  #Pour chaque valeure max dans chaque ligne dans Gamma, on récupère les top 3 dans beta 
  
  score.list = list()
  
  for (i in 1:nrow(df.gamma.test)){
    score.words = list()
    for(j in 1:nrow(df.beta.exp.t)){

      sum.for.word = 0
      if(use_cor==TRUE){
        best.themes = sort(abs(df.gamma.test[i,]), decreasing = TRUE)[1]
        all.themes = c(gsub("V", "K", names(best.themes)), names(l.pwk[[names(best.themes)]]))
        for(th in all.themes){
          th.for.gamma = gsub("K", "V", th)
          
          p.theme.in.document = df.gamma.test[i,][[th.for.gamma]]
          p.word.in.document = df.beta.exp.t[j,][[th]]
          p.for.sum = p.theme.in.document*p.word.in.document
          sum.for.word = sum.for.word + p.for.sum
        }
      }
      else{
        all.themes = c("K1","K2","K3","K4","K5","K6","K7","K8","K9","K10",
                       "K11","K12","K13","K14","K15","K16","K17","K18","K19","K20")
        for(th in all.themes){
          th.for.gamma = gsub("K", "V", th)
          
          p.theme.in.document = df.gamma.test[i,][[th.for.gamma]]
          p.word.in.document = df.beta.exp.t[j,][[th]]
          p.for.sum = p.theme.in.document*p.word.in.document
          sum.for.word = sum.for.word + p.for.sum
        }
      }

    
      score.words[[j]] = sum.for.word
      score.list[[i]] = score.words
      
      #name.theme = names(best.themes)
      #score.list[[i]] = sum(l.pwk[[name.theme]]*best.themes[[name.theme]])
    }
  }
  return(score.list)
}

#Utilise tous les thèmes

final_result = create_list_score("./data/final.beta", "./data/exp20test-gamma.dat", FALSE)
ind =  sort(unlist(final_result[[1]]), decreasing = TRUE, index.return = TRUE)
idx = sort(ind$ix[0:68]-1)

ap_test <- read.table("./data/ap_test.txt", quote="\"", stringsAsFactors=FALSE)

ap_test_idx = as.integer(as.vector(unlist(as.list(ap_test))))
precision_false = length(intersect(idx, ap_test_idx))/68

#Utilise la corrélation entre les thèmes

final_result = create_list_score("./data/final.beta", "./data/exp20test-gamma.dat", TRUE)
ind =  sort(unlist(final_result[[1]]), decreasing = TRUE, index.return = TRUE)
idx = sort(ind$ix[0:68]-1)
ap_test_idx = as.integer(as.vector(unlist(as.list(ap_test))))
precision_true = length(intersect(idx, ap_test_idx))/68


install.packages("rmarkdown")
