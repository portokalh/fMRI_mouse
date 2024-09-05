library(dplyr)
library(pROC)
# noreadcsf=c(148,152,161,314,318,327) # dont read csf already in matlab

load(file='connectivity.rda')
load(file='response.rda')




not_in_Atlas = c(23, 86, 139, 150, 189, 274, 296, 316)
present_rois = setdiff(1:332, not_in_Atlas)


connectivity_new = array(0,c(332 , 332, dim(connectivity)[3]))

for (i in 1:dim(connectivity)[3]) {
  connectivity_new[present_rois,present_rois,i] = connectivity[,,i]
}
connectivity = connectivity_new

not_in_Atlas_symm = -1
for (i in not_in_Atlas) {
  if ( i + 166 <=332) {if (!(i+166)%in% not_in_Atlas) { not_in_Atlas_symm = c(not_in_Atlas_symm, i+166)}  } 
  if ( i - 166 >=0) {if (!(i-166)%in% not_in_Atlas) { not_in_Atlas_symm = c(not_in_Atlas_symm, i-166) }  } 
}
not_in_Atlas_symm = not_in_Atlas_symm[-1]

for (i in 1:dim(connectivity)[3]) {
  connectivity[not_in_Atlas_symm,,i] = 0
  connectivity[,not_in_Atlas_symm,i] = 0
}



# temp=connectivity[-noreadcsf,-noreadcsf,1]
temp=connectivity[,,1]
indexlower=lower.tri(temp, diag=FALSE)
indexlowertrue=which(indexlower==TRUE)
temp=temp[indexlower]
len=sum(indexlower)  



#riskfactors=matrix(NA,  dim(response)[1], (dim(response)[2]-1))
riskfactors=response [ , c("Diet","Distance", "SW_Distance", "Sex", "Age", "Genotype", "Lifestyle"  ) ]
# riskfactors = riskfactors %>% select(-c( "DOB", "CT.Date"))
# riskfactors  = riskfactors[ , -c(6,7) ]
table(response$Genotype)
table(response$Diet)



##########no HN
# indeceis_apoe_no_hn= riskfactors$Genotype %in% c("APOE22", "APOE33", "APOE44", "APOE22HN", "APOE33HN", "APOE44HN")
# riskfactors=riskfactors[indeceis_apoe_no_hn,]
# riskfactors=riskfactors%>%dplyr::select(Sex, Age_Months)
dim(connectivity)
# connectivity = connectivity [ , , indeceis_apoe_no_hn , drop =T] 
# 
# HN_index <- grep("HN",riskfactors$Genotype)
# riskfactors$HN <- 1
# riskfactors$HN[HN_index] <- -1

Gene=riskfactors$Genotype
Gene[Gene=="APOE22"]=2
Gene[Gene=="APOE33"]=3
Gene[Gene=="APOE44"]=4
Gene[Gene=="APOE22HN"]=2
Gene[Gene=="APOE33HN"]=3
Gene[Gene=="APOE44HN"]=4
Gene[Gene=="KO"]=0
riskfactors$Genotype=as.numeric(Gene)

Sex=riskfactors$Sex
Sex[Sex=="male"]=-1
Sex[Sex=="female"]=1
riskfactors$Sex=as.numeric(Sex)

Diet=riskfactors$Diet
Diet[Diet=="Control"]=-1
Diet[Diet=="HFD"]=1
riskfactors$Diet=as.numeric(Diet)



Exercise=riskfactors$Lifestyle
Exercise[Exercise=="Sedentary"]=-1
Exercise[Exercise=="Exercise"]=1
riskfactors$Lifestyle=as.numeric(Exercise)


riskfactors$Age=as.numeric(riskfactors$Age)

riskfactors=as.data.frame(riskfactors)
riskfactors[] <- lapply(riskfactors, as.numeric)
class(riskfactors)


riskfactors_orig = riskfactors


# riskfactors1=riskfactors%>%select(Sex, Age_Months, Genotype, Diet)
#riskfactors2=riskfactors%>%select(NormSWDist, Distance, Winding)
# riskfactors2=riskfactors%>%select( NormSWDist, Winding)

# inddz1=0
# for (i in 1:dim(riskfactors)[2]) if(sd(riskfactors[,i])==0 ) {inddz1=rbind(inddz1,i);  cat ( i , sd(riskfactors[,i]), "\n" );}
# if (length(inddz1)>1){
#   inddz1=inddz1[2:dim(inddz1)[1]]
#   riskfactors=riskfactors[,-inddz1]
# }
# 


# 
# inddz2=0
# for (i in 1:dim(riskfactors2)[2]) if(sd(riskfactors2[,i])==0 ) {inddz2=rbind(inddz2,i);  cat ( i , sd(riskfactors2[,i]), "\n" );}
# if (length(inddz2)>1){
#   inddz2=inddz2[2:dim(inddz2)[1]]
#   riskfactors2=riskfactors2[,-inddz2]
# }

# temp=connectivity[-noreadcsf,-noreadcsf,1]
temp=connectivity[,,1]
indexlower=lower.tri(temp, diag=FALSE)
indexlowertrue=which(indexlower==TRUE)
temp=temp[indexlower]
len=sum(indexlower)  
len = 48*47/2
image=matrix(NA,  dim(connectivity)[3], len) # -6 becasue of cfs removal





largest_24=c(57,
             15,
             16,
             19,
             20,
             25,
             27,
             32,
             33,
             37,
             42,
             51,
             59,
             60,
             62,
             64,
             65,
             66,
             69,
             72,
             73,
             81,
             82,
             89)

largest_24 = c(largest_24 , largest_24+166)


for (i in 1:dim(connectivity)[3]){
  # temp=connectivity[-noreadcsf,-noreadcsf,i]
  temp=connectivity[largest_24,largest_24,i]
  # temp[-largest_24,]=0
  # temp[,largest_24]=0
  indexlower=lower.tri(temp, diag=FALSE)
  temp=temp[indexlower]
  temp[abs(temp)<0.3] = 0 
  image[i,]=temp
}
dim(image)




# indd=0
# for (i in 1:dim(image)[2]) if(sd(image[,i])==0 ) {indd=rbind(indd,i);  cat ( i , sd(image[,i]), "\n" );}
# if (length(indd)>1){
#   indd=indd[2:dim(indd)[1]]
#   image=image[,-indd] }
# 
# 
# 
# 





#lets run
## Not run:
#install.packages("PMA")
#install.packages("https://gitlab.oit.duke.edu/am983/PMA2/-/archive/master/PMA2-master.tar.gz", repos = NULL, type="source")
library(PMA)
library(glmnet)
set.seed(3189) #for reproductivity
train_index <- sample(1:nrow(image), 0.8 * nrow(image))


alphas <- seq(0,1,by=0.05)


#lets run
## Not run:
#install.packages("PMA")
#install.packages("https://gitlab.oit.duke.edu/am983/PMA2/-/archive/master/PMA2-master.tar.gz", repos = NULL, type="source")
library(PMA)
library(glmnet)
set.seed(3189) #for reproductivity




image <- cbind(image, riskfactors$Sex, riskfactors$Age, riskfactors$Diet, riskfactors$Lifestyle,riskfactors$Genotype  )
penalty_factors <- c(rep(1, 48*47/2), rep(0, 5))

train_index <- sample(1:nrow(image), 0.8 * nrow(image))


alphas <- seq(0,1,by=0.1)

  
  #i = 7
  y <- 100* riskfactors$SW_Distance/riskfactors$Distance 
  # y <- (y - mean(y))/sd(y)
  # hist(y)
  cv_results <- sapply(alphas, function(alpha) {
    cv_fit <- cv.glmnet(image[train_index,], y[train_index], alpha = alpha, nfolds = 5, penalty.factor = penalty_factors)
    # cv_fit <- cv.glmnet(image[train_index,], y[train_index], alpha = alpha, nfolds = 5)
    min(cv_fit$cvm)
  })
  
  best_alpha <- alphas[which.min(cv_results)]
  plot(cv_results, x= alphas)  
  cv_fit_final = cv.glmnet(image[train_index,], y[train_index], alpha = best_alpha, nfolds = 5, penalty.factor = penalty_factors)
  # cv_fit_final = cv.glmnet(image[train_index,], y[train_index], alpha = best_alpha, nfolds = 5)
  plot(cv_fit_final)
  
  best_lambda <- cv_fit_final$lambda.min  
  
  final_model <- glmnet(image[train_index,], y[train_index], alpha = best_alpha, lambda = best_lambda, penalty.factor = penalty_factors)
  # final_model <- glmnet(image[train_index,], y[train_index], alpha = best_alpha, lambda = best_lambda)
  length(coef(final_model))
  
  
  
  
  
  
  
  # Predict on the training data
  predictions <- predict(final_model, newx = image[train_index, ])
  
  # Calculate the residuals
  residuals <- y[train_index] - predictions
  
  # Compute the RMSE
  rmse <- sqrt(mean(residuals^2))
  
  # Report the RMSE
  rmse
  sd(y[train_index])
  
  
  final_model
  
  ####################
  n_bootstraps = 1000

  coeff_matrix <- matrix(0, nrow = n_bootstraps, ncol =   (length(coef(final_model))))

  # Bootstrap procedure
  set.seed(123)  # for reproducibility
  for (i in 1:n_bootstraps) {
    
    
    
    
    # Resample with replacement
    bootstrap_indices <- sample(train_index, length(train_index), replace = TRUE)

    
    
    cv_results <- sapply(alphas, function(alpha) {
      cv_fit <- cv.glmnet(image[bootstrap_indices,], y[bootstrap_indices], alpha = alpha, nfolds = 5, penalty.factor = penalty_factors)
      # cv_fit <- cv.glmnet(image[train_index,], y[train_index], alpha = alpha, nfolds = 5)
      min(cv_fit$cvm)
    })
    
    best_alpha <- alphas[which.min(cv_results)]
    
    
    
    # Fit the model on the bootstrap sample
    # bootstrap_model <- glmnet(image[bootstrap_indices, ], y[bootstrap_indices],
    #                           alpha = best_alpha, lambda = best_lambda, penalty.factor = penalty_factors)
    bootstrap_model_cv <- cv.glmnet(image[bootstrap_indices, ], y[bootstrap_indices],
                              alpha = best_alpha, penalty.factor = penalty_factors)
    
    bootstrap_model <- glmnet(image[bootstrap_indices, ], y[bootstrap_indices],
                                    alpha = best_alpha, lambda = bootstrap_model_cv$lambda.min, penalty.factor = penalty_factors)
    # Store the coefficients
    coeff_matrix[i, ] <- as.vector(coef(bootstrap_model))  # Exclude intercept
    cat("alpha=" ,best_alpha , " and ",i, "bootstrapping \n")
  }

  # Calculate the frequency of non-zero coefficients
  freq_non_zero <- apply(coeff_matrix != 0, 2, sum)

  # Select coefficients that appear in 900 or more bootstrap samples
  selected_coefficients <- which(freq_non_zero >= 750)

  # Calculate the average value of these selected coefficients
  average_coefficients <- apply(coeff_matrix[, selected_coefficients], 2, mean)

  # Final model with selected coefficients
  final_coefficients <- rep(0, (length(coef(final_model))))
  final_coefficients[selected_coefficients] <- average_coefficients

  # Print or return the final coefficients
  final_coefficients
  final_coefficients[(length(final_coefficients)-4):length(final_coefficients)]
  colnames(image)[(length(final_coefficients)-4):length(final_coefficients)]
  
  # save(final_coefficients, file= "final_coef_boot.rda")
  
  
  # Predict on the test data manually using the final_coefficients
  test_predictions <- image[-train_index, ] %*% final_coefficients[-1]  + final_coefficients[1]
  
  # Calculate the residuals
  test_residuals <- y[-train_index] - test_predictions
  
  # Compute the RMSE for the test data
  test_rmse <- sqrt(mean(test_residuals^2))
  
  # Report the RMSE
  test_rmse
  
  sd(y[-train_index])
  
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  
  
  
  #######################
  
  
  
  
  
  
  
  
  # 
  # 
  # y_pred <- predict(final_model, s = best_lambda, newx = image[-train_index,])
  # 
  # # Calculate RMSE
  # print(rmse <- sqrt(mean((y[-train_index] - y_pred)^2)))
  # print(sd(y[-train_index]))
  
  
  
  #### no intercept 
  edge_vec = final_coefficients[-1]
  edge_vec = edge_vec[1:(48*47/2)]
  print(sum(edge_vec!=0))
  edge_matrix_48 = matrix(0, 48, 48)
  
  edge_matrix_48[lower.tri(edge_matrix_48, diag = FALSE)] <- edge_vec
  edge_matrix_48 = edge_matrix_48 + t(edge_matrix_48)
  
  edge_matrix_332 = matrix(0,332,332)
  
  edge_matrix_332[largest_24,largest_24] = edge_matrix_48
  
  edge_vector = edge_matrix_332[lower.tri(edge_matrix_332, diag = FALSE)]
  
  
  save(file = "distance_Elastic_net_boot.rda", edge_vector)
    