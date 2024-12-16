### 202482123 이종철
library(gbm)
library(randomForest)

rename_and_reorder_columns <- function(data, target_column) {
  if (!is.data.frame(data)) {
    stop("Input must be a data frame.")
  }
  
  if (!(target_column %in% colnames(data))) {
    stop(paste("Column", target_column, "not found in the data frame."))
  }
  
  old_names <- colnames(data)
  other_columns <- setdiff(old_names, target_column)
  new_names <- paste0("X", seq_along(other_columns))
  renamed_columns <- setNames(data, c(new_names, "y")[match(old_names, c(other_columns, target_column))])
  reordered_data <- renamed_columns[, c(new_names, "y")]
  
  return(reordered_data)
}

stack8.3.0 <- function(trainset, testset) {
  stack_fold <- sample(rep(1:5, nrow(trainset)%/%5+1), size=nrow(trainset))
  n_models1 <- 8
  n_models2 <- 3
  
  stage1_cv_pred_mat <- matrix(0, nrow=nrow(trainset), ncol=n_models1)
  stage1_test_pred_list <- lapply(1:n_models1, function(x) matrix(0,
                                                                  nrow = nrow(testset),
                                                                  ncol = max(stack_fold)))
  
  for (i in 1:max(stack_fold)) {
    fit1 <- gbm(y~.,
                data = trainset[stack_fold!=i,],
                distribution = "gaussian",
                n.trees = 500,
                verbose = F,
                shrinkage = 0.1)
    fit2 <- gbm(y~.,
                data = trainset[stack_fold!=i,],
                distribution = "gaussian",
                n.trees = 200,
                verbose = F,
                shrinkage = 0.05)
    fit3 <- randomForest(y~.,
                         data = trainset[stack_fold!=i,],
                         ntree = 300,
                         do.trace=F)
    fit4 <- gbm(y~.,
                data = trainset[stack_fold!=i,],
                distribution = "gaussian",
                n.trees = 150,
                verbose = F,
                shrinkage = 0.08)
    fit5 <- randomForest(y~.,
                         data = trainset[stack_fold!=i,],
                         ntree = 200,
                         do.trace=F)
    fit6 <- lm(y~., data = trainset[stack_fold!=i,])
    fit7 <- randomForest(y~.,
                         data = trainset[stack_fold!=i,],
                         ntree = 500,
                         do.trace=F)
    fit8 <- gbm(y~.,
                data = trainset[stack_fold!=i,],
                distribution = "gaussian",
                n.trees = 300,
                verbose = F,
                shrinkage = 0.13)
    
    
    stage1_cv_pred_mat[stack_fold==i,1] <-
      predict(fit1, newdata = trainset[stack_fold==i,])
    stage1_cv_pred_mat[stack_fold==i,2] <-
      predict(fit2, newdata = trainset[stack_fold==i,])
    stage1_cv_pred_mat[stack_fold==i,3] <-
      predict(fit3, newdata = trainset[stack_fold==i,])
    stage1_cv_pred_mat[stack_fold==i,4] <-
      predict(fit4, newdata = trainset[stack_fold==i,])
    stage1_cv_pred_mat[stack_fold==i,5] <-
      predict(fit5, newdata = trainset[stack_fold==i,])
    stage1_cv_pred_mat[stack_fold==i,6] <-
      predict(fit6, newdata = trainset[stack_fold==i,])
    stage1_cv_pred_mat[stack_fold==i,7] <-
      predict(fit7, newdata = trainset[stack_fold==i,])
    stage1_cv_pred_mat[stack_fold==i,8] <-
      predict(fit8, newdata = trainset[stack_fold==i,])
    
    
    stage1_test_pred_list[[1]][,i] <- 
      predict(fit1, newdata = testset)
    stage1_test_pred_list[[2]][,i] <- 
      predict(fit2, newdata = testset)
    stage1_test_pred_list[[3]][,i] <- 
      predict(fit3, newdata = testset)
    stage1_test_pred_list[[4]][,i] <- 
      predict(fit4, newdata = testset)
    stage1_test_pred_list[[5]][,i] <- 
      predict(fit5, newdata = testset)
    stage1_test_pred_list[[6]][,i] <- 
      predict(fit6, newdata = testset)
    stage1_test_pred_list[[7]][,i] <- 
      predict(fit7, newdata = testset)
    stage1_test_pred_list[[8]][,i] <- 
      predict(fit8, newdata = testset)
  }
  
  stage1_test_pred_mat <-
    lapply(stage1_test_pred_list, function(t) apply(t, 1, mean))
  stage1_test_pred_mat <-
    sapply(stage1_test_pred_mat, identity)
  
  stage2_trainset <- data.frame(stage1_cv_pred_mat, y=trainset$y)
  stage2_testx <- data.frame(stage1_test_pred_mat)
  
  stage2_cv_pred_mat <- matrix(0, nrow=nrow(trainset), ncol=n_models2)
  stage2_test_pred_list <- lapply(1:n_models2,
                                  function(x) matrix(0, nrow = nrow(testset),
                                                     ncol = max(stack_fold)))
  
  stack_fold <- sample(rep(1:5, nrow(trainset)%/%5+1), size=nrow(trainset))
  
  for (i in 1:max(stack_fold)) {
    fit1 <- gbm(y~.,
                data = stage2_trainset[stack_fold!=i,],
                distribution = "gaussian",
                n.trees = 200,
                verbose = F,
                shrinkage = 0.05)
    fit2 <- gbm(y~.,
                data = stage2_trainset[stack_fold!=i,],
                distribution = "gaussian",
                n.trees = 250,
                verbose = F,
                shrinkage = 0.05)
    fit3 <- gbm(y~.,
                data = stage2_trainset[stack_fold!=i,],
                distribution = "gaussian",
                n.trees = 150,
                verbose = F,
                shrinkage = 0.05)
    
    stage2_cv_pred_mat[stack_fold==i,1] <-
      predict(fit1, newdata = stage2_trainset[stack_fold==i,])
    stage2_cv_pred_mat[stack_fold==i,2] <-
      predict(fit2, newdata = stage2_trainset[stack_fold==i,])
    stage2_cv_pred_mat[stack_fold==i,3] <-
      predict(fit3, newdata = stage2_trainset[stack_fold==i,])
    
    stage2_test_pred_list[[1]][,i] <- 
      predict(fit1, newdata = stage2_testx)
    stage2_test_pred_list[[2]][,i] <- 
      predict(fit2, newdata = stage2_testx)
    stage2_test_pred_list[[3]][,i] <- 
      predict(fit3, newdata = stage2_testx)
  }
  
  stage2_test_pred_mat <- 
    lapply(stage2_test_pred_list, function(t) apply(t, 1, mean))
  stage2_test_pred_mat <-
    sapply(stage2_test_pred_mat, identity)
  
  stack_pred <- apply(stage2_test_pred_mat, 1, mean)
  
  gbm1 <- gbm(y~., data = trainset,
              verbose = F, n.trees = 220,
              shrinkage = 0.05,
              distribution = "gaussian")
  gbm2 <- gbm(y~., data = trainset,
              verbose = F, n.trees = 200,
              shrinkage = 0.05,
              distribution = "gaussian")
  gbm3 <- gbm(y~., data = trainset,
              verbose = F, n.trees = 180,
              shrinkage = 0.05,
              distribution = "gaussian")
  
  y_pred1 <- predict(gbm1, newdata = testset)
  y_pred2 <- predict(gbm2, newdata = testset)
  y_pred3 <- predict(gbm3, newdata = testset)
  
  final_pred <- (0.7*stack_pred + 1.1*y_pred1 +
                   1.1*y_pred2 + 1.1*y_pred3)/4
  
  return(final_pred)
}


main <- function(x.train, y.train, x.test) {
  trainset <- data.frame(x.train, y=y.train)
  testset <- data.frame(x.test)
  
  trainset$V1 <- log(log(trainset$V1+1e-10))
  trainset$V4 <- log(trainset$V4+1e-10)
  trainset$V11 <- log(trainset$V11+1e-10)
  
  testset$V1 <- log(log(testset$V1+1e-10))
  testset$V4 <- log(testset$V4+1e-10)
  testset$V11 <- log(testset$V11+1e-10)
  
  trainset <- rename_and_reorder_columns(trainset, "y")
  colnames(testset) <- colnames(trainset)[colnames(trainset)!="y"]
  
  y <- stack8.3.0(trainset = trainset, testset = testset)
  return(y)
}
