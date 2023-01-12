#Initializing a pseudorandom number generator
set.seed(100)

#creating vectors with specific distribution
CX101 <- rnorm(20,45,8)
CX102 <- rnorm(20,65,8)
CX103 <- rnorm(20,85,10)
CX104 <- rnorm(20,45,10)
CX105 <- rnorm(20,60,5)

#Summary of all vectors
summary(CX101)
summary(CX102)
summary(CX103)
summary(CX104)
summary(CX105)

#Creating matrix for all vectors and set row and columns name.
res <- matrix(c(CX101,CX102,CX103,CX104,CX105),
               nrow = 20,
               ncol=5,
               dimnames = list(c(paste0("Student_",1:20)),
                               c("CX101","CX102","CX103","CX104","CX105")))

res
#Checking how many values are greater than 100
res[res[,"CX103"] > 100,]

#Creating matrix to replace <0 or >100 values with NA. 
res1 <- apply(res,2,function(x) ifelse(x>100 | x<0, NA, x))
res1

##Creating matrix to replace NA with mean of column. 
res2 <- apply(res1,2,function(x) ifelse(is.na(x),mean(x,na.rm=T),x))
res2

#calculating mean of each row.
Mean <- apply(res2,1,mean)

#calculating Range of each row.
Range <- apply(res2,1,function(x) max(x) - min(x))

#Bind Mean and Range into existing matrix
res2 <- cbind(res2,Mean,Range)
res2

#Print the highest mean with row name
res2[res2[,6] ==max(res2[,6]),, drop=F]




