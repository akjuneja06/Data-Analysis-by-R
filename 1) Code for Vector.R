set.seed(100)
A <- round(rnorm(100,7,4),1)
B <- paste0("D-",1:100)
c <- setNames(A,B)
c
mean(c)
d <- c > mean(c)
d
# [1] Answer for Question 1
sum(d)
e <- max(c)
f <- names(c)[c==max(c)]
e
f
#[2] Answer for Question 2
cat("The max temp was on day",f,"with a value of",e)
g <- min(c)
h <- names(c)[c==min(c)]
g
h
#[3] Answer for Question 3
cat("The min temp was on day",h,"with a value of",g)
A_warning <- ifelse(A<=4,"Warning","Normal")
A_warning1 <- setNames(A_warning,B)
#[4] Answer for Question 4
A_warning1
count_warning <- A_warning1 == "Warning"
#[5] Answer for Question 5
sum(count_warning)
days_warning <- c <= 4
days_warning1 <- names(days_warning)[days_warning=="TRUE"]
#[6] Answer for Question 6
days_warning1
Tabular_WN <- table(A_warning1)
#[7] Answer for Question 7
Tabular_WN
z <- rle(A_warning)
z
z1 <- z$lengths
z2 <- z$values
z3 <- max(z1[z2=="Normal"])
z3
z4 <- max(z1[z2=="Warning"])
z4
#[8] Answer for Question 8
cat("The maximum run of days with warnings was",z4)
cat("The maximum run of days without warnings was",z3)