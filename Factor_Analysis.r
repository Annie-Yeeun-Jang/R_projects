setwd("C:\Users\User\Desktop\Factor_Analysis")

# We use the package 'bootstrap' because it contains the data set
# 'scor' on open-closed book exams discussed in MKB
# You need to install the package once, and can then load it via
# 'library(bootstrap)'

install.packages("bootstrap")
library(bootstrap)

# load data:
data(scor)
help(scor)

# compute correlation matrix:
(cor <- cor(scor)) 

# remove diagonal:
(diag.1 <- diag(rep(1,5)))
(cor.min <- cor - diag.1)  # have 0s on diagonal
#Represents how each test results correlates with other test results 
#Set those values as inital h

# compute max_{i!=j} |r_{ij}| for each column
(h2 <- apply(abs(cor.min), 2, max)) #max in columns

# this is our initial estimate of $h_i^2$

# compute eigenvalues and eigenvectors of reduced correlation matrix
(cor.reduced <- cor.min + diag(h2))  # this is R - \hat Psi
(eig <- eigen(cor.reduced))

# let's consider the model with k=1:
(pf.1 <- eig$vectors[,1] * eig$values[1]^{1/2})
# h^2:
pf.1^2

# let's consider the model with k=2
(pf.2 <- eig$vectors[,1:2] %*% diag(eig$values[1:2]^{1/2}))
# h^2:
apply(pf.2^2, 1, sum)
# note that factor loadings of first component remain unchanged

# repeat the procedure for the 2-factor model
# update estimates for commonalities, and repeat 100 times:
for (i in 1:100){
  h2 <- apply(pf.2^2, 1, sum)
  cor.reduced <- cor.min + diag(h2)
  eig <- eigen(cor.reduced)
  pf.2 <- eig$vectors[,1:2] %*% diag(eig$values[1:2]^{1/2})} #compute lambda, repear for 100 times
pf.2 #final lambda

(h2 <- apply(pf.2^2, 1, sum))

# check that constraint 2 is satisfied:
round(t(pf.2) %*% pf.2, 5)

# check how close our estimated Lambda*Lambda' + Psi is to
# the correlation matrix R
(fit <- pf.2 %*% t(pf.2) + diag(1-h2))
cor(scor)
round(cor(scor)-fit, 5)

# compare results to PCA
prcomp(scor, scale=TRUE, center=TRUE)
# interpretation is qualitatively the same
