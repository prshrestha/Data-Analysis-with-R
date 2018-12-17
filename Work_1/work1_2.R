a = 2.3; b = 2; c = 4.97347348234723423; d = "cat"
typeof(a); typeof(b); typeof(c); typeof(d)

#cast double to integer
b = as.integer(b)
typeof(b)

# alternateve to casting
b = 5L; typeof(b)

# arrays
z = seq(1, 20,length.out = 20) # create a vector 1,2,..,20
x = array(data = z, dim = c(4, 5)) # create a 2-d array
x

# matrix transpose
t(x)
# matrix product
y = x %*% t(x)
y

# Dataframe
vecn = c("John Smith","Jane Doe")
veca = c(42, 45)
vecs = c(50000, 55000)
df = data.frame(name = vecn, age = veca, salary = vecs)
df
## name age salary
## 1 John Smith 42 50000
## 2 Jane Doe 45 55000
names(df) = c("NAME", "AGE", "SALARY") # modify column names
df
## NAME AGE SALARY
## 1 John Smith 42 50000
## 2 Jane Doe 45 55000

# summary of df
summary(df)

# Time
a = 1:10
# compute sum of squares using a for loops
c = 0
for (e in a) c = c + e^2
c
## [1] 385
# same operation using vector arithmetic
sum(a^2)
## [1] 385
# time comparison with a million elements
a = 1:1000000; c = 0
system.time(for (e in a) c = c+e^2)
# user  system elapsed 
# 0.042   0.000   0.043 

system.time(sum(a^2))
# user  system elapsed 
# 0.004   0.002   0.006 

# Matrix multiplication
n = 100; nsq = n*n
# generate two random matrices
A = matrix(runif(nsq), nrow = n, ncol = n)
B = matrix(runif(nsq), nrow = n, ncol = n)

# matrix multiplication with loops
matMult=function(A, B, n) {
  R=matrix(data = 0, nrow = n, ncol = n)
  for (i in 1:n)
    for (j in 1:n)
      for (k in 1:n)
        R[i,j]=R[i,j]+A[i,k]*B[k,j]
      return(R)
}
# time for nested loops implementation
system.time(matMult(A, B, n))
# user  system elapsed 
# 0.176   0.001   0.176 

# built in function
system.time(A%*%B) # built-in matrix multiplication
# user  system elapsed 
# 0.001   0.000   0.001



