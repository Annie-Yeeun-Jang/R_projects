#If and for statement

#1
x <- 75
if (x>=90) {print('A')
} else if(x>= 80) {print('B')
}else if(x>=70) { print('C') 
      }else {print('D')}

#2
input <- function(x) {
  if(x > 0) {print(x)}
  else { print(-x)}
}
input(-1)
input(1)

#3
input <- function(x) { 
  if( x > 0) { print(2*x)} 
  else {x <- 0 + print(x) } 
}
input(5)
input(0)
input(-1)

#4
input <- function(x) {
  if(x > 0) {print(2*x)} else
  if( x == 0) {print(x)} else
   {print(-2*x)}
}
input(3)
input(0)
input(-3)

#5
f <- function(x) {
   for (i in 1:x) {print(i)}
   }
f(5)

#6
f2 <- function(x) {
 i <- 0
 for (j in 1:x) {i <- i+j}
 print(i)
 }
f2(10)
f2(100)

#7
f3 <- function(a,b) { 
  if( (a>1) & (b>1)) { c <- a*b 
  print(c) }
  else { 
  c <- a+b 
  print(c) 
  }
  }

f3(2,3)
f3(1,-1)

#Q 2-1

c2f <- function(x) { 
  f<- (x*1.8)+32 
  print(f)
}

c2f(27)


#Q 2-2
f2c <- function(x) { 
  c<- (x-32 ) /1.8
  print(c)
}

f2c(80.6)





