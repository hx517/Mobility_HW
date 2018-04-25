f <- function(x) (2.9*sqrt(x)-x^2)

n1 <- 100
n2 <- 1000
n3 <- 10000
a = 0
b1 = 1
b2 = 2

#question a.
#True integral value when a=0, b=1
integrate(f,a,b1)
#True integral value when a=0, b=2
integrate(f,a,b2)

#question b.
#when a=0, b=1
#(1)n=100
x <- runif(n1,min=a,max=b1)
I = sum(f(x))/n1*(b1-a)
I
#(2)n=1000
x <- runif(n2,min=a,max=b1)
I = sum(f(x))/n2*(b1-a)
I
#(2)n=10000
x <- runif(n3,min=a,max=b1)
I = sum(f(x))/n3*(b1-a)
I

#when a=0, b=2
#(1)n=100
x <- runif(n1,min=a,max=b2)
I = sum(f(x))/n1*(b2-a)
I
#(2)n=1000
x <- runif(n2,min=a,max=b2)
I = sum(f(x))/n2*(b2-a)
I
#(2)n=10000
x <- runif(n3,min=a,max=b2)
I = sum(f(x))/n3*(b2-a)
I
