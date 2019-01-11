trapezoid_method <- function(f, a, b, n = 1000)
{
    #integral of f from a to b with a partition of size n
    
    delta_x <- (b - a) / n
    x <- seq(a, b, by <- delta_x)
    y <- f(x)
    
    integral <- delta_x * (y[1]/2 + sum(y[2:n]) + y[n+1]/2)
    
    return(integral)
}

#examples

f <- function(x) 1 / x
trapezoid_method(f, 1, 10) #integral of 1/x from x = 1 to x = 10 (converges to 2.3)
plot(f, main = "First Example", ylab = "1 / x")

g <- function(x) 1 / sin(x)
trapezoid_method(g, 0, 1) #integral of 1/sin(x) from x = 0 to 1 (does not converge)
plot(g, main = "Second Example", ylab = "1 / sin(x)")

h <- function(x) x^5 * (1 - x)^6 * (462 * x^2 + 197) / (530 * (1 + x^2)) + (333/106) 
trapezoid_method(h, 0, 1) #pi approximation 
plot(h, main = "Pi Approximation")