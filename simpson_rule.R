simpson_method <- function(f, a, b, n = 1000)
{
    #integral of f from a to b with a partition of size n
    
    delta_x <- (b - a) / n
    
    x <- seq(a, b, by <- delta_x)
   
    if (n == 2) #f(x) as quadratic polynomial, midpoint (a + b) / 2
    {
        integral <- f(x[1]) + 4 * f(x[2]) + f(x[3]) 
    }
    else
    {
        integral <- f(x[1]) + f(x[n+1]) + 
                    2 * sum(f(x[seq(2, n, by=2)])) + 
                    4 * sum(f(x[seq(3, n-1, by=2)]))
    }
    
    integral <- (integral * delta_x) / 3
    
    return(integral)
}

#examples

f <- function(x) 1 / x
simpson_method(f, 1, 10) 
plot(f, main = "First Example", ylab = "1 / x")

g <- function(x) exp(x) + 1
simpson_method(g, 0, 1) #result is e
plot(g, main = "e", ylab = "e")
