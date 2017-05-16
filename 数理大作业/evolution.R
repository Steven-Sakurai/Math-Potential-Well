library(ggplot2)
tau = seq(0, 10, 0.01)
s = 0.5 #sigma
x0 = 10 #x'
r = 0.1
var = s^2 * tau
m = x0 - tau * (r - 0.5 * s^2)
x = c(x0)
for(i in 1:1000)
{
    x = append(x, rnorm(1, m[i+1], var[i+1]))
}
plot(tau, x, type='l')
x = c(x0)
for(i in 1:1000)
{
    x = append(x, rnorm(1, m[i+1], var[i+1]))
}
plot(tau, x, type='l')
x = c(x0)
for(i in 1:1000)
{
    x = append(x, rnorm(1, m[i+1], var[i+1]))
}
plot(tau, x, type='l')

#lines(tau, x)