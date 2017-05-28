c_time_evolution <- function(sigma, x0, r, K)
{
    tau = seq(0, 1, 0.01)[-1]
    w = rnorm(100, 0, 0.01)
    w = cumsum(w)
    # calculate s(t)
    x = (r - 0.5*sigma^2) * tau + sigma * w
    x = exp(x)
    x = x0 * x
    #fi
    plot(tau, x, type='l')
    
    d1 = log(x/K) + (1 - tau) * (r + 0.5*sigma^2)
    d1 = d1/(sqrt(1 - tau) * sigma)
    d2 = d1 - sigma*sqrt(1 - tau)
    c = x * pnorm(d1) - exp(-r * (1-tau)) * K * pnorm(d2)
    plot(tau, c, 'l')
}
c_time_evolution(sigma = 0.5, x0 = 100, r = 0.20, K = 100)