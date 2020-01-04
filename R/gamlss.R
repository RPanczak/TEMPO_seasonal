p_load(gamlss)

f1 <- fitDist(Airbnb_visitor_nights, k = 2, 
        # type = c("realAll", "realline", "realplus", "real0to1", "counts", "binom"), 
        type = "counts", 
        try.gamlss = TRUE, 
        data = data, 
        trace = TRUE)

f1$fits
f1$failed

m1 <- gamlss(y~pb(x), sigma.fo=~pb(x), family=NO, data=abdom)

f2 <- chooseDist(m1, 
                 k = c(2, 3.84, round(log(length(object$y)), 2)), 
           # type = c("realAll", "realline", "realplus", "real0to1", "counts", "binom"), 
           type = "counts", 
           trace = TRUE, 
           parallel = "snow", ncpus = 4)
