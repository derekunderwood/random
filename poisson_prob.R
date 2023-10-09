known_rate = 2
x <- rpois(100000, known_rate)

k = 3

print(paste0("P(k > ", k, "): ", length(which(x > 3))/length(x)))

# estimate different thresholds simulatenously using ppois
1 - ppois(q = c(2, 3, 4, 5) - 1, known_rate)

# compare rpois vs ppois
length(which(x > 3))/length(x) - (1 - ppois(q = 3, known_rate))
