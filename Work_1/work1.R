

# 2
log_gamma_loop = function(n){
  factorial = 1
  logresult = log(factorial)
  if (n==1){
    logresult = log(n) # (n-1 = 0) and 0! = 1
  } else {
    for (i in (1:(n-1))){
      logresult = logresult + log(i)
    }
  }
  return(logresult)
}

# 3
log_gamma_recursive = function(n) {
  factorial = 1
  logresult = log(factorial)
  if (n==1){
    logresult = log(n) # (n-1 = 0) and 0! = 1
  } else {
    # the first log is log (n-1) as we have to reduce by 1 and take the log upto (n-1)
    logresult = log(n-1) + log_gamma_recursive(n-1)
    return(logresult)
  }
}

# n = 100
# lgamma(n)
# log_gamma_loop(n)
# log_gamma_recursive(n)

# 4a
sum_log_gamma_loop = function(n){
  sum = 0
  for (i in 1:n){
    sum = sum + log_gamma_loop(i)
  }
  return (sum)
}

# sum_log_gamma_loop(100)

# 4b
sum_log_gamma_recursive = function(n) {
  sum = 0
  for (i in 1:n) {
    sum = sum + log_gamma_recursive(i)
  }
  return (sum)
}

# 5
sum_lgamma = function(n){
  sum = 0
  for (i in 1:n){
    sum = sum + lgamma(i)
  }
  return (sum)
}

n = 100
sum_log_gamma_loop(n)
sum_log_gamma_recursive(n)
sum_lgamma(n)

# > sum_log_gamma_loop(n)
# [1] 15617.2
# > sum_log_gamma_recursive(n)
# [1] 15617.2
# > sum_lgamma(n)
# [1] 15617.2

# .Options
options(expressions=500000)

max.value = 3450
tot.division = 100

# create data frame for time
create_data_frame = function(max_value, total_iteration_div){
  iteration.point = as.integer(seq(35, max_value, length.out = total_iteration_div))
  vec.loop = rep(1, times = total_iteration_div)
  vec.rec = rep(1, times = total_iteration_div)
  vec.lgamma = rep(1, times = total_iteration_div)
  for (j in 1:total_iteration_div) {
    vec.loop[j] = (system.time(sum_log_gamma_loop(iteration.point[j]))[1])
    vec.rec[j] = (system.time(sum_log_gamma_recursive(iteration.point[j]))[1])
    vec.lgamma[j] = (system.time(sum_lgamma(iteration.point[j]))[1])

    x = cbind(iteration.point, vec.loop, vec.rec, vec.lgamma)
    df = as.data.frame(x)
  }
  return (df)
}

# vec.loop
# vec.rec
# vec.lgamma

# x
# class(x)

df = create_data_frame(max.value, tot.division)
library(ggplot2)

ggplot(df, aes(iteration.point)) +
  geom_line(aes(y = vec.loop, color = "loop")) +
  geom_line(aes(y = vec.rec, color = "recursion")) +
  geom_line(aes(y = vec.lgamma, color = "lgamma")) +
  labs(title = "Time vs. value of n") +
  xlab("Value of n") +
  ylab("Execution time [sec]")

# loop = O(n), recursion = O(n^2), lgamma = O(1)

# write.csv(file = "run time.csv", x = df)
