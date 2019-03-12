---
title: "Cox-Rubinstein Binomial Option Pricing Model for Pricing Power Options"
output: 
  html_document:
    keep_md: true
---


```r
u <- exp(0.02)
d <- exp(-0.02)
qu <- ((1.01)-d)/(u-d)
qd <- (u-(1.01))/(u-d)

build_stock_tree = function(S,N,u,d) {
  tree = matrix(0, nrow=N+1, ncol=N+1)
  
  for (i in 1:(N+1)) {
    for (j in 1:i) {
      tree[i,j] = S * u^(j-1) * d^((i-1)-(j-1))
    }
  }
  return(tree)
}

value_binomial_option = function(tree, qu, qd, r, K) {
  option_tree = matrix(0, nrow=nrow(tree), ncol=ncol(tree))
  option_tree[nrow(option_tree),] = pmax(tree[nrow(tree),]**1.5 - K, 0)
  
  for (i in (nrow(tree)-1):1) {
    for(j in 1:i) {
      option_tree[i, j] = (((qd)*option_tree[i+1,j]) + (qu*option_tree[i+1,j+1]))/(1.01)
    }
  }
  return(option_tree)
}

delta_tree = function(stock_tree,option_tree) {
  stock_tree = stock_tree
  option_tree = option_tree
  for (i in 1:(nrow(stock_tree)-1)) {
    for(j in 1:i) {
      option_tree[i, j] =(option_tree[i+1, j+1] - option_tree[i+1, j])/(stock_tree[i+1, j+1] - stock_tree[i+1, j])
    }
  }
  return(option_tree)
}

stock_tree <- build_stock_tree(50, 8, u, d) #(stock price, time periods)
option_tree <- value_binomial_option(stock_tree, qu, qd, 0.01, 55**1.5) #(stock_tree, qu, qd, r, K)
delta_tree <- delta_tree(stock_tree, option_tree) #(stock_tree, option_tree)
```

Price of the power option


```r
option_tree[1,1]
```

```
## [1] 7.334402
```

Evolution of price v(t, St)

```r
option_tree
```

```
##           [,1]     [,2]      [,3]      [,4]      [,5]      [,6]     [,7]
##  [1,] 7.334402 0.000000  0.000000  0.000000  0.000000  0.000000  0.00000
##  [2,] 1.828088 9.317728  0.000000  0.000000  0.000000  0.000000  0.00000
##  [3,] 0.000000 2.478402 11.783981  0.000000  0.000000  0.000000  0.00000
##  [4,] 0.000000 0.000000  3.360055 14.825767  0.000000  0.000000  0.00000
##  [5,] 0.000000 0.000000  0.000000  4.555344 18.540460  0.000000  0.00000
##  [6,] 0.000000 0.000000  0.000000  0.000000  6.175838 23.021884  0.00000
##  [7,] 0.000000 0.000000  0.000000  0.000000  0.000000  8.372798 28.34546
##  [8,] 0.000000 0.000000  0.000000  0.000000  0.000000  0.000000 11.35129
##  [9,] 0.000000 0.000000  0.000000  0.000000  0.000000  0.000000  0.00000
##           [,8]     [,9]
##  [1,]  0.00000  0.00000
##  [2,]  0.00000  0.00000
##  [3,]  0.00000  0.00000
##  [4,]  0.00000  0.00000
##  [5,]  0.00000  0.00000
##  [6,]  0.00000  0.00000
##  [7,]  0.00000  0.00000
##  [8,] 34.54325  0.00000
##  [9,] 15.38934 41.56353
```

Evolution of the number of shares held in the replicating portfolio

```r
delta_tree
```

```
##          [,1]     [,2]     [,3]     [,4]     [,5]     [,6]      [,7]
##  [1,] 3.74457 0.000000 0.000000 0.000000 0.000000 0.000000  0.000000
##  [2,] 1.26415 4.560354 0.000000 0.000000 0.000000 0.000000  0.000000
##  [3,] 0.00000 1.679916 5.507700 0.000000 0.000000 0.000000  0.000000
##  [4,] 0.00000 0.000000 2.232422 6.584904 0.000000 0.000000  0.000000
##  [5,] 0.00000 0.000000 0.000000 2.966642 7.774912 0.000000  0.000000
##  [6,] 0.00000 0.000000 0.000000 0.000000 3.942339 9.035405  0.000000
##  [7,] 0.00000 0.000000 0.000000 0.000000 0.000000 5.238933 10.284024
##  [8,] 0.00000 0.000000 0.000000 0.000000 0.000000 0.000000  6.961962
##  [9,] 0.00000 0.000000 0.000000 0.000000 0.000000 0.000000  0.000000
##           [,8]     [,9]
##  [1,]  0.00000  0.00000
##  [2,]  0.00000  0.00000
##  [3,]  0.00000  0.00000
##  [4,]  0.00000  0.00000
##  [5,]  0.00000  0.00000
##  [6,]  0.00000  0.00000
##  [7,]  0.00000  0.00000
##  [8,] 11.37662  0.00000
##  [9,] 15.38934 41.56353
```
