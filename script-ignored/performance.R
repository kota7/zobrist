### compare named
### list, hash, and zht


library(hash)
library(microbenchmark)
library(zobrist)
library(combiter)
library(ggplot2)



d1 <- NULL
d2 <- NULL

for (s in 0:5)
{
  keysize <- 5000
  n <- 10^s
  l <- list()
  h <- hash()
  z <- zht(keysize, hashsize = max(1L, ceiling(log2(n))))

  # add values
  iter <- isubset(keysize)
  for (i in 1:n)
  {
    cat(sprintf("\rcreating objects...%6d/%6d", i, n))
    i <- nextElem(iter)
    name <- zobrist:::KeyToStr(i, keysize)
    l[[name]] <- i
    h[name] <- i
    z[i] <- i
  }

  cat("\n\n\nsize:", n, "\n")

  ## object size
  tmp <- data.frame(size = n,
                    object = c("list", "hash", "zht"),
                    value = c(object.size(l), object.size(h), object.size(z)),
                    stringsAsFactors = FALSE)
  print(tmp)
  d1 <- rbind(d1, tmp)

  ## get speed
  m <- microbenchmark(l[[name]], h[[name]], z[i])
  tmp <- as.data.frame(m)
  tmp$size <- n
  d2 <- rbind(d2, tmp)

  print(m)
}


ggplot(d2, aes(factor(size), time, color = expr)) +
  scale_y_log10() +
  geom_boxplot()
ggsave("script-ignored/speed-comparison.png", width = 10, height = 5)
