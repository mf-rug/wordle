library(wordle)
library(tidyverse)
w <- cbind(as.data.frame(wordle_dict), 
          as.data.frame(matrix(unlist(str_split(wordle_dict[1:length(wordle_dict)], '')), ncol = 5, byrow = TRUE)))
f <- cbind(as.data.frame(letters), 
           as.data.frame(apply(w[,2:6], 2, function(x) table(x) / nrow(w) *100))) 

for (i in 1:nrow(w)) {
  for (o in 1:5) {
    w[i, o+1+5] <- f[which(f$letter == w[i, o+1]), o+1]
  }
}
w$unique <- apply(w[, 2:6], 1, function(x) length(unique(x)))
w$sumf <- apply(w[, 7:11], 1, sum)
w$score <- w$unique * w$sumf
w$rank <- w$rank <- nrow(w) - rank(w$score) + 1

hit <- cbind(str_split(w[w$rank == 1, "wordle_dict"], '')[[1]], diag(5)) %>% as.data.frame()
colnames(hit) <- colnames(f)

ggplot(pivot_longer(f, cols = 2:6), aes(x=name, y=factor(letters, levels=rev(tolower(LETTERS))))) +
  geom_tile(aes(fill = value)) +
  scale_fill_gradient(low='white', high='black') +
  geom_tile(data = pivot_longer(hit, cols=2:6), aes(color=value), fill = 'transparent', size=1, show.legend = F) + 
  scale_color_manual(values = c('0' = "transparent", '1' = "red")) +
  labs(fill = 'freq. at position') +
  xlab('position') + 
  ylab('letter')
