source("/Users/gregorymatthews/Dropbox/Pinochle_git/meld_combos.R")


pinochle_deck <- c("9H", "9S", "9D", "9C", "9H", "9S", "9D", "9C", "10H", "10S", "10D", "10C", "10H", "10S", "10D", "10C", "JH", "JS", "JD", "JC", "JH", "JS", "JD", "JC", "QH", "QS", "QD", "QC", "QH", "QS", "QD", "QC","KH", "KS", "KD", "KC", "KH", "KS", "KD", "KC", "AH", "AS", "AD", "AC", "AH", "AS", "AD", "AC")

# pinochle_deck <- matrix(entries, nrow = 12, ncol = 4, dimnames = list(c("nine", "nine", "ten","ten", "jack", "jack", "queen", "queen", "king", "king", "ace", "ace"), c("heart", "spade", "diamond", "club")), byrow = TRUE)


#3 handed deal.  
deal <- function(deck = pinochle_deck) {

#shuffling the deck   
temp <- sample(pinochle_deck, size = 48, replace = FALSE)

player1 <- temp[1:15]
player2 <- temp[16:30]
player3 <- temp[31:45]
kitty <- temp[46:48]
  
hand <- list(player1, player2, player3, kitty)
  return(hand)
}




meld_score <- list()
for (i in 1:10000){print(i)
pin <- deal()
meld_score[[i]] <- meld(pin)
}

meld_score_mat <- do.call(rbind, meld_score)
cor(meld_score_mat)

plot(meld_score_mat[,1],meld_score_mat[,3])

meld_score_mat <- as.data.frame(meld_score_mat)

library(ggplot2)
ggplot(aes(x = V1, y = V2, color = factor(V3)) , data = meld_score_mat) + geom_jitter()








