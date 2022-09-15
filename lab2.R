# lab2
# --- 1 ----------------------------------------------------------------------------------------------------------------------------------
library(HMM)
sectors = c(1:10)
transProbs = matrix(0,nrow=10,ncol=10)
for (r in 1:10) {
  transProbs[r,r] = 0.5
  if (r==10) {
    transProbs[r, 1] = 0.5
  }else{
    transProbs[r,r+1] = 0.5
  }
}
emissionProbs = matrix(0,nrow=10,ncol=10)
for(r in 1:10) {
  for (c in 1:10) {
    if((c+7-r) %% 10 >= 5) {
      emissionProbs[r,c] = 0.2
    } else {
      emissionProbs[r,c] = 0
    }
  }
}
hmm = initHMM(sectors,sectors,transProbs = transProbs,emissionProbs = emissionProbs)

# --- 2 ----------------------------------------------------------------------------------------------------------------------------------
simulation = simHMM(hmm,100)

# --- 3 ----------------------------------------------------------------------------------------------------------------------------------
observed = simulation$observation
forwardProbs =exp(forward(hmm, observed))
filtering = forwardProbs / colSums(forwardProbs)
filtering = prop.table(filtering, margin=2)

backwardProbs = exp(backward(hmm, observed))
smoothing = forwardProbs*backwardProbs/ colSums(forwardProbs*backwardProbs)
smoothing = prop.table(smoothing, margin=2)

most_probable = viterbi(hmm, observed)

# --- 4 ----------------------------------------------------------------------------------------------------------------------------------
filtering_guesses = apply(filtering, 2, which.max)
smoothing_guesses = apply(smoothing, 2, which.max)

true_states = simulation$states
cm_1 = table(true_states, filtering_guesses)
sum(diag(cm_1))/sum(cm_1)
cm_2 = table(true_states, smoothing_guesses)
sum(diag(cm_2))/sum(cm_2)
cm_3 = table(true_states, most_probable)
sum(diag(cm_3))/sum(cm_3)

# --- 5 ----------------------------------------------------------------------------------------------------------------------------------
comparison = matrix(0, nrow=10, ncol=3)
colnames(comparison) = c("Filtering","Smoothing","Most probable")
for(i in 1:10){
  simulation = simHMM(hmm,100)
  observed = simulation$observation
  forwardProbs =exp(forward(hmm, observed))
  filtering = forwardProbs / colSums(forwardProbs)
  filtering = prop.table(filtering, margin=2)
  
  backwardProbs = exp(backward(hmm, observed))
  smoothing = forwardProbs*backwardProbs/ colSums(forwardProbs*backwardProbs)
  smoothing = prop.table(smoothing, margin=2)
  
  most_probable = viterbi(hmm, observed)
  
  filtering_guesses = apply(filtering, 2, which.max)
  smoothing_guesses = apply(smoothing, 2, which.max)
  
  true_states = simulation$states
  cm_1 = table(true_states, filtering_guesses)
  comparison[i,1] = sum(diag(cm_1))/sum(cm_1)
  cm_2 = table(true_states, smoothing_guesses)
  comparison[i,2] = sum(diag(cm_2))/sum(cm_2)
  cm_3 = table(true_states, most_probable)
  comparison[i,3] = sum(diag(cm_3))/sum(cm_3)
}
comparison
mean(comparison[,1])
mean(comparison[,2])
mean(comparison[,3])

#smoothing is more accurate since it takes into account all of the observations

# --- 6 ----------------------------------------------------------------------------------------------------------------------------------
library(entropy)
plot(apply(filtering, 2, entropy.empirical), type='l')
# no clear trend in entropy given more observations

# --- 7 ----------------------------------------------------------------------------------------------------------------------------------
simulation$observation
smoothing
