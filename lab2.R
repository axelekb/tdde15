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

accuracy = function(pred, true) {
  count = 0
  for (i in 1:length(pred)){
    if(pred[i] == true[i]){
      count = count + 1
    }
  }
  return(count/length(true))
}

true_states = simulation$states
accuracy(filtering_guesses, true_states)
accuracy(smoothing_guesses, true_states)
accuracy(most_probable, true_states)

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
  comparison[i,1] = accuracy(filtering_guesses, true_states)
  comparison[i,2] = accuracy(smoothing_guesses, true_states)
  comparison[i,3] = accuracy(most_probable, true_states)
}
comparison
mean(comparison[,1])
mean(comparison[,2])
mean(comparison[,3])

#smoothing is more accurate since it takes into account all of the observations

# --- 6 ----------------------------------------------------------------------------------------------------------------------------------
library(entropy)
plot(apply(filtering, 2, entropy.empirical), type='l')

# --- 7 ----------------------------------------------------------------------------------------------------------------------------------
smoothing[,100] %*% transProbs

filtering[,100]

# varför är inte smoothing och filtering samma på sista? hur defineras beta på sista (x(t+1) existerar ej)?