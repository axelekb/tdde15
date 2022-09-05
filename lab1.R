# 1)
library(bnlearn)
data("asia")
bn1 = hc(asia)
init = empty.graph(c("A","S","T","L","B","E","X","D"), 1)
arcs(init) = matrix(c("A","S","T","L","B","E","X","D"), ncol = 2, byrow = TRUE, dimnames = list(NULL, c("from", "to")))
bn2 = hc(asia, start=init)
plot(bn1)
plot(bn2)
all.equal(cpdag(bn1),cpdag(bn2))

# 2)
set.seed(12345)
n = nrow(asia)
id = sample(1:n, floor(0.8*n))
train = asia[id,]
test = asia[-id,]

library(gRain)
bn = bn.fit(hc(train), train)
true_bn = bn.fit(model2network("[A][S][T|A][L|S][B|S][D|B:E][E|T:L][X|E]"), train)

cond_probs = matrix(ncol=2, nrow=dim(test)[1])
colnames(cond_probs)=c("learnt", "true")
for (i in 1:dim(test)[1]) {
  cond_probs[i,1] = cpquery(bn, (S=="yes"), ((A==test[i,1]) & (T==test[i,3]) & (L==test[i,4]) & (B==test[i,5]) & (E==test[i,6]) & (X==test[i,7]) & (D==test[i,8])))
  cond_probs[i,2] = cpquery(true_bn, (S=="yes"), ((A==test[i,1]) & (T==test[i,3]) & (L==test[i,4]) & (B==test[i,5]) & (E==test[i,6]) & (X==test[i,7]) & (D==test[i,8])))
}

pred_learnt = ifelse(cond_probs[,1]>0.5, "yes", "no")
cm_learnt = table(pred_learnt, test[,"S"])
cm_learnt
sum(diag(cm_learnt))/sum(cm_learnt)

pred_true = ifelse(cond_probs[,2]>0.5, "yes", "no")
cm_true = table(pred_true, test[,"S"])
cm_true
sum(diag(cm_true))/sum(cm_true)

# 3)
graphviz.plot(bn)
mb(bn, "E")
cond_probs = c()
for (i in 1:dim(test)[1]) {
  cond_probs[i] = cpquery(bn, (S=="yes"), ((L==test[i,4]) & (B==test[i,5])))
  
}
pred = ifelse(cond_probs>0.5, "yes", "no")
cm = table(pred_true, test[,"S"])
cm
sum(diag(cm))/sum(cm)

# 4)
