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
bn = bn.fit(hc(test), test)

cond_probs = c()
for (i in 1:dim(test)[1]) {
  cond_probs[i] = cpquery(bn, (S=="yes"), ((A==test[i,1]) & (T==test[i,3]) & (L==test[i,4]) & (B==test[i,5]) & (E==test[i,6]) & (X==test[i,7]) & (D==test[i,8])))
}
pred = ifelse(cond_probs>0.5, "yes", "no")
table(pred, test[,"S"])
