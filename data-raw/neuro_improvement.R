set.seed(35421)

neuro_improvement <- data.frame(treat = rep(1:2,each=1000))

mrs_distr <- prop.table(
                rbind(c(271,366,229,252,329,157,216),
                      c(381,389,209,210,240,157,216)),
                margin = 1
             )

neuro_distr <- expand.grid(treat = 1:2, mRS = 0:6)

        # treat        1   2   1   2   1   2   1   2   1   2   1   2   1   2
        #   mRS        0   0   1   1   2   2   3   3   4   4   5   5   6   6
neuro_distr$worse =  c(05, 05, 06, 06, 07, 07, 07, 07, 07, 07, 11, 11, 12, 12)
neuro_distr$stable = c(10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10)
neuro_distr$better = c(36, 38, 30, 35, 29, 31, 30, 31, 28, 29, 06, 06, 05, 05)
neuro_distr[,-(1:2)] <- prop.table(as.matrix(neuro_distr[,-(1:2)]),margin=1)

neuro_improvement$mRS <- sapply(neuro_improvement$treat, function(i){
        sample(0:6,size = 1,replace = T,prob=mrs_distr[i,])
})

neuro_improvement$early_neuro_improvement <- apply(neuro_improvement,1,function(x){
        probs <- neuro_distr[neuro_distr[,"treat"]==x["treat"] &
                    neuro_distr[,"mRS"]==x["mRS"],
                    c("worse","stable","better")
                    ]

        sample(-1:1,1,replace=TRUE,prob=probs)

})

neuro_improvement$mRS <- factor(neuro_improvement$mRS,levels=6:0)
neuro_improvement$early_neuro_improvement <- factor(neuro_improvement$early_neuro_improvement,levels=-1:1, labels=c("worse","stable","improve"))

usethis::use_data(neuro_improvement, overwrite = TRUE)


