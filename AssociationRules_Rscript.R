df =  read.csv("userLog_201801_201802_for_participants.csv", sep=";",stringsAsFactors = FALSE)[ ,c('userCode', 'project_id')]
test = read.csv("test_data2.csv")
sample = df[sample(nrow(df), 10), ]

library(arules)
user_item_matrix = as(split(df[,"project_id"], df[,"userCode"]), "transactions")
user_item_matrix

test_matrix = as(split(test[,"project_id"], test[,"userCode"]), "transactions")
test_matrix

rules = apriori(user_item_matrix,parameter = list(supp = 0.0001, conf = 0.005,minlen=2,maxlen=2))
summary(rules)

rules <- apriori(data=user_item_matrix, parameter=list (supp=0.00004,conf = 0.005,minlen=2), appearance = list(default="rhs",lhs=LIST(test_matrix[1])), control = list (verbose=F))
rules_conf <- sort (rules, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
inspect(head(rules_conf))

rulesMatchLHS <- is.subset(rules@lhs,test_matrix[1])
suitableRules <-  rulesMatchLHS & !(is.subset(rules@rhs,test_matrix[1]))
inspect(rules[as.logical(suitableRules)])

i <- 1
while (i <= nrow(rules[as.logical(suitableRules)]@rhs)) {
recommendations <- strsplit(LIST(rules[as.logical(suitableRules)]@rhs)[[i]],split=" ")
recommendations <- lapply(recommendations,function(x){paste(x,collapse=" ")})
recommendations <- as.character(recommendations)
recommendations <- recommendations[!sapply(recommendations,function(x){test_matrix[1] %in% x})]
print(reccomendatation)
i = i+1
}

write(rules, file = "apriori.csv", sep = ",")
