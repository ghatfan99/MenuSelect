reponse =c(rep("yes",5), rep("no", 5), rep("yes", 5), rep("na", 3), rep("yes", 2))
ville =c(rep("Paris",10), rep("Londre", 5), rep("Rome", 5))
score = c(rep(5,7), rep(7, 3), rep(10, 5), rep(20, 5))
data = data.frame(ville, reponse, score)

Choices = colnames(data)

