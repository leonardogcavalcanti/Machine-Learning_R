###### Usando multiclass no caret --------------------
# Data frame de avaliação (treino)
treino_pred <- predict(modelo_naiveBayes, training_balance, type = "prob")
treino_pred$predito <- predict(modelo_naiveBayes, training_balance)
treino_pred$real <- training_balance$EVOLUCAO


str(treino_pred)


multiClassSummary(treino_pred, lev = levels(treino_pred$real))

test_pred <- predict(modelo_naiveBayes, testing, type = "prob")
test_pred$obs <- testing$EVOLUCAO
test_pred$pred <- predict(modelo_naiveBayes, testing)

result <- multiClassSummary(test_pred, lev = levels(test_pred$obs))
result

#####################################################
###### Naive Bayes ##################################
#####################################################

# Controle do modelo 
# Repeated K-Fold Cross Validation
fitControl <- trainControl(method = "repeatedcv",
                           # número de grupos criados na reamostragem K-Fold Cross Validation
                           number = 10,
                           # número de repetições do K-Fold Cross Validation;
                           repeats = 5,
                           savePredictions = TRUE,
                           classProbs = TRUE,
                           verboseIter = TRUE)

# Criando modelo ------------------------------

# Tempo inicial
tempo_inicial <- proc.time()
# Treino do modelo
modelo_naiveBayes <- train(EVOLUCAO ~ ., 
                           data = training_balance, 
                           method = "naive_bayes",
                           metric = "ROC",
                           summaryFunction = multiClassSummary,
                           trControl = fitControl)


# Tempo total
proc.time() - tempo_inicial
###### Avaliando o modelo ###########################

# Base de treino
prob_predicao_nb_trainingBalance <- predict(modelo_naiveBayes, 
                                            newdata =  training_balance, 
                                            type = 'prob')
prob_predicao_nb_trainingBalance

predicao_nb_trainingBalance <- predict(modelo_naiveBayes, 
                                       newdata = training_balance)
predicao_nb_trainingBalance

# Matriz de confusão de treino ------------------
confusionMatrix(data = predicao_nb_trainingBalance, training_balance$EVOLUCAO)

# Criando matriz de confusão gráfica
matriz_treino <- data.frame(confusionMatrix(data = predicao_nb_trainingBalance, training_balance$EVOLUCAO)$table)

matriz_treino <- rename(matriz_treino,
                        Predições = Prediction, 
                        Valores_Reais = Reference, 
                        Frequência = Freq)
names(matriz_treino)

plotTabela <- matriz_treino %>%
  mutate(acerto_erro = ifelse(matriz_treino$Predições == matriz_treino$Valores_Reais, "Acerto", "Erro")) %>%
  group_by(Valores_Reais) %>%
  mutate(prop = Frequência/sum(Frequência))

ggplot(data = plotTabela, mapping = aes(x = Valores_Reais, y = Predições, fill = acerto_erro, alpha = Frequência)) +
  geom_tile() +
  geom_text(aes(label = Frequência), vjust = .5, fontface  = "bold", alpha = 1) +
  scale_fill_manual(values = c(Acerto = "green", Erro = "red")) +
  theme_bw() +
  xlim(rev(levels(matriz_treino$Valores_Reais)))

# Visualização Gráfica 

tabela <- data.frame(confusionMatrix(data = predicao_naiveBayes_confusion, testing$EVOLUCAO)$table)
tabela

str(tabela)

tabela <- rename(tabela,
                 Predições = Prediction, 
                 "Valores_Reais" = Reference, 
                 Frequência = Freq)

plotTabela <- tabela %>%
  mutate(acerto_erro = ifelse(tabela$Predições == tabela$Valores_Reais, "Acerto", "Erro")) %>%
  group_by(Valores_Reais) %>%
  mutate(prop = Frequência/sum(Frequência))

ggplot(data = plotTabela, mapping = aes(x = Valores_Reais, y = Predições, fill = acerto_erro, alpha = Frequência)) +
  geom_tile() +
  geom_text(aes(label = Frequência), vjust = .5, fontface  = "bold", alpha = 1) +
  scale_fill_manual(values = c(Acerto = "green", Erro = "red")) +
  theme_bw() +
  xlim(rev(levels(tabela$Valores_Reais)))


###### Curva ROC --------------------------------- 
library(pROC)

result <- pROC::multiclass.roc(as.numeric(predicao_naiveBayes_confusion),
                               as.numeric(testing$EVOLUCAO),
                               percent = TRUE,
                               levels = c(1,2,3),
                               direction = ">")

plot.roc(result$rocs[[1]], 
         print.auc=T,
         legacy.axes = T)
plot.roc(result$rocs[[2]],
         add=T, col = 'red',
         print.auc = T,
         legacy.axes = T,
         print.auc.adj = c(0,3))
plot.roc(result$rocs[[3]],add=T, col = 'blue',
         print.auc=T,
         legacy.axes = T,
         print.auc.adj = c(0,5))

legend('bottomright',
       legend = c('cura',
                  'obito',
                  'obito_outras_causas'),
       col=c('black','red','blue'),lwd=2)