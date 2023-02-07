# Dataset full --------------------------------------

# chamando dataset salvo
dataset_full <- 
  read.csv("dataset_tcc.csv", 
           sep = ",", 
           na.strings = "", 
           stringsAsFactors = T)

###### Construindo modelos ---------------------------


# Divisão dos dados para treino e teste --------------

# prop = proporção de 75% para treino e 25% para teste
# strata = dividir a amostra da classe de forma estratificada para minimizar o desbalanceamento
split_dataset <- initial_split(dataset_full, prop = 0.75, strata = EVOLUCAO)
split_dataset

train_dataset <- training(split_dataset)
dim(train_dataset)

test_dataset  <-  testing(split_dataset)
dim(test_dataset)


# Verificando o balanceamento da class
table(train_dataset$EVOLUCAO)
# Classe desbalanceada
plot(train_dataset$EVOLUCAO)

# Montando gráfico do desbalanceamento com estética melhor

counts_evolucao <- table(train_dataset$EVOLUCAO)
str(counts_evolucao)
counts_evolucao <- as.data.frame(counts_evolucao)

# Gráfico para o tcc
counts_evolucao %>% 
  ggplot() +
  geom_col(aes(x = Var1, y = Freq, fill = Var1), show.legend = FALSE) +
  labs(title = "Classe Desbalanceada", x = "Evolução", y = "Frequência") +
  geom_label(aes(x = Var1, y = Freq, label = Freq))

  
# Balanceando os dados preditivos usando step_upsample 
# Referência: https://themis.tidymodels.org/ 
# Traz o número de amostras de todas as classes minoritárias igual a 100% da classe majoritária
# recipe = receita dos passos que solicitei 
# Recipe (Como será dado o processamento dos dados)
train_balance <- recipe(EVOLUCAO ~., data = train_dataset) %>% 
  themis::step_upsample(EVOLUCAO) %>% 
  # prep (especifica as estimativas necessárias)
  prep() %>% 
  # juice(aplica todas as especificações)
  juice()

# Montando gráfico com o balanceamento com estética melhor

plot(train_balance$EVOLUCAO)
counts_balance <- table(train_balance$EVOLUCAO)
str(counts_balance)
counts_balance <- as.data.frame(counts_balance)

# Gráfico para o tcc
counts_balance %>% 
  ggplot() +
  geom_col(aes(x = Var1, y = Freq, fill = Var1), show.legend = FALSE) +
  labs(title = "Classe Balanceada", x = "Evolução", y = "Frequência") +
  geom_label(aes(x = Var1, y = Freq, label = Freq))


# Receita para modelagem ------------------------------
rec_recipe <-
  recipe(EVOLUCAO ~., 
         data = train_balance)

rec_recipe


# Reamostragem -------------------------

# reamostragrem mais leve do que a validação cruzada por repetição
trapping_resample <- bootstraps(train_balance, 
                             times = 5, 
                             strata = EVOLUCAO)

###################### Modelos ##################################


mdl_nb <-
  naive_Bayes() %>% 
  set_engine("klaR") %>% 
  set_mode("classification") %>% 
  set_args(smoothness = tune(),
           Laplace = tune())

?naive_Bayes

mdl_logistic <-
  multinom_reg() %>%
  set_engine("nnet") %>% 
  set_mode("classification") %>% 
  set_args(penalty = tune(),
           mixture = 1)
?multinom_reg

# Modelo pesado para minha máquina
#mdl_svm <- 
  #svm_linear() %>% 
  #set_engine("kernlab") %>% 
  #set_mode("classification") %>% 
  #set_args(cost = tune(),
           #margin = tune())

# Modelo pesado pra minha máquina (9h 42min 27,55s)
#mdl_xgb <-
  #boost_tree() %>% 
  #set_engine("xgboost") %>% 
  #set_mode("classification") %>% 
  #set_args(trees = 300,
           #tree_depth = tune(),
           #finalize(mtry(), train_balance),
           #min_n = tune(),
           #loss_reduction = tune(),
           #sample_size = tune(),
           #learn_rate = tune())

#args(boost_tree) ou
#?boost_tree

mdl_arv_decisao <-
  decision_tree() %>% 
  set_engine("rpart") %>% 
  set_mode("classification") %>% 
  set_args(cost_complexity = tune(),
           min_n = tune(),
           tree_depth = tune())

args(decision_tree)

# Modelo pesado pra minha máquina (4h 3min 58,85s)
mdl_rforest <-
  rand_forest() %>% 
  set_engine("ranger") %>% 
  set_mode("classification") %>% 
  set_args(
           trees = 300,
           min_n = tune())

#?rand_forest
  
# Rede neural demora 2h 40min em média
mdl_rneural <-
  mlp() %>% 
  set_engine("nnet") %>% 
  set_mode("classification") %>% 
  set_args(penalty = tune(),
           hidden_units = tune(),
           epochs = tune())

#args(mlp)

################ Workflow Set ##########################################

wflow_full <-
  workflow_set(
    preproc = list(receita = rec_recipe),
    models = list(rforest = mdl_rforest,
                  rlogistic = mdl_logistic,
                  nb = mdl_nb,
                  #rneural = mdl_rneural,
                  #xgboost = mdl_xgb,
                  adecisao = mdl_arv_decisao))
                  #svm = mdl_svm))
wflow_full

# Tirando o nome receita que ficou nos modelos 
wflow_full <- wflow_full %>% 
  mutate(wflow_id = gsub("(receita_)", "", wflow_id))

wflow_full

# Controlando a grid --------------------------------
grid_control <-
  control_grid(
    save_pred = TRUE,
    #finalize(mtry(), train_balance),
    parallel_over = "everything",
    save_workflow = TRUE)

# TREINANDO ----------------------------------------
# Rodando os modelos de treino com a reamostragem de acordo com os ajustes
-----------------------------
# iniciar o cronômetro
tempo_inicial <- Sys.time()
grid_result <-
  wflow_full %>% 
  workflow_map(
    resamples = trapping_resample,
    grid = 5,
    # roc_auc = curva roc; accuracy = acurácia; precision = precisão; recall = sensibilidade; especificity = especificadade
    metrics = metric_set(roc_auc, accuracy, precision, recall, specificity),
    control = grid_control,
    verbose = TRUE)
# Tempo total
Sys.time() - tempo_inicial
-----------------------------


# Mostra o resultado do ranking de acordo com diferentes métricas
grid_result %>% 
  autoplot()
  
# Mostra o ranking baseano na curva roc
autoplot(grid_result,
         rank_metric = "roc_auc",
         metric = "roc_auc",
         select_best = TRUE)

# Mostra os intervalos de confiança para cada modelo na ordem do melhor para o pior
library(ggrepel)
autoplot(grid_result, metric = "roc_auc") +
  geom_text_repel(aes(label = wflow_id), nudge_x = 1/8, nudge_y = 1/100) +
  theme(legend.position = "none")


# Escolhendo o modelo campeão do Ranking (Random Forest)

best_results <- 
  grid_result %>% 
  extract_workflow_set_result("rforest") %>% 
  select_best(metric = "roc_auc")

best_results

############### Testando com o melhor modelo ajustado ########################
----------------------------------------
# iniciar o cronômetro
tempo_inicial <- Sys.time()
random_forest_test_results <- 
  grid_result %>% 
  extract_workflow("rforest") %>% 
  finalize_workflow(best_results) %>% 
  last_fit(split = split_dataset)
# Tempo total
Sys.time() - tempo_inicial
-----------------------------


random_forest_test_results %>% 
  collect_metrics()


######################### Matriz de confusão ######################
# Análise de resultado reamostragem de teste

# Matriz de confusão
random_forest_test_results %>% 
  unnest(.predictions) %>% 
  conf_mat(truth = EVOLUCAO, estimate = .pred_class)

# Gráfica
matriz_confusao_teste <- random_forest_test_results %>% 
  unnest(.predictions) %>%
  conf_mat(EVOLUCAO, .pred_class) %>% 
  autoplot(random_forest_test_results, type = "heatmap", colour="Positive") 
  
matriz_confusao_teste



########################## ROC #########################

X <- subset(test_dataset, select = -c(EVOLUCAO))


mdl_rforest <-
  rand_forest() %>% 
  set_engine("ranger") %>% 
  set_mode("classification") %>% 
  fit(EVOLUCAO ~ ., data = test_dataset)
 

y_scores <- mdl_rforest %>%
  predict(X, type = 'prob')


y_onehot <- dummy_cols(test_dataset$EVOLUCAO)
colnames(y_onehot) <- c('drop','cura', 'obito', 'obito_outras_causas')
y_onehot <- subset(y_onehot, select = -c(drop))

str(y_onehot)
z = cbind(y_scores, y_onehot)
str(z)

z$cura <- as.factor(z$cura)
roc_cura <- roc_curve(data = z, cura, .pred_cura)
roc_cura$specificity <- 1 - roc_cura$specificity
colnames(roc_cura) <- c('threshold', 'tpr', 'fpr')
auc_cura <- roc_auc(data = z, cura, .pred_cura)
auc_cura <- auc_cura$.estimate
cura <- paste('cura (AUC=',toString(round(1-auc_cura,2)),')',sep = '')


z$obito <- as.factor(z$obito)
roc_obito <- roc_curve(data = z, obito, .pred_obito)
roc_obito$specificity <- 1 - roc_obito$specificity
colnames(roc_obito) <- c('threshold', 'tpr', 'fpr')
auc_obito <- roc_auc(data = z, obito, .pred_obito)
auc_obito <- auc_obito$.estimate
obito <- paste('obito (AUC=',toString(round(1-auc_obito,2)),')', sep = '')

z$obito_outras_causas <- as.factor(z$obito_outras_causas)
roc_obito_outras_causas <- roc_curve(data = z, obito_outras_causas, .pred_obito_outras_causas)
roc_obito_outras_causas$specificity <- 1 - roc_obito_outras_causas$specificity
colnames(roc_obito_outras_causas) <- c('threshold', 'tpr', 'fpr')
auc_obito_outras_causas <- roc_auc(data = z, obito_outras_causas, .pred_obito_outras_causas)
auc_obito_outras_causas <- auc_obito_outras_causas$.estimate
obito_outras_causas <- paste('obito_outras_causas (AUC=',toString(round(1-auc_obito_outras_causas,2)),')',sep = '')

# Create an empty figure, and iteratively add a line for each class
fig <- plot_ly()%>%
  add_segments(x = 0, xend = 1, y = 0, yend = 1, line = list(dash = "dash", color = 'black'), showlegend = FALSE) %>%
  add_trace(data = roc_cura,x = ~fpr, y = ~tpr, mode = 'lines', name = cura, type = 'scatter')%>%
  add_trace(data = roc_obito,x = ~fpr, y = ~tpr, mode = 'lines', name = obito, type = 'scatter')%>%
  add_trace(data = roc_obito_outras_causas,x = ~fpr, y = ~tpr, mode = 'lines', name = obito_outras_causas, type = 'scatter')%>%
  layout(xaxis = list(
    title = "False Positive Rate"
  ), yaxis = list(
    title = "True Positive Rate"
  ),legend = list(x = 100, y = 0.5))
fig




