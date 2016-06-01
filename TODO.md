## Lista de tarefas

Para melhorar a capacidade de previsão de um modelo devemos trabalhar em 4 esferas principais que são complementares entre si:

i) Obtenção de dados

ii) Complexidade dos modelos

iii) Métodos para avaliação e validação dos modelos

iv) Eficiência computacional

No momento as seguintes medidas podem ser tomadas para a melhoria na previsão ou para a melhor visualização e organização dos dados e scripts:

### Dados

- Fazer script para verificar a existência de imóveis repetidos na base de dados
- Incluir a informação da data de download no arquivo salvo em csv automaticamente utilizando o script imoveis.py
- Criar rotina de extração dos dados no final de cada mês para construção de série temporal
- Verificar outras possibilidades de remoção de outliers automática no arquivo `dados_novos.R`
- Coletar mais informações sobre os imóveis através do arquivo `imoveis.py` (ano de construção, área total, cobertura, etc.)

### Análise preliminar

- Permitir a criação de subconjuntos dos dados no aplicativo `graphs`
- Melhor as opções no aplicativo `graphs`
- Construir tabelas de estatíticas condicionais no aplicativos `tables`
- Melhor as opções no aplicativo `tables`

### Modelos de previsão

- Implementar o método de gradient descent na função train do pacote caret
- Descobrir outras combinações e interações entre variáveis que levam a erros de previsão menores
- Fazer uma busca mais detalhada dos parâmetros nos modelos implementados
- Melhorar a automatização da busca pelos parâmetros para dados novos
- Implementar outros modelos de previsão que possam gerar resultados melhores

### Modelos de validação

- Comparar os resultados do 10-repeated 10-fold cross validation com métodos com menor custo computacional
- Se possível reduzir o custo computacional dos métodos de validação utilizados

### Eficiência computacional

- Aprimorar a função gradient descent
- Aprimorar os códigos que implementam a validação dos modelos
- Medir o tempo necessário para rodar os modelos com a função train através do comando `system.time`
- Utilizar clusters e verificar a economia de tempo para rodar os modelos
- Identificar outras possíveis melhorias no código que pode reduzir o tempo necessário para rodar os modelos

### Organização do material

- Fazer um arquivo em R markdown com explicações detalhadas sobre a implementação dos scritps
- Construir uma FAQ com perguntas comuns que aparecem ao implementar os diversos modelos e métodos de validação (quais são os parâmetros de escolha, opções de validação, etc.) 
- Construir pdf em latex explicando o funcionamento teórico dos modelos implementados
- Construir site no github para acesso aos dados, gráficos e previsões
