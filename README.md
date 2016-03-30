# previmov

*previmov* é um trabalho colaborativo coordenado pelo Professor [Regis A. Ely](http://regisely.com) e inicialmente desenvolvido pelos alunos do [Mestrado em Economia Aplicada](http://wp.ufpel.edu.br/ppgom/) e do Curso de Graduação em Economia da Universidade Federal de Pelotas (UFPel). No momento os participantes são:

| Nome | Descrição |
|---|---|
| Regis A. Ely | Professor de Economia (PPGOM/UFPel) |
|Rafael Parfitt|  Aluno do PPGOM |
| Fabio Michel de Oliveira | Aluno mestrado PPGOM 2016|
| Michel R. Meyer | Graduando em Economia (DECON/UFPel) |
|  Leonardo Cordeiro | Aluno (PPGOM/UFPel)   |
| Renan P. Peres | Aluno de Mestrado (PPGOM/UFPel) |
| Patricia Colussi  |  Aluna de Mestrado (ppgom/UFPel)|
|   |   |
|   |   |

O objetivo deste trabalho é desenvolver algoritmos para coleta, análise e previsão de preços de imóveis baseado em suas características. Inicialmente a análise será aplicada para a cidade de Pelotas-RS, podendo ser estendida para outras cidades futuramente.

*previmov* é um trabalho em progresso. No momento este repositório inclui:

1. Rotinas em Python para coleta de cerca de 5200 imóveis à venda na cidade de Pelotas, disponibilizados em um arquivo csv;

2. Scripts para a análise dos dados em [R](https://www.r-project.org/);

3. Dois aplicativos interativos construídos através do framework [shiny](http://shiny.rstudio.com/), um para a análise visual dos dados e outro para construção de tabelas.

# Como utilizar

No momento você pode acessar os aplicativos interativos criados no R com o pacote shiny (em desenvolvimento). Para isso abra a sua sessão do R e digite:

```r
install.packages("shiny") # Instale o pacote shiny caso não o tenha
library(shiny)
runGitHub("previmov", "regisely", subdir="graphs")
runGitHub("previmov", "regisely", subdir="tables")
```

# Como contribuir

Se você deseja contribuir com este projeto você pode:

1. Submeter [issues](https://github.com/regisely/previmov/issues) neste site ao identificar problemas e/ou sugestões relevantes;

2. Criar novos scripts ou modificar arquivos existentes utilizando o git (acesse o arquivo [git-info.md](git-info.md) para mais informações);

3. Enviar sugestões ou contribuições através do email regisaely@gmail.com. 

Caso você queira saber mais especificamente onde contribuir você pode acessar uma [lista de tarefas](TODO.md) pendentes que está sendo constantemente atualizada.

# Copyright

No momento este projeto não se enquadra na definição de "software livre". Ele está licenciado sob [Creative Commons Attribution-NonCommercial-ShareAlike 3.0 Unported](https://creativecommons.org/licenses/by-nc-sa/3.0/). Esta licença permite que você copie, redistribua e adapte qualquer código encontrado aqui, porém você não pode usá-los com intuito comercial e qualquer obra derivada deve atribuir o crédito original elencando as mudanças que foram feitas, além de estar licenciada sob os mesmos termos. Isto vale também para a utilização dos dados disponibilizados neste repositório. Se você tem interesse na utilização destes dados para pesquisa científica, entre em contato com o proprietário do repositório. Note que a utilização destes dados para publicação em uma revista que não está licenciada sobre os mesmos termos implica na violação do Copyright.
