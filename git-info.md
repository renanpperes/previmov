## Instale o git (caso não o tenha)

Siga as instruções [aqui](https://confluence.atlassian.com/bitbucket/set-up-git-744723531.html) para instalar e configurar o seu nome e email no git.

## Faça um fork do repositório

Primeiro crie um usuário no [github](https://github.com), depois entre no site deste repositório e clique no botão Fork. Agora você terá uma cópia deste repositório na sua conta pessoal do github.

## Clone seu repositório do github para seu computador local

Abre o terminal do Linux ou o prompt de comando do Windows (procure por cmd), entre no seu diretório pessoal e digite:

```
git clone https://github.com/[SEU_USUARIO]/previmov.git
```

Este comando criará uma cópia local no seu computador de todos os arquivos do projeto em um novo diretório chamado `previmov`.

## Configure a sincronização com o repositório original

Para atualizar a sua cópia local com as últimas alterações do repositório original, primeiro você precisa configurar o acesso a este repositório:

```
git remote add upstream https://github.com/regisely/previmov.git
```

Note que ao digitar `git remote -v` você visualizará dois repositórios remotos, o `origin`, que se refere a sua conta do github, e o `upstream`, que se refere ao repositório original.

## Atualizando seus arquivos locais

Para atualizar seus arquivos locais com o repositório original, digite:

```
git pull upstream master
```

Você pode digitar `git status` a qualquer momento para checar a situação dos seus arquivos locais em relação aos repositórios.

## Contribuindo para o repositório original

Você pode fazer alterações nos seus arquivos locais sem afetar os arquivos dos repositórios do github. Caso você crie um novo arquivo chamado `script.R` e queira adicioná-lo ao seu repositório do github, digite:

```
git add "script.R"
git commit -m "Novo script para modelo linear"
git push origin master
```

Lembre de sempre atualizar seu repositório usando o comando da seção anterior antes de realizar novas mudanças.

Caso tenha feito várias alterações nos arquivos existentes e/ou criado novos arquivos e gostaria de adicionar todas as alterações no seu repositório local você pode usar `git add .` ou `git add *` e então dar o commit e o push. Para remover um arquivo do repositório é necessário usar `git add -A`.

## Pull requests

Uma vez que você fez as alterações e mandou para o seu repositório no github, você pode solicitar que essas alterações sejam aceitas no repositório original. Para isso entre no seu repositório do github e clique em Pull Request e siga as instruções.

## Mais informações sobre o git

Um tutorial completo sobre o git pode ser obtido [aqui](https://git-scm.com/book/pt-br/v1/Primeiros-passos-No%C3%A7%C3%B5es-B%C3%A1sicas-de-Git).

Um guia prático dos principais comandos pode ser obtido [aqui](http://rogerdudler.github.io/git-guide/index.pt_BR.html).

Você pode obter uma lista maior dos comandos do git [aqui](https://confluence.atlassian.com/bitbucketserver/basic-git-commands-776639767.html).

Digite `git --help` ou `git [command] --help` para obter informações rápidas na linha de comando.
