# Intruções para Deploy no Code Engine

Pré-requisitos:

- Docker ou Podman

## Build da Imagem

1. Clone o repositório para sua máquina.
2. Faça um build no repositório
3. Suba a imagem para o registry que escolher.

## Deploy no Code Engine

1. Crie um projeto no Code Engine
2. Crie um Secret no Code Engine
   1. Adicione as `PASSWORD` como uma variável de ambiente e preencha seu valor;
   2. Adicione as `ES_URL` como uma variável de ambiente e preencha seu valor;
   3. Adicione as `GITHUB_CLIENT_ID` como uma variável de ambiente e preencha seu valor;
   4. Adicione as `GITHUB_CLIENT_SECRET` como uma variável de ambiente e preencha seu valor;
3. Com o Secret criado, crie uma nova aplicação
4. Insira o endereço da sua imagem no formulário de criação.
5. Defina pelo menos uma instância ativa
6. Defina o secret criado no passo 2 como variaveis de ambiente
7. Verifique e defina que a porta exposta seja a 8080
8. Clique em criar
9. Em intantes sua instancia deve estar dísponível para ser acessada.
