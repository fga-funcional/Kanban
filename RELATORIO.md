# RELATORIO

## Front-end

No front-end em Elm, foram implementados os encoders e decoders de JSON, refinamento da lógica de modificação dos status, e animações. 

## Back-end

Em Haskell, foi implementado um pequeno back-end utilizando Scotty para servir o JSON ao front-end. O mecanismo de persistência pretendido, por meio de arquivo .json, não foi implementado com sucesso. O back-end atende uma requisição GET e retorna um JSON que está hardcoded no código-fonte, e que é decodificado (os tipos derivam a classe Generics), e recodificado em seguida.

## Desenvolvimento técnico  (3,0 pts)
  - Algum mecanismo de persistência?
  
  Existe um mecanismo de persistência que só não está funcional por causa de erro de CORS do haskell, o json é lido e salvo em um arquivo.

  - Utilizou Recursos avançados da linguagem? (Elm ou Haskell)
  
  Foram utilizadas as mônadas em haskell para realizar as operações IO para salvar em arquivo.

  - Rotas? Tasks e Subscribers em Elm?
  
  Sim. Existe o subscriber em elm para a realização das animações.

  - Criou tipos Union types?
  
  Foram criados os union types padrão para trabalhar com o update msg e ainda foi criado um union type para o status de uma tarefa, podendo ser todo, doing, done, etc...
  
  - Instanciou alguma classe explicitamente em Haskell?
  
  Não foi necessário instanciar nenhuma classe em Haskell.

## Qualidade do Produto  (3,0 pts)
   - Ignorando a aparência, implementa recursos básicos esperados?
  
  Sim. É possível criar novas tarefas e passar todas elas em todos os níveis do kanban.

   - Implementa interações de forma eficiente?
  
  Sim.

   - Conseguiu polir a aplicação?
  
  Sim. Tanto o Front-end quanto o Back-end estão bem modularizados e ambos estão seguindo padrões das linguagens (como a tipagem das funções). 

   - Pronto para produção?	
  
  Não inteiramente, porém em vários pontos está funcional.

## Integração front + back   (2,5 pts)
  - Front usa backend como mecanismo de persistência?
  
  Sim. É usado arquivo json para persistir os dados. O get funciona normalmente, porém o post não funciona a partir da aplicação do Elm por motivos de CORS. Porém, o backend se comporta de maneira adequada ao processar um json via CURL.

  - Conseguiu conectar os dois sistemas adequadamente?
  
  Sim, cors.

  - Consegue rodar mais de uma instâcia (discriminada por URL, por exemplo)
  
  Não.
   
## Método  (1,5 pts)
   - Possui sistema de build?
   
   Não.

   - Testes unitários e boas práticas?
   
   Não.

   - Implantou em algum lugar?
   
   Não.
