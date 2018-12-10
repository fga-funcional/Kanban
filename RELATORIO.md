# RELATORIO

## Front-end

No front-end em Elm, foram implementados os encoders e decoders de JSON, refinamento da lógica de modificação dos status, e animações. 

## Back-end

Em Haskell, foi implementado um pequeno back-end utilizando Scotty para servir o JSON ao front-end. O mecanismo de persistência pretendido, por meio de arquivo .json, não foi implementado com sucesso. O back-end atende uma requisição GET e retorna um JSON que está hardcoded no código-fonte, e que é decodificado (os tipos derivam a classe Generics), e recodificado em seguida.

- Desenvolvimento técnico  (3,0 pts)
  - Algum mecanismo de persistência?
  Não concluído.

  - Utilizou Recursos avançados da linguagem? (Elm ou Haskell)
  Não.

  - Rotas? Tasks e Subscribers em Elm?
  Sim.

  - Criou tipos Union types?
  Não.
  
  - Instanciou alguma classe explicitamente em Haskell?
  Sim.

- Qualidade do Produto  (3,0 pts)
   - Ignorando a aparência, implementa recursos básicos esperados?
   Sim.

   - Implementa interações de forma eficiente?
   Sim.

   - Conseguiu polir a aplicação?
   Não.

   - Pronto para produção?	
   Não.

- Integração front + back   (2,5 pts)
  - Front usa backend como mecanismo de persistência?
  Sim.

  - Conseguiu conectar os dois sistemas adequadamente?
  Sim.

  - Consegue rodar mais de uma instâcia (discriminada por URL, por exemplo)
  Não.
   
- Método  (1,5 pts)
   - Possui sistema de build?
   Não.

   - Testes unitários e boas práticas?
   Não.

   - Implantou em algum lugar?
   Não.
