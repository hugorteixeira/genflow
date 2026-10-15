# Contratos de runtime da Genflow 0.0.9

A biblioteca continua responsável pela inferência, pelos adaptadores, pela
preparação de entradas e pelos artefatos opcionais. PostgreSQL, API, fila durável,
workflows, decisões humanas e editor visual pertencem ao aplicativo separado.
Esta versão prepara essa fronteira; não implementa o aplicativo nem vídeo.

## Pedido e execução

Chamadas diretas, métodos de agentes e batches utilizam os mesmos argumentos dos
geradores. O agente reúne setup, conteúdo e overrides por campo completo: a
última definição vence; listas de configuração não são mescladas recursivamente.
Um override explícito `NULL` limpa o campo. `...` nos geradores não é um canal
oculto para parâmetros de provedor; argumentos desconhecidos são rejeitados.

`gen_batch_agent()` preserva quantidade (`qty`) e concorrência (`workers`)
independentes, sem clones por tarefa em `.GlobalEnv`. Cada batch mantém um
snapshot das definições de provedores e configuração local. Workers PSOCK
recebem esse snapshot e as opções de caminhos reconhecidas; chaves continuam
resolvidas pelo ambiente. Entradas de arquivo devem permanecer imutáveis durante
uma execução. Sua identidade de cache inclui bytes, não só nomes; mudar o arquivo
antes de retomar invalida a execução anterior. URLs não são snapshots de conteúdo.

O catálogo e `gen_capabilities(service, model, reasoning)` descrevem o adaptador.
`model_support = "unverified"` é intencional: uma lista de modelos não comprova
que cada um aceita ferramentas, imagens ou parâmetros específicos. O registry
centraliza os dez provedores OpenAI-compatible, seus endpoints, credenciais,
catálogos e dispatch; adaptadores de protocolos diferentes permanecem próprios.

## Resultado

Os campos públicos anteriores permanecem: `response_value`, `service`, `model`,
`duration`, `status_api`, `status_msg` e os específicos de cada modalidade.

| Campo novo | Significado |
| --- | --- |
| `schema_version` | Versão 1 deste envelope |
| `inference_status` | `SUCCESS` ou `ERROR`, antes da publicação em disco |
| `persistence$status` | `saved`, `temporary`, `not_requested` ou `failed` |
| `persistence$path/bytes/sha256` | Identidade do arquivo quando disponível |
| `error` | NULL ou `{code, message, retryable, stage}` |
| `usage$source` | `provider`, `estimated` ou `unknown` |
| `usage$input_tokens/output_tokens` | Uso disponível; campos desconhecidos ficam NA |

Se a inferência terminar e a gravação falhar, `status_api` será `ERROR`,
`inference_status` será `SUCCESS` e `error$stage` será `persistence`. Texto e
transcrição permanecem no retorno; áudio permanece no temporário gerado; imagem
retorna um temporário de recuperação quando sua gravação é possível. Falhar
inclusive a recuperação de bytes não implica que exista um arquivo recuperável.
`gen_batch()` não resubmete inferência bem-sucedida apenas para tentar corrigir
essa falha de publicação. O chamador deve salvar o conteúdo recuperado.

Em texto, `tokens_sent` e `tokens_received` continuam sendo estimativas legadas.
Não são substituídos silenciosamente pelo uso real. Quando o provedor informa,
`usage` guarda seus números, e `provider_request_id`, `provider_model`,
`finish_reason` e `truncated` preservam a proveniência. Truncamento com texto
útil permanece sucesso parcial, marcado por `truncated = TRUE`; o workflow deve
validar sua completude. Recusas e respostas vazias não são sucesso.

Texto gerado pode começar com `API_ERROR`, `TIMEOUT` ou qualquer outra palavra.
Adaptadores envolvem conteúdo válido em um resultado tipado antes da fronteira
que ainda aceita sentinelas de erro legadas. Novos adaptadores devem fazer o mesmo.

## Arquivos e configurações

Texto, imagem, TTS e STT compartilham publicação por staging no mesmo diretório,
nomes com identidade aleatória de 128 bits, sanitização e hash do arquivo final.
Labels são nomes de apresentação, nunca caminhos. Os nomes `pedido_dir_mkt` e
`response_dir_mkt` não têm comportamento especial. `res_context` decide se o
prompt entra no arquivo de texto.

- Texto: `persist = FALSE` evita arquivos e estatísticas do gerador.
- Imagem/TTS: `persist = FALSE` retorna o arquivo temporário em `response_value`,
  marca `persistence$status = "temporary"` e deixa `saved_file = NA`. O chamador
  deve movê-lo ou removê-lo antes do fim da sessão R.
- STT: `save_txt = FALSE` preserva o contrato existente. Checkpoints continuam
  sob controle dos argumentos próprios de STT, independentemente do TXT final.
- Publicação atômica evita arquivos finais parciais; não promete durabilidade
  contra perda de energia nem uma transação conjunta entre banco e disco.

Setup, agente, conteúdo salvo e bundles aceitam dados portáveis. Funções,
ambientes, expressões executáveis e campos de credenciais literais, inclusive
aninhados, são rejeitados. Use nomes de variáveis de ambiente para credenciais;
funções construtoras de ferramentas continuam possíveis em chamadas efêmeras.
Essa validação por estrutura não é um detector de segredos dentro de texto livre.
O usuário continua responsável pelo conteúdo que inclui em prompts e bundles.

A atualização de provedores protege todo o ciclo de leitura/modificação/gravação
com lock e substituição atômica. Exportações são validadas e montadas antes de
substituir um ZIP anterior. A importação de várias entidades permanece uma
operação por arquivos, sem transação global. Estatísticas RDS diárias permanecem
para compatibilidade; o aplicativo deverá inserir seus registros no PostgreSQL.

## Votação e retries

`gen_vote()` conta cada elemento de um vetor, ignora respostas com status de
erro, e restringe candidatos aos itens de `underlying_list` quando fornecida.
`return_winner = "result"` fornece estado, vencedor, contagem, quorum e empatados.
`min_votes` determina o quorum. `ties = "none"` é o padrão seguro: empate não
seleciona silenciosamente um vencedor. `ties = "first"` é a escolha explícita
para o comportamento anterior; `ties = "error"` interrompe. Votos continuam
extraídos por marcador textual; um schema de votos JSON pertence ao workflow.

`gen_txt()` repete somente respostas vazias e carregamento HF quando
`null_repeat = TRUE`. `retry_delays = c(2, 5, 10)` permite três tentativas
adicionais. `retry_deadline` limita a fase de inferência após resolver o modelo;
`numeric()` nos delays desliga retries. Exaustão retorna erro estruturado.
Fallback de endpoint não repete submissões por timeout ambíguo, erro 429 ou 5xx.
Política durável de jobs, `Retry-After`, cancelamento e orçamento total do
workflow ainda serão responsabilidades do backend. STT mantém seus controles
próprios: o worker futuro deve escolher explicitamente retries finitos.

## Compatibilidade e validação

Entrypoints públicos de agentes e Shiny permanecem. A UI é construída somente
quando lançada, por `.genflow_agent_app()`; suas dependências continuam em
Imports, pois extraí-las exige migrar todas as referências do namespace.
A semântica de chunks STT, namespaces locais de locutor, timestamps e checkpoints
opacos permanece preservada. Apenas a rota remota HF também muda na assinatura
para invalidar caches da configuração anterior.

PSOCK é o padrão. Fork é opt-in Unix e mantém limpeza de filhos com SIGKILL na
interrupção. Testes que misturam fork com jobs `processx` configuram
`PROCESSX_NOTIFY_OLD_SIGCHLD=true` antes do carregamento, conforme a
[documentação do processx](https://processx.r-lib.org/#mixing-processx-and-the-parallel-base-r-package).
A biblioteca não muda o ambiente ou o tratamento de sinais da sessão do usuário.

Os testes incluem contratos HTTP interceptados, publicação/recuperação de
arquivos, votos, cache por bytes e servidor HTTP local com workers PSOCK reais.
Eles não qualificam todos os modelos comerciais nem substituem smokes
credenciados dos provedores escolhidos para o futuro aplicativo.

As correções de rota e sampling seguem a documentação primária:
[Hugging Face router](https://huggingface.co/docs/inference-providers/en/index),
[HF Inference](https://huggingface.co/docs/inference-providers/en/providers/hf-inference)
e [compatibilidade de parâmetros OpenAI](https://developers.openai.com/api/docs/guides/latest-model?model=gpt-5.2).
