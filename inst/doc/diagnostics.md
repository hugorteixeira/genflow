# Diagnósticos de erros

O contrato público está em `inst/diagnostics/1.json`: schema de diagnóstico v1 e
catálogo pt-BR. `gen_error_catalog()` expõe esses dados. As regras continuam nos
contratos dos blocos e nos validadores; o catálogo descreve suas falhas.

```r
report <- gen_validate_call("gen_img", list(prompt = 123))
report$errors[[1]]$code        # invalid_type
report$errors[[1]]$params      # expected_types e actual_type
gen_error_summary(report$errors[[1]])
```

Cada diagnóstico tem `diagnostic_version`, `code`, `source`, `category`, `stage`,
`message_key`, `message`, `hint`, `params`, `retryable` e `outcome`. O código e os
parâmetros são dados para programas; a explicação e a orientação são para pessoas.
`path` aponta para o documento original. `schema_path` identifica a regra. Quando
disponíveis, `node_id`, `node_label`, `field`, `block` e `contract_version`
identificam a peça. `causes` preserva a cadeia de falhas.

`gen_diagnostic()` normaliza uma condição, um código ou um diagnóstico antigo.
Condições `genflow_validation_error` e `genflow_error` carregam `$diagnostic`;
erros de validação também preservam `$errors`. Resultados dos geradores carregam
`$error`. Batches acrescentam `combined_stats$diagnostics` e mantêm
`detailed_errors` para consumidores anteriores.

Aplicações podem fornecer seu próprio `catalog` a `gen_error_message()` e
`gen_diagnostic()`. Cada componente mantém suas mensagens; um catálogo da
aplicação não substitui a explicação de uma causa pertencente à biblioteca.

Para acrescentar uma falha, adicione uma chave estável com `message`, `hint`
opcional, `category` e `stage` opcional. Prefira parâmetros tipados, sem valores
de credenciais, prompts ou respostas completos. `fallback` explica a regra
quando um registro antigo não contém seus parâmetros. `alias` permite que chaves
de compatibilidade compartilhem uma explicação. Teste código, parâmetros e causas;
reserve testes do texto para a apresentação e o catálogo.

`outcome` pode ser `not_started`, `failed`, `unknown` ou `succeeded`. Não deduza
resultado, repetição ou sucesso a partir de uma mensagem. `retryable` informa a
política do produtor; não autoriza sozinho uma nova chamada. Falha de publicação
após inferência bem-sucedida mantém `outcome = "succeeded"` e o conteúdo recuperável.

Exceções desconhecidas recebem uma explicação segura. `log = TRUE` registra uma
referência uma única vez, limita o texto técnico e oculta formatos comuns de
credenciais. A referência permanece no diagnóstico e permite investigar o log
privado. Isso não é um detector de todos os segredos possíveis em texto livre.

Os campos legados `status_msg`, checkpoints e `detailed_errors` continuam
compatíveis; novos consumidores devem apresentar `$error`. Alguns adaptadores
ainda emitem sentinelas internas, normalizadas na fronteira antes da tradução.
As rotinas antigas de chunks STT mantêm suas regras de retomada. Texto gerado não
é reinterpretado como erro. Falhas de terceiros ou bugs novos usam o diagnóstico
genérico com referência, sem exigir uma lista impossível de todas as exceções.
