proposal_bundle <- function() list(format='genflow-studio-definition',schema_version=1L,
 root=list(kind='workflow',name='Personagem',body=list(schema_version=1L,nodes=list(
  list(id='perfil',type='template',config=list(template='Uma gata alaranjada chamada Mimi.')),
  list(id='imagem',type='generate',config=list(count=2L,agent_drafts=list(list(agent=list(operation='image.generate',contract_version=2L,parameters=list()),prompt=list(template='Desenhe: {{perfil}}')))),inputs=list(perfil=list(node='perfil',port='asset'))),
  list(id='escolha',type='select',config=list(mode='manual'),inputs=list(candidates=list(node='imagem',port='candidates'))),
  list(id='fim',type='output',inputs=list(imagem=list(node='escolha',port='asset')))))),resources=list(),assets=list())

test_that('proposals are portable editable configuration, never executable agents', {
 d <- proposal_bundle()
 a <- gen_validate_structure(d,purpose='authoring')
 expect_true(a$valid, info=paste(vapply(a$errors,`[[`,character(1),'message'),collapse='; '))
 expect_false(a$ready)
 expect_identical(a$ports$imagem$candidates,'candidates:image')
 expect_null(a$document$root$body$nodes[[2]]$config$agent_drafts[[1]]$agent$service)
 expect_identical(a$requirements[[1]]$code,'agent_draft_pending')
 expect_false(gen_validate_structure(d)$valid)
 m <- gen_materialize_structure(d,'m',purpose='authoring')
 expect_true(m$valid)
 expect_identical(m$document$root$body$nodes[[2]]$config$agent_drafts,d$root$body$nodes[[2]]$config$agent_drafts)
 e <- gen_extract_structure(d,list('imagem'),purpose='authoring')
 expect_true(e$valid)
 expect_identical(e$boundary$inputs$perfil,list(node='perfil',port='asset'))
 expect_true(gen_validate_structure(jsonlite::toJSON(m$document,auto_unbox=TRUE,null='null'),purpose='authoring')$valid)
})

test_that('authoring still rejects wrong supplied fields, broken ports, and cycles', {
 d <- proposal_bundle(); d$root$body$nodes[[2]]$config$agent_drafts[[1]]$agent$parameters$n <- 'many'
 expect_false(gen_validate_structure(d,purpose='authoring')$valid)
 d <- proposal_bundle(); d$root$body$nodes[[2]]$inputs$perfil$port <- 'invented'
 expect_true(any(vapply(gen_validate_structure(d,purpose='authoring')$errors,function(e)e$code=='unknown_port',logical(1))))
 d <- proposal_bundle(); d$root$body$nodes[[1]]$inputs <- list(x=list(node='escolha',port='asset'))
 expect_true(any(vapply(gen_validate_structure(d,purpose='authoring')$errors,function(e)e$code=='cycle',logical(1))))
 d <- proposal_bundle(); d$root$body$nodes[[2]]$config$agent_version_id <- 'x'
 expect_false(gen_validate_structure(d,purpose='authoring')$valid)
 d <- proposal_bundle(); d$root$body$nodes[[2]]$config$agent_drafts <- list(list(agent=list(operation='invented')))
 expect_false(gen_validate_structure(d,purpose='authoring')$valid)
 expect_false(gen_validate_structure(proposal_bundle(),purpose='anything')$valid)
})

test_that('missing required connections are explicit requirements in authoring', {
 d <- list(schema_version=1L,nodes=list(list(id='a',type='template',config=list(template='{{input}}'))))
 expect_false(gen_validate_structure(d)$valid)
 a <- gen_validate_structure(d,purpose='authoring')
 expect_true(a$valid); expect_false(a$ready)
 expect_identical(a$requirements[[1]]$code,'required_input')
 d$nodes[[1]]$config$template <- 10
 expect_false(gen_validate_structure(d,purpose='authoring')$valid)
})

test_that('draft prompt references use the canonical resource walker', {
 d <- proposal_bundle(); n <- d$root$body$nodes[[2]]
 n$config$agent_drafts[[1]]$prompt <- NULL
 n$config$agent_drafts[[1]]$agent$prompt_version_id <- 'p'
 b <- list(nodes=list(n))
 refs <- gen_definition_references(b,'workflow')
 expect_identical(refs[[1]]$kind,'prompt')
 expect_identical(gen_remap_definition(b,'workflow',list(p='new'))$nodes[[1]]$config$agent_drafts[[1]]$agent$prompt_version_id,'new')
})

test_that('malformed proposal objects produce diagnostics, never exceptions', {
 for (bad in list(5,'x',list(agent=5),list(agent=list(operation='text.generate'),prompt=4))) {
  d <- proposal_bundle(); d$root$body$nodes[[2]]$config$agent_drafts <- list(bad)
  expect_false(gen_validate_structure(d,purpose='authoring')$valid)
 }
 d <- list(schema_version=1L,nodes=list(list(id='a',type='subworkflow',config=list())))
 expect_true(gen_validate_structure(d,purpose='authoring')$valid)
 d <- list(schema_version=1L,nodes=list(list(id='a',type='generate',config=list()),list(id='b',type='output',inputs=list(result=list(node='a',port='asset')))))
 a <- gen_validate_structure(d,purpose='authoring')
 expect_true(a$valid); expect_false(a$ready); expect_null(a$ports$a)
})


test_that('agent definition reports missing identity without choosing defaults', {
 a <- gen_validate_agent_definition(list(operation='text.generate'),purpose='authoring')
 expect_true(a$valid); expect_false(a$ready); expect_length(a$requirements,2)
 expect_null(a$body$service); expect_null(a$body$model)
})

test_that('malformed prompt identities return field diagnostics without indexing resources', {
 for (bad in list(list(),list('a','b'),c('a','b'),1,TRUE,list(a=1))) {
  d <- proposal_bundle(); d$root$body$nodes[[2]]$config$agent_drafts[[1]]$prompt <- NULL
  d$root$body$nodes[[2]]$config$agent_drafts[[1]]$agent$prompt_version_id <- bad
  report <- gen_validate_structure(d,purpose='authoring')
  expect_false(report$valid)
  expect_true(any(vapply(report$errors,function(e) grepl('prompt_version_id',e$path,fixed=TRUE),logical(1))))
 }
})

test_that('authoring keeps portable published dependencies strict', {
 d <- proposal_bundle(); child <- d$root; child$key <- 'child'
 d$resources <- list(child)
 d$root$body <- list(schema_version=1L,nodes=list(list(id='child',type='subworkflow',config=list(workflow_id='child'))))
 a <- gen_validate_structure(d,purpose='authoring')
 expect_false(a$valid)
 expect_true(any(vapply(a$errors,function(e) startsWith(e$path,'/resources/0/body/'),logical(1))))
 expect_false(gen_materialize_structure(d,purpose='authoring')$valid)
})
