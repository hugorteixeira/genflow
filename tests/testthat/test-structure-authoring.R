proposal_bundle <- function() list(format="genflow-studio-definition",schema_version=1L,
 root=list(kind="workflow",name="Personagem",body=list(schema_version=1L,nodes=list(
  list(id="perfil",type="content",config=list(content_type="text",source="inline",text="Uma gata alaranjada chamada Mimi.")),
  list(id="imagem",type="content",config=list(content_type="image",source="ai",ai=list(contract_version=3L,parameters=structure(list(),names=character()),content=list(list(text="Desenhe: "),list(input="perfil")))),inputs=list(perfil=list(node="perfil",port="asset"))),
  list(id="fim",type="output",inputs=list(imagem=list(node="imagem",port="asset")))))),resources=list(),assets=list())

test_that("incomplete inline AI is portable and cannot execute until configured", {
 d <- proposal_bundle(); a <- gen_validate_structure(d,purpose="authoring")
 expect_true(a$valid); expect_false(a$ready); expect_identical(a$ports$imagem$asset,"asset:image")
 expect_null(a$document$root$body$nodes[[2]]$config$ai$service)
 expect_gt(length(a$requirements),0); expect_false(gen_validate_structure(d)$valid)
 m <- gen_materialize_structure(d,"m",purpose="authoring"); expect_true(m$valid)
 expect_identical(m$document$root$body$nodes[[2]]$config$ai,d$root$body$nodes[[2]]$config$ai)
 e <- gen_extract_structure(d,list("imagem"),purpose="authoring"); expect_true(e$valid)
 expect_identical(e$boundary$inputs$perfil,list(node="perfil",port="asset"))
 expect_true(gen_validate_structure(jsonlite::toJSON(m$document,auto_unbox=TRUE,null="null"),purpose="authoring")$valid)
})

test_that("authoring rejects wrong supplied fields, broken ports, cycles and foreign configuration", {
 d <- proposal_bundle(); d$root$body$nodes[[2]]$config$ai$parameters$n <- "many"
 expect_false(gen_validate_structure(d,purpose="authoring")$valid)
 d <- proposal_bundle(); d$root$body$nodes[[2]]$inputs$perfil$port <- "invented"
 expect_true(any(vapply(gen_validate_structure(d,purpose="authoring")$errors,function(e)e$code=="unknown_port",logical(1))))
 d <- proposal_bundle(); d$root$body$nodes[[1]]$inputs <- list(x=list(node="imagem",port="asset"))
 expect_true(any(vapply(gen_validate_structure(d,purpose="authoring")$errors,function(e)e$code=="cycle",logical(1))))
 d <- proposal_bundle(); d$root$body$nodes[[2]]$config$agent_version_id <- "x"
 expect_false(gen_validate_structure(d,purpose="authoring")$valid)
 expect_false(gen_validate_structure(proposal_bundle(),purpose="anything")$valid)
})

test_that("missing consumed values are authoring requirements and malformed values are errors", {
 d <- list(schema_version=1L,nodes=list(list(id="a",type="text.join",contract_version=2L,config=list(parts=list(list(input="input"))))))
 expect_false(gen_validate_structure(d)$valid)
 a <- gen_validate_structure(d,purpose="authoring"); expect_true(a$valid); expect_false(a$ready)
 expect_identical(a$requirements[[1]]$code,"required_input")
 for(bad in list(5,"x",list(text=5),list(text="a",input="a"))) {
  d$nodes[[1]]$config$parts <- list(bad); expect_false(gen_validate_structure(d,purpose="authoring")$valid)
 }
})
