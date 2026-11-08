test_that("validation preserves explicitly invalid fields in its prepared document", {
  for (field in c("config","inputs","ui")) for (literal in c("null","[]","false","1","\"bad\"")) {
    d <- gen_parse_json(paste0('{"schema_version":1,"nodes":[{"id":"text","type":"content","',field,'":',literal,
      if(field!="config") ',"config":{"content_type":"text","source":"inline","text":"Olá"}' else '', '}]}'))
    r <- gen_validate_structure(d,purpose="authoring",check_capabilities=FALSE)
    expect_false(r$valid)
    expect_true(field %in% names(r$document$nodes[[1]]))
    expect_identical(r$document$nodes[[1]][[field]],d$nodes[[1]][[field]],info=paste(field,literal))
  }
})
