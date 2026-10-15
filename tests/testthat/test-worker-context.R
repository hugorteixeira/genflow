test_that("PSOCK workers use the same custom provider and prompt as direct calls", {
  skip_if(!nzchar(Sys.which("python3")), "Python is required for the local HTTP fixture")
  root <- tempfile("genflow-worker-http-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE))
  withr::local_options(genflow.providers_path = file.path(root, "providers.json"))
  # Credentials are unnecessary; all requests remain on loopback.
  server_script <- file.path(root, "server.py")
  writeLines(c(
    "import http.server, json, sys",
    "class Handler(http.server.BaseHTTPRequestHandler):",
    "  def log_message(self, *args): pass",
    "  def do_POST(self):",
    "    data = json.loads(self.rfile.read(int(self.headers['Content-Length'])))",
    "    content = data['messages'][0]['content']",
    "    result = json.dumps({'id':'local-request','model':data['model'],'choices':[{'message':{'content':content}}]}).encode()",
    "    self.send_response(200)",
    "    self.send_header('Content-Type', 'application/json')",
    "    self.send_header('Content-Length', str(len(result)))",
    "    self.end_headers()",
    "    self.wfile.write(result)",
    "server = http.server.ThreadingHTTPServer(('127.0.0.1', 0), Handler)",
    "with open(sys.argv[1], 'w') as f: f.write(str(server.server_port))",
    "server.serve_forever()"
  ), server_script)
  port_file <- file.path(root, "port")
  server <- processx::process$new(Sys.which("python3"), c(server_script, port_file), cleanup = TRUE)
  withr::defer(server$kill())
  deadline <- Sys.time() + 5
  while (!file.exists(port_file) && server$is_alive() && Sys.time() < deadline) Sys.sleep(0.01)
  expect_true(file.exists(port_file))
  port <- readLines(port_file, warn = FALSE)
  set_provider_openai_compat("workerfixture", base_url = paste0("http://127.0.0.1:", port),
    api_key_required = FALSE, default_model = "fixture")
  agent <- set_agent("worker-context", setup = list(service = "workerfixture", model = "fixture", type = "Chat"),
    content = list(context = "instruction", add = "content"), save = FALSE, assign = FALSE)
  direct <- gen_txt(agent, persist = FALSE)
  batch <- gen_batch_agent(agent, qty = 2, workers = 2, persist = FALSE,
    verbose = FALSE, always_fix_errors = FALSE)
  expect_identical(direct$status_api, "SUCCESS")
  expect_identical(batch$combined_stats$backend_requested, "psock")
  expect_true(all(vapply(batch[1:2], function(result) identical(result$status_api, "SUCCESS"), logical(1))))
  expect_identical(batch[[1]]$response_value, direct$response_value)
  expect_identical(batch[[2]]$response_value, direct$response_value)
  expect_identical(batch[[2]]$provider_request_id, "local-request")
})

test_that("concurrent provider edits preserve every registry entry", {
  root <- tempfile("genflow-registry-race-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE))
  registry <- file.path(root, "providers.json")
  cluster <- parallel::makeCluster(2L)
  withr::defer(parallel::stopCluster(cluster))
  ids <- paste0("concurrent", 1:12)
  parallel::parLapplyLB(cluster, ids, function(id, path) {
    options(genflow.providers_path = path)
    genflow::set_provider_openai_compat(id, base_url = "http://127.0.0.1:1", api_key_required = FALSE)
    TRUE
  }, path = registry)
  withr::local_options(genflow.providers_path = registry)
  expect_setequal(list_providers(include_builtin = FALSE)$id, ids)
  expect_false(dir.exists(paste0(registry, ".lock")))
})
