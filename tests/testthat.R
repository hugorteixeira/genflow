# This suite deliberately mixes fork tests and processx-managed native jobs.
# Follow processx's documented SIGCHLD interoperability setup before loading it.
if (!nzchar(Sys.getenv("PROCESSX_NOTIFY_OLD_SIGCHLD"))) {
  Sys.setenv(PROCESSX_NOTIFY_OLD_SIGCHLD = "true")
}
library(testthat)
library(genflow)

test_check("genflow")
