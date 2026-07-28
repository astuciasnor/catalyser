stopifnot(
  "launch.browser" %in% names(formals(catalyser::run_app)),
  "..." %in% names(formals(catalyser::run_app))
)

cat("OK: run_app expõe launch.browser e mantém argumentos adicionais\n")
