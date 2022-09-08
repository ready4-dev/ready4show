knit_from_tmpl <- function (params_to_expand_ls, path_to_tmpl_1L_chr) # Migrated from specific / TTU
{
  src <- purrr::map(params_to_expand_ls,
                    ~{
                      args_ls <- append(list(file = path_to_tmpl_1L_chr),
                                        .x)
                      rlang::exec(knitr::knit_expand,
                                  !!!args_ls)
                    })
  res <- knitr::knit_child(text = unlist(src), quiet = TRUE)
  cat(res, sep = "\n")
}
