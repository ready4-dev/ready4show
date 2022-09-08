transform_abstract <- function(path_to_abstract_1L_chr){
  tfd_abstract_chr <- paste0(stringr::str_replace_all(stringr::str_replace_all(stringr::str_replace_all(stringr::str_replace_all(readLines(path_to_abstract_1L_chr),
                                                                                                                                 '\\\\\\\\textbf',
                                                                                                                                 ''),
                                                                                                        '\\{','**'),
                                                                               '\\}',
                                                                               '**'),
                                                      '\\\\\\\\newline',
                                                      '\\\\n'),
                             collapse=' ')
  return(tfd_abstract_chr)
}