manufacture_Ready4showSynopsis <- function(x,
                                           what_1L_chr = "paths_ls"){
  if(what_1L_chr == "paths_ls"){
    outp_dir_1L_chr <- x@a_Ready4showPaths@outp_data_dir_1L_chr
    reports_dir_1L_chr <- paste0(outp_dir_1L_chr,
                                "/",
                                x@a_Ready4showPaths@reports_dir_1L_chr)
    object_xx <- list(outp_dir_1L_chr = outp_dir_1L_chr,
                      mkdn_data_dir_1L_chr = paste0(outp_dir_1L_chr,
                                                    "/",
                                                    x@a_Ready4showPaths@mkdn_data_dir_1L_chr),
                      reports_dir_1L_chr = reports_dir_1L_chr,
                      path_to_ms_mkdn_dir_1L_chr = paste0(outp_dir_1L_chr,
                                                          "/",
                                                          x@a_Ready4showPaths@mkdn_data_dir_1L_chr,
                                                          "/",
                                                          x@a_Ready4showPaths@ms_mkdn_dir_1L_chr),
                      path_to_ms_outp_dir_1L_chr = paste0(reports_dir_1L_chr,
                                                          "/",
                                                          x@a_Ready4showPaths@ms_dir_1L_chr))
  }
  return(object_xx)
}
