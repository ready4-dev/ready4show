
setOldClass(c("ready4show_institutes_lup","tbl_df", "tbl", "data.frame"))
#' ready4 S3 class for institutes lookup table
#' @description Create a new valid instance of the ready4 S3 class for institutes lookup table
#' @param x A prototype for the ready4 S3 class for institutes lookup table, Default: make_pt_ready4show_institutes_lup()
#' @return A validated instance of the ready4 S3 class for institutes lookup table
#' @details ready4 S3 class for institutes lookup table
#' @rdname ready4show_institutes_lup
#' @export 

ready4show_institutes_lup <- function(x = make_pt_ready4show_institutes_lup()){ 
validate_ready4show_institutes_lup(make_new_ready4show_institutes_lup(x))
}
#' Make new ready4show institutes lookup table ready4 S3 class for institutes lookup table
#' @description Create a new unvalidated instance of the ready4 S3 class for institutes lookup table
#' @param x A prototype for the ready4 S3 class for institutes lookup table
#' @return An unvalidated instance of the ready4 S3 class for institutes lookup table
#' @details ready4 S3 class for institutes lookup table
#' @rdname make_new_ready4show_institutes_lup
#' @export 
#' @importFrom tibble is_tibble
make_new_ready4show_institutes_lup <- function(x){ 
stopifnot(tibble::is_tibble(x))
class(x) <- append(c("ready4show_institutes_lup",setdiff(make_pt_ready4show_institutes_lup() %>% class(),class(x))),
class(x))
x
}
#' Make prototype ready4show institutes lookup table ready4 S3 class for institutes lookup table
#' @description Create a new prototype for the ready4 S3 class for institutes lookup table
#' @param distribution_chr Distribution (a character vector), Default: character(0)
#' @param dstr_param_1_dbl Distribution parameter 1 (a double vector), Default: numeric(0)
#' @param dstr_param_2_dbl Distribution parameter 2 (a double vector), Default: numeric(0)
#' @param dstr_param_3_dbl Distribution parameter 3 (a double vector), Default: numeric(0)
#' @param dstr_param_4_dbl Distribution parameter 4 (a double vector), Default: numeric(0)
#' @param transformation_chr Transformation (a character vector), Default: character(0)
#' @return A prototype for ready4 S3 class for institutes lookup table
#' @details ready4 S3 class for institutes lookup table
#' @rdname make_pt_ready4show_institutes_lup
#' @export 
#' @importFrom ready4fun update_pt_fn_args_ls
#' @importFrom rlang exec
#' @importFrom tibble tibble
make_pt_ready4show_institutes_lup <- function(distribution_chr = character(0),
dstr_param_1_dbl = numeric(0),
dstr_param_2_dbl = numeric(0),
dstr_param_3_dbl = numeric(0),
dstr_param_4_dbl = numeric(0),
transformation_chr = character(0)){ 
args_ls <- list(distribution_chr = distribution_chr,
dstr_param_1_dbl = dstr_param_1_dbl,
dstr_param_2_dbl = dstr_param_2_dbl,
dstr_param_3_dbl = dstr_param_3_dbl,
dstr_param_4_dbl = dstr_param_4_dbl,
transformation_chr = transformation_chr) %>% ready4fun::update_pt_fn_args_ls()
rlang::exec(tibble::tibble,!!!args_ls)
}
#' Validate ready4show institutes lookup table ready4 S3 class for institutes lookup table
#' @description Validate an instance of the ready4 S3 class for institutes lookup table
#' @param x An unvalidated instance of the ready4 S3 class for institutes lookup table
#' @return A prototpe for ready4 S3 class for institutes lookup table
#' @details ready4 S3 class for institutes lookup table
#' @rdname validate_ready4show_institutes_lup
#' @export 
#' @importFrom stringr str_detect str_c
#' @importFrom dplyr summarise_all filter arrange pull
#' @importFrom tidyr gather
#' @importFrom purrr map_chr map2_chr
validate_ready4show_institutes_lup <- function(x){
if(sum(stringr::str_detect(names(x)[names(x) %in% names(make_pt_ready4show_institutes_lup())],
names(make_pt_ready4show_institutes_lup())))!=length(names(make_pt_ready4show_institutes_lup()))){
stop(paste0("TIBBLE must include columns named: ",
names(make_pt_ready4show_institutes_lup()) %>% stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}

 if(!identical(make_pt_ready4show_institutes_lup() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
 dplyr::filter(!is.na(class)) %>% 
dplyr::arrange(variable),
x %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
 dplyr::filter(!is.na(class)) %>% 
dplyr::filter(variable %in% names(make_pt_ready4show_institutes_lup())) %>% dplyr::arrange(variable))){
stop(paste0("TIBBLE columns should be of the following classes: ",
"",
{
class_lup <- make_pt_ready4show_institutes_lup() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
 dplyr::filter(!is.na(class))
  vars_chr <- class_lup %>% dplyr::pull(1) %>% unique()
  classes_chr <- vars_chr %>%  purrr::map_chr(~dplyr::filter(class_lup, variable == .x) %>%  dplyr::pull(2) %>% paste0(collapse = ", "))
purrr::map2_chr(vars_chr,
classes_chr,
~ paste0(.x,": ",.y)) %>% 
stringr::str_c(sep="", collapse = ", 
")
}),
call. = FALSE)
}

x}
#' Is ready4show institutes lookup table ready4 S3 class for institutes lookup table
#' @description Check whether an object is a valid instance of the ready4 S3 class for institutes lookup table
#' @param x An object of any type
#' @return A logical value, TRUE if a valid instance of the ready4 S3 class for institutes lookup table
#' @details ready4 S3 class for institutes lookup table
#' @rdname is_ready4show_institutes_lup
#' @export 

is_ready4show_institutes_lup <- function(x) inherits(validate_ready4show_institutes_lup(x), "ready4show_institutes_lup")
