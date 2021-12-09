
setOldClass(c("ready4show_institutes","tbl_df", "tbl", "data.frame"))
#' ready4 S3 class for institutes lookup table
#' @description Create a new valid instance of the ready4 S3 class for institutes lookup table
#' @param x A prototype for the ready4 S3 class for institutes lookup table, Default: make_pt_ready4show_institutes()
#' @return A validated instance of the ready4 S3 class for institutes lookup table
#' @details ready4 S3 class for institutes lookup table
#' @rdname ready4show_institutes
#' @export 
ready4show_institutes <- function(x = make_pt_ready4show_institutes()){ 
validate_ready4show_institutes(make_new_ready4show_institutes(x))
}
#' make new ready4show institutes ready4 S3 class for institutes lookup table
#' @description Create a new unvalidated instance of the ready4 S3 class for institutes lookup table
#' @param x A prototype for the ready4 S3 class for institutes lookup table
#' @return An unvalidated instance of the ready4 S3 class for institutes lookup table
#' @details ready4 S3 class for institutes lookup table
#' @rdname make_new_ready4show_institutes
#' @export 
#' @importFrom tibble is_tibble
#' @keywords internal
make_new_ready4show_institutes <- function(x){ 
stopifnot(tibble::is_tibble(x))
class(x) <- append(c("ready4show_institutes",setdiff(make_pt_ready4show_institutes() %>% class(),class(x))),
class(x))
x
}
#' make prototype ready4show institutes ready4 S3 class for institutes lookup table
#' @param short_name_chr Short name (a character vector), Default: character(0)
#' @param long_name_chr Long name (a character vector), Default: character(0)
#' @return A prototype for ready4 S3 class for institutes lookup table
#' 
#' @rdname ready4show_institutes
#' @export 
#' @importFrom ready4 update_pt_fn_args_ls
#' @importFrom rlang exec
#' @importFrom tibble tibble
make_pt_ready4show_institutes <- function(short_name_chr = character(0),
long_name_chr = character(0)){ 
args_ls <- list(short_name_chr = short_name_chr,
long_name_chr = long_name_chr) %>% ready4::update_pt_fn_args_ls()
rlang::exec(tibble::tibble,!!!args_ls)
}
#' validate ready4show institutes ready4 S3 class for institutes lookup table
#' @description Validate an instance of the ready4 S3 class for institutes lookup table
#' @param x An unvalidated instance of the ready4 S3 class for institutes lookup table
#' @return A prototpe for ready4 S3 class for institutes lookup table
#' @details ready4 S3 class for institutes lookup table
#' @rdname validate_ready4show_institutes
#' @export 
#' @importFrom stringr str_detect str_c
#' @importFrom dplyr summarise_all filter arrange pull
#' @importFrom tidyr gather
#' @importFrom purrr map_chr map2_chr
#' @keywords internal
validate_ready4show_institutes <- function(x){
if(sum(stringr::str_detect(names(x)[names(x) %in% names(make_pt_ready4show_institutes())],
names(make_pt_ready4show_institutes())))!=length(names(make_pt_ready4show_institutes()))){
stop(paste0("TIBBLE must include columns named: ",
names(make_pt_ready4show_institutes()) %>% stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}

 if(!identical(make_pt_ready4show_institutes() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
 dplyr::filter(!is.na(class)) %>% 
dplyr::arrange(variable),
x %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
 dplyr::filter(!is.na(class)) %>% 
dplyr::filter(variable %in% names(make_pt_ready4show_institutes())) %>% dplyr::arrange(variable))){
stop(paste0("TIBBLE columns should be of the following classes: ",
"",
{
class_lup <- make_pt_ready4show_institutes() %>% 
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
#' is ready4show institutes ready4 S3 class for institutes lookup table
#' @param x An object of any type
#' @return A logical value, TRUE if a valid instance of the ready4 S3 class for institutes lookup table
#' 
#' @rdname ready4show_institutes
#' @export 
is_ready4show_institutes <- function(x) inherits(validate_ready4show_institutes(x), "ready4show_institutes")
