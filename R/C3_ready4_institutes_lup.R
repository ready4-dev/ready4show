
setOldClass(c("ready4_institutes_lup","tbl_df", "tbl", "data.frame"))
#' ready4show S3 class for institutes lookup table
#' @description Create a new valid instance of the ready4show S3 class for institutes lookup table
#' @param x A prototype for the ready4show S3 class for institutes lookup table, Default: make_pt_ready4_institutes_lup()
#' @return A validated instance of the ready4show S3 class for institutes lookup table
#' @details ready4show S3 class for institutes lookup table
#' @rdname ready4_institutes_lup
#' @export 

ready4_institutes_lup <- function(x = make_pt_ready4_institutes_lup()){ 
validate_ready4_institutes_lup(make_new_ready4_institutes_lup(x))
}
#' Make new ready4show S3 class for institutes lookup table
#' @description Create a new unvalidated instance of the ready4show S3 class for institutes lookup table
#' @param x A prototype for the ready4show S3 class for institutes lookup table
#' @return An unvalidated instance of the ready4show S3 class for institutes lookup table
#' @details ready4show S3 class for institutes lookup table
#' @rdname make_new_ready4_institutes_lup
#' @export 
#' @importFrom tibble is_tibble
make_new_ready4_institutes_lup <- function(x){ 
stopifnot(tibble::is_tibble(x))
class(x) <- append(c("ready4_institutes_lup",setdiff(make_pt_ready4_institutes_lup() %>% class(),class(x))),
class(x))
x
}
#' Make prototype ready4show S3 class for institutes lookup table
#' @description Create a new prototype for the ready4show S3 class for institutes lookup table
#' @param short_name_chr Short name (a character vector), Default: character(0)
#' @param long_name_chr Long name (a character vector), Default: character(0)
#' @return A prototype for ready4show S3 class for institutes lookup table
#' @details ready4show S3 class for institutes lookup table
#' @rdname make_pt_ready4_institutes_lup
#' @export 
#' @importFrom ready4class update_pt_fn_args_ls
#' @importFrom rlang exec
#' @importFrom tibble tibble
make_pt_ready4_institutes_lup <- function(short_name_chr = character(0),
long_name_chr = character(0)){ 
args_ls <- list(short_name_chr = short_name_chr,
long_name_chr = long_name_chr) %>% ready4class::update_pt_fn_args_ls()
rlang::exec(tibble::tibble,!!!args_ls)
}
#' Validate ready4show S3 class for institutes lookup table
#' @description Validate an instance of the ready4show S3 class for institutes lookup table
#' @param x An unvalidated instance of the ready4show S3 class for institutes lookup table
#' @return A prototpe for ready4show S3 class for institutes lookup table
#' @details ready4show S3 class for institutes lookup table
#' @rdname validate_ready4_institutes_lup
#' @export 
#' @importFrom stringr str_detect str_c
#' @importFrom dplyr summarise_all arrange filter pull
#' @importFrom tidyr gather
#' @importFrom purrr map2_chr
validate_ready4_institutes_lup <- function(x){
if(sum(stringr::str_detect(names(x)[names(x) %in% names(make_pt_ready4_institutes_lup())],
names(make_pt_ready4_institutes_lup())))!=length(names(make_pt_ready4_institutes_lup()))){
stop(paste0("TIBBLE must include columns named: ",
names(make_pt_ready4_institutes_lup()) %>% stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}

 if(!identical(make_pt_ready4_institutes_lup() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
dplyr::arrange(variable),
x %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
dplyr::filter(variable %in% names(make_pt_ready4_institutes_lup())) %>% dplyr::arrange(variable))){
stop(paste0("TIBBLE columns should be of the following classes: ",
purrr::map2_chr(make_pt_ready4_institutes_lup() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
dplyr::pull(1),
 make_pt_ready4_institutes_lup() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
dplyr::pull(2),
 ~ paste0(.x,": ",.y)) %>% 
stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}

x}
#' Is ready4show S3 class for institutes lookup table
#' @description Check whether an object is a valid instance of the ready4show S3 class for institutes lookup table
#' @param x An object of any type
#' @return A logical value, TRUE if a valid instance of the ready4show S3 class for institutes lookup table
#' @details ready4show S3 class for institutes lookup table
#' @rdname is_ready4_institutes_lup
#' @export 

is_ready4_institutes_lup <- function(x) inherits(validate_ready4_institutes_lup(x), "ready4_institutes_lup")
