
setOldClass(c("ready4show_authors","tbl_df", "tbl", "data.frame"))
#' ready4 S3 class for authors lookup table
#' @description Create a new valid instance of the ready4 S3 class for authors lookup table
#' @param x A prototype for the ready4 S3 class for authors lookup table, Default: make_pt_ready4show_authors()
#' @return A validated instance of the ready4 S3 class for authors lookup table
#' @details ready4 S3 class for authors lookup table
#' @rdname ready4show_authors
#' @export 
ready4show_authors <- function(x = make_pt_ready4show_authors()){ 
validate_ready4show_authors(make_new_ready4show_authors(x))
}
#' make new ready4show authors ready4 S3 class for authors lookup table
#' @description Create a new unvalidated instance of the ready4 S3 class for authors lookup table
#' @param x A prototype for the ready4 S3 class for authors lookup table
#' @return An unvalidated instance of the ready4 S3 class for authors lookup table
#' @details ready4 S3 class for authors lookup table
#' @rdname make_new_ready4show_authors
#' @export 
#' @importFrom tibble is_tibble
#' @keywords internal
make_new_ready4show_authors <- function(x){ 
stopifnot(tibble::is_tibble(x))
class(x) <- append(c("ready4show_authors",setdiff(make_pt_ready4show_authors() %>% class(),class(x))),
class(x))
x
}
#' make prototype ready4show authors ready4 S3 class for authors lookup table
#' @param first_nm_chr First name (a character vector), Default: character(0)
#' @param middle_nm_chr Middle name (a character vector), Default: character(0)
#' @param last_nm_chr Last name (a character vector), Default: character(0)
#' @param title_chr Title (a character vector), Default: character(0)
#' @param qualifications_chr Qualifications (a character vector), Default: character(0)
#' @param institute_chr Institute (a character vector), Default: character(0)
#' @param sequence_int Sequence (an integer vector), Default: integer(0)
#' @param is_corresponding_lgl Is corresponding (a logical vector), Default: logical(0)
#' @param email_chr Email (a character vector), Default: character(0)
#' @param is_equal_first_lgl Is equal first (a logical vector), Default: logical(0)
#' @return A prototype for ready4 S3 class for authors lookup table
#' 
#' @rdname ready4show_authors
#' @export 
#' @importFrom ready4 update_pt_fn_args_ls
#' @importFrom rlang exec
#' @importFrom tibble tibble
make_pt_ready4show_authors <- function(first_nm_chr = character(0),
middle_nm_chr = character(0),
last_nm_chr = character(0),
title_chr = character(0),
qualifications_chr = character(0),
institute_chr = character(0),
sequence_int = integer(0),
is_corresponding_lgl = logical(0),
email_chr = character(0),
is_equal_first_lgl = logical(0)){ 
args_ls <- list(first_nm_chr = first_nm_chr,
middle_nm_chr = middle_nm_chr,
last_nm_chr = last_nm_chr,
title_chr = title_chr,
qualifications_chr = qualifications_chr,
institute_chr = institute_chr,
sequence_int = sequence_int,
is_corresponding_lgl = is_corresponding_lgl,
email_chr = email_chr,
is_equal_first_lgl = is_equal_first_lgl) %>% ready4::update_pt_fn_args_ls()
rlang::exec(tibble::tibble,!!!args_ls)
}
#' validate ready4show authors ready4 S3 class for authors lookup table
#' @description Validate an instance of the ready4 S3 class for authors lookup table
#' @param x An unvalidated instance of the ready4 S3 class for authors lookup table
#' @return A prototpe for ready4 S3 class for authors lookup table
#' @details ready4 S3 class for authors lookup table
#' @rdname validate_ready4show_authors
#' @export 
#' @importFrom stringr str_detect str_c
#' @importFrom dplyr summarise_all filter arrange pull
#' @importFrom tidyr gather
#' @importFrom purrr map_chr map2_chr
#' @keywords internal
validate_ready4show_authors <- function(x){
if(sum(stringr::str_detect(names(x)[names(x) %in% names(make_pt_ready4show_authors())],
names(make_pt_ready4show_authors())))!=length(names(make_pt_ready4show_authors()))){
stop(paste0("TIBBLE must include columns named: ",
names(make_pt_ready4show_authors()) %>% stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}

 if(!identical(make_pt_ready4show_authors() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
 dplyr::filter(!is.na(class)) %>% 
dplyr::arrange(variable),
x %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
 dplyr::filter(!is.na(class)) %>% 
dplyr::filter(variable %in% names(make_pt_ready4show_authors())) %>% dplyr::arrange(variable))){
stop(paste0("TIBBLE columns should be of the following classes: ",
"",
{
class_lup <- make_pt_ready4show_authors() %>% 
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
#' is ready4show authors ready4 S3 class for authors lookup table
#' @param x An object of any type
#' @return A logical value, TRUE if a valid instance of the ready4 S3 class for authors lookup table
#' 
#' @rdname ready4show_authors
#' @export 
is_ready4show_authors <- function(x) inherits(validate_ready4show_authors(x), "ready4show_authors")
