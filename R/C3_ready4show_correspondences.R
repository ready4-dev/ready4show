
setOldClass(c("ready4show_correspondences","tbl_df", "tbl", "data.frame"))
#' Name correspondences lookup table
#' @description Create a new valid instance of the Name correspondences lookup table
#' @param x A prototype for the Name correspondences lookup table, Default: make_pt_ready4show_correspondences()
#' @return A validated instance of the Name correspondences lookup table
#' @details Name correspondences lookup table
#' @rdname ready4show_correspondences
#' @export 
ready4show_correspondences <- function(x = make_pt_ready4show_correspondences()){ 
validate_ready4show_correspondences(make_new_ready4show_correspondences(x))
}
#' make new ready4show correspondences Name correspondences lookup table
#' @description Create a new unvalidated instance of the Name correspondences lookup table
#' @param x A prototype for the Name correspondences lookup table
#' @return An unvalidated instance of the Name correspondences lookup table
#' @details Name correspondences lookup table
#' @rdname make_new_ready4show_correspondences
#' @export 
#' @importFrom tibble is_tibble
#' @keywords internal
make_new_ready4show_correspondences <- function(x){ 
stopifnot(tibble::is_tibble(x))
class(x) <- append(c("ready4show_correspondences",setdiff(make_pt_ready4show_correspondences() %>% class(),class(x))),
class(x))
x
}
#' make prototype ready4show correspondences Name correspondences lookup table
#' @param old_nms_chr Old names (a character vector), Default: character(0)
#' @param new_nms_chr New names (a character vector), Default: character(0)
#' @return A prototype for Name correspondences lookup table
#' 
#' @rdname ready4show_correspondences
#' @export 
#' @importFrom ready4 update_pt_fn_args_ls
#' @importFrom rlang exec
#' @importFrom tibble tibble
make_pt_ready4show_correspondences <- function(old_nms_chr = character(0),
new_nms_chr = character(0)){ 
args_ls <- list(old_nms_chr = old_nms_chr,
new_nms_chr = new_nms_chr) %>% ready4::update_pt_fn_args_ls()
rlang::exec(tibble::tibble,!!!args_ls)
}
#' validate ready4show correspondences Name correspondences lookup table
#' @description Validate an instance of the Name correspondences lookup table
#' @param x An unvalidated instance of the Name correspondences lookup table
#' @return A prototpe for Name correspondences lookup table
#' @details Name correspondences lookup table
#' @rdname validate_ready4show_correspondences
#' @export 
#' @importFrom stringr str_detect str_c
#' @importFrom dplyr summarise_all filter arrange pull
#' @importFrom tidyr gather
#' @importFrom purrr map_chr map2_chr
#' @keywords internal
validate_ready4show_correspondences <- function(x){
if(sum(stringr::str_detect(names(x)[names(x) %in% names(make_pt_ready4show_correspondences())],
names(make_pt_ready4show_correspondences())))!=length(names(make_pt_ready4show_correspondences()))){
stop(paste0("TIBBLE must include columns named: ",
names(make_pt_ready4show_correspondences()) %>% stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}

 if(!identical(make_pt_ready4show_correspondences() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
 dplyr::filter(!is.na(class)) %>% 
dplyr::arrange(variable),
x %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
 dplyr::filter(!is.na(class)) %>% 
dplyr::filter(variable %in% names(make_pt_ready4show_correspondences())) %>% dplyr::arrange(variable))){
stop(paste0("TIBBLE columns should be of the following classes: ",
"",
{
class_lup <- make_pt_ready4show_correspondences() %>% 
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
#' is ready4show correspondences Name correspondences lookup table
#' @param x An object of any type
#' @return A logical value, TRUE if a valid instance of the Name correspondences lookup table
#' 
#' @rdname ready4show_correspondences
#' @export 
is_ready4show_correspondences <- function(x) inherits(validate_ready4show_correspondences(x), "ready4show_correspondences")
