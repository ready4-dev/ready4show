## ----echo = F-----------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----message=FALSE------------------------------------------------------------
library(ready4)
library(ready4show)
library(bookdown)
library(rticles)

## -----------------------------------------------------------------------------
X <- Ready4showSynopsis(background_1L_chr = "Our study is entirely fictional.",
                        coi_1L_chr = "None declared.",
                        conclusion_1L_chr = "These fake results are not interesting.",
                        digits_int = 3L,
                        ethics_1L_chr = "The study was reviewed and granted approval by Awesome University's Human Research Ethics Committee (1111111.1).",
                        funding_1L_chr = "The study was funded by Generous Benefactor.",
                        interval_chr = "three months",
                        keywords_chr = c("entirely","fake","do", "not","cite"),
                        outp_formats_chr = "PDF",
                        sample_desc_1L_chr = "The study sample is fake data that pretends to be young people aged 12 to 25 years who attended Australian primary care services for mental health related needs between November 2019 to August 2020.",
                        title_1L_chr = "A hypothetical study using fake data")

## -----------------------------------------------------------------------------
exhibitSlot(X,
            "authors_r3") 

## -----------------------------------------------------------------------------
X <- renewSlot(X,
          "authors_r3",
          first_nm_chr = "Alejandra",
          middle_nm_chr = "Rocio",
          last_nm_chr = "Scienceace",
          title_chr = "Dr",
          qualifications_chr = "MD, PhD",
          institute_chr = "Institute_A, Institute_B",
          sequence_int = 1,
          is_corresponding_lgl = T,
          email_chr = "fake_email@fake_institute.com") %>%
  renewSlot("authors_r3",
            first_nm_chr = "Fionn",
            middle_nm_chr = "Seamus",
            last_nm_chr = "Researchchamp",
            title_chr = "Prof",
            qualifications_chr = "MSc, PhD",
            institute_chr = "Institute_C, Institute_B",
            sequence_int = 2,
            email_chr = "fake_email@unreal_institute.com") 


## -----------------------------------------------------------------------------
X %>%
  exhibitSlot("authors_r3") 

## -----------------------------------------------------------------------------
X <- renewSlot(X,
          "institutes_r3",
          short_name_chr = "Institute_A", 
          long_name_chr = "Awesome University, Shanghai") %>%
  renewSlot("institutes_r3",
            short_name_chr = "Institute_B", 
            long_name_chr = "August Institution, London") %>%
  renewSlot("institutes_r3",
            new_val_xx = "use_renew_mthd",
            short_name_chr = "Institute_C", 
            long_name_chr = "Highly Ranked Uni, Montreal")

## -----------------------------------------------------------------------------
X %>%
  exhibitSlot("institutes_r3") 

## -----------------------------------------------------------------------------
X <- renewSlot(X,
               "correspondences_r3",
               old_nms_chr = c("PHQ9", "GAD7"),
               new_nms_chr = c("PHQ-9", "GAD-7"))

## -----------------------------------------------------------------------------
X %>%
  exhibitSlot("correspondences_r3") # Add Exhibit Method

## -----------------------------------------------------------------------------
X <- renewSlot(X,
               "a_Ready4showPaths@outp_data_dir_1L_chr",
               new_val_xx = tempdir())

## -----------------------------------------------------------------------------
## Not run
# procureSlot(X,
#             "a_Ready4showPaths@mkdn_source_dir_1L_chr",
#             new_val_xx  = "PATH TO MARKDOWN DATASET")

## ----echo=FALSE---------------------------------------------------------------
# This section is only required when rendering vignettes (it replaces interactivity).
file_fl <- file()
write(paste(c("Y","Y","Y","Y","Y","Y"), collapse = "\n"), file_fl)
options("prompt_opts.con" = file_fl)

## ----message=FALSE, results='hide', warning=FALSE-----------------------------
authorData(X)

## ----eval = F, message=FALSE, results='hide', warning=FALSE-------------------
#  authorReport(X)

## ----eval = F, message=FALSE, results='hide', warning=FALSE-------------------
#  renewSlot(X,
#            "outp_formats_chr",
#            new_val_xx = "Word") %>%
#    authorReport()

