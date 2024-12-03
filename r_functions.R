## Functions needed for the manuscript:
## ------------------------------------

#' Plot with share of texts with at least one of the specific positive words, by
#' gender
#'
#' @param freq_word data frame (tibble) with the count of specific words in
#' each text, plus information on the gender of the responsible applicant
#' @param pos_words the investigated positive words
#'
#' @return Plot with shares
#'
plot_positivewords <- function(freq_word, pos_words) {
  freq_word %>%
    group_by(ResponsibleApplicantGender) %>%
    summarize_at(pos_words, function(x) sum(x > 0, na.rm = TRUE)) %>%
    ungroup() %>%
    gather(key = words, count, novel:inspiring) %>%
    left_join(freq_word %>% 
                count(ResponsibleApplicantGender),
              by = "ResponsibleApplicantGender") %>%
    mutate(prevalence = count * 100 / n) %>%
    ungroup() %>%
    arrange(prevalence) %>% 
    mutate(words = case_when(words == "favorable|favourable" ~
                               "favorable",
                             TRUE ~ words)) %>% 
    ggplot(aes(y = prevalence, x = reorder(words, -prevalence),
               fill = ResponsibleApplicantGender)) +
    geom_bar(position = "dodge", stat = "identity") +
    xlab("") +
    ylab("Prevalence (%)") +
    labs(fill = "Gender") +
    get_datastory_theme() +
    scale_fill_manual(values = get_datastory_scheme(),
                      labels = c("Female", "Male")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) %>% 
    return()
}

#Effect of gender on the use of positive language
#Use of positive language as a response variable
#====================================================
##input: dat = wordfreq :
#' Modeling the effect of gender on the use of positive language represented by
#' a binary variable, for at least one positve word, or the count of positive 
#' words in the text, with confounders age at submission, institution type, and 
#' text length (per 100).
#'
#' @param dat data frame (tibble) with the count of specific words in
#' each text, plus information on the gender of the responsible applicant
#'
#' @return list with five model fits: linear probability model, logistic 
#' regression model, poisson, quasi poisson and negative binomial
#' @export
#'
#' @examples
analyze_gender <- function(dat) {
  
  covariates <- c(
    "ResponsibleApplicantGender",
    "ResponsibleApplicantAgeAtSubmission",
    "ResearchInstitutionType",
    "text_length100"
  )
  
  binary_formula <- as.formula(paste("as.numeric(sum_pos > 0) ~",
                                     paste(covariates, collapse = "+")))
  count_formula <- as.formula(paste("sum_pos ~",
                                   paste(covariates, collapse = "+")))
  
  #likelihood to use at least one positive word
  #-----------------------------
  
  lpmfit <- lm(binary_formula,
               data = dat)
  
  logitfit <- glm(binary_formula,
                  family = binomial(link = "logit"),
                  data = dat)
  
  probitfit <- glm(binary_formula,
                  family = binomial(link = "probit"),
                  data = dat)
  #likelihood to use more positive words
  #-----------------------------
  poissonfit <-
    glm(count_formula,
        family = "poisson",
        data = dat)
  
  quasipoissonfit <-
    glm(count_formula,
        family = "quasipoisson",
        data = dat)
  
  nbfit <-
    MASS::glm.nb(count_formula,
                 data = dat)
  
  reg_results <-
    list(
      linear = lpmfit,
      log = logitfit,
      prob = probitfit,
      count = poissonfit,
      quasicount = quasipoissonfit,
      nbcount = nbfit
    )
  return(reg_results)
}


