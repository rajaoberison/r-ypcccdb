#' First Commit to CCAM Database
#'
#' Use only once to add exisitng CCAM data at the creation of the database.
#'
#' @name first_commit_ccam
#'
#' @param ccam_file Path to the .rda file of the exisitng CCAM data
#' @param ccam_label_file Path to the .csv file containing the labels for the CCAM variables
#'
#' @return None
#'
#' @import dplyr
#' @import tibble
#' @import DBI
#' @import tidyr
#' @import stringr
#'
#' @export


first_commit_ccam <- function(ccam_file, ccam_label_file, censustract_path, cd116_path, stateleglower_path, statelegupper_path, place_path) {


  load(file = ccam_file)


  ## Table: individual_person
  pid <- unique(as.vector(megapoll$CaseID))
  individual_person <- tibble(
    'pid' = pid
  )
  rm(pid)
  ## Updating database: individual_person
  individual_person <- add_rows_to_table(con, 'individual_person', '(pid)', individual_person)


  ## Table: survey
  cat("Writing survey\n")
  wave <- unique(as.vector(megapoll$wave))
  year <- rep(NA, length(wave))
  for (i in 1:length(wave)) {
    year[i] <- as.integer(trimws(unlist(strsplit(wave[i], " "))[2]))
  }
  survey <- tibble(
    'source' = 'CCAM',
    'wave' = wave,
    'year' = year
  )
  rm(wave, year, i)

  ## Updating database: survey
  survey <- add_rows_to_table(con, 'survey', '(source, wave, year)', survey)


  ## Table: individual_respondent
  cat("Writing respondent\n")
  tmp_irespondent <- select(megapoll, CaseID, wave, weight, latitude_comb, longitude_comb)
  tmp_irespondent1 <- tmp_irespondent %>%
    inner_join(survey, by = "wave") %>%
    mutate(CaseID = trimws(as.character(CaseID))) %>%
    inner_join(individual_person, by = c("CaseID"="pid"))

  tmp_irespondent2 <- select(
    tmp_irespondent1,
    survey_id, id, weight, latitude_comb, longitude_comb
  )
  names(tmp_irespondent2) <- c(
    "survey_id", "person_id", "weight", "lat", "long"
  )

  individual_respondent <- tmp_irespondent2 %>%
    add_geo_unit(censustract_path, 'tract') %>%
    add_geo_unit(cd116_path, 'cd116') %>%
    add_geo_unit(stateleglower_path, 'sldl') %>%
    add_geo_unit(statelegupper_path, 'sldu') %>%
    add_geo_unit(place_path, 'place')

  rm(tmp_irespondent, tmp_irespondent1, tmp_irespondent2)

  ## Updating database: individual_respondent
  individual_respondent <- add_rows_to_table(con, 'individual_respondent', '(survey_id, person_id, `weight`, `lat`, `long`, tract, cd116, sldl, sldu, place)', individual_respondent)


  ## Table: variables
  cat("Writing variables\n")
  df.variables <- attributes(megapoll)
  # variables.text <- gsub("'", "", as.vector(trimws(df.variables$variable.labels)))
  # variables.name <- unique(trimws(names(df.variables$variable.labels)))
  # variables.type <- sapply(select(megapoll, one_of(variables.name)), class)
  variable.text0 <- as.vector(trimws(unlist(lapply(megapoll, function(x) {
    if (!is.null(attr(x, 'label'))){ return(attr(x, 'label')) } else {return(NA)} } ))))
  variable.text1 <- gsub("^'", "", variable.text0)
  variable.text2 <- gsub("'$", "", variable.text1)
  variables.text <- gsub("'", "''", variable.text2)
  variables.name <- unique(trimws(df.variables$names))
  variables.type <- sapply(select(megapoll, one_of(variables.name)), class)

  # # Not all variables were selected
  # all.variables <- df.variables$names
  # variables_without_attributes <- all.variables[!(all.variables %in% variables.name)]

  ### For variables that have labels
  ycom.map.parameters <-  read.csv(ccam_label_file)
  ycom.shortlabels <- ycom.map.parameters %>%
    group_by(qid) %>%
    summarise(all.shortlabels=paste0(shortlabel, collapse = ";"))
  variables.label0 <- tibble('qid'=variables.name) %>%
    left_join(select(ycom.shortlabels, qid, all.shortlabels), by="qid")
  variables.label <- gsub("'", "", variables.label0$all.shortlabels)

  variables0 <- tibble(
    'variable_name' = variables.name,
    'variable_label' = variables.label,
    'variable_type' = variables.type,
    'variable_text' = variables.text
  )
  variables <- dplyr::filter(variables0, !grepl("(?i)wave|(?i)weight|(?i)lat|(?i)lon|(?i)repeat|(?i)date", variable_name))
  rm(df.variables, variables.type, variables.text, variables.name, ycom.map.parameters, ycom.shortlabels, variables.label0, variables.label, variables0)

  ## Updating database: variables
  variables <- add_rows_to_table(con, 'variables', '(variable_name, variable_label, variable_type, variable_text)', variables)


  ## Table: individual_response
  cat("Writing responses\n")
  df.megapoll.numeric <- select(megapoll, CaseID, one_of(variables$variable_name[variables$variable_type=="numeric"]))
  df.megapoll.factor <- select(megapoll, CaseID, one_of(variables$variable_name[variables$variable_type=="factor"]))

  # Old: gather()
  df.numeric.responses0 <- gather(
    df.megapoll.numeric,
    "varname", "response", -CaseID
  )
  df.factor.responses0 <- gather(
    df.megapoll.factor,
    "varname", "response", -CaseID
  )
  df.factor.responses0$response <- gsub("\\", "", as.character(df.factor.responses0$response), fixed=T)
  df.factor.responses0$response <- str_replace_all(as.character(df.factor.responses0$response), "\\\\", "")
  df.factor.responses0$response <- gsub("^'", "", as.character(df.factor.responses0$response))
  df.factor.responses0$response <- gsub("'$", "", as.character(df.factor.responses0$response))
  df.factor.responses0$response <- gsub("'", "''", as.character(df.factor.responses0$response))
  df.factor.responses0$response <- gsub('"', '', as.character(df.factor.responses0$response))
  df.factor.responses0$response <- trimws(df.factor.responses0$response)

  # Newer implementation but slow
  # df.numeric.responses0 <- tidyr::pivot_longer(
  #   df.megapoll.numeric,
  #   cols = -CaseID,
  #   names_to = "varname",
  #   values_to = "response"
  # )
  # df.factor.responses0 <- tidyr::pivot_longer(
  #   df.megapoll.factor,
  #   cols = -CaseID,
  #   names_to = "varname",
  #   values_to = "response"
  # )

  df.numeric.responses <- df.numeric.responses0 %>%
    mutate(CaseID = trimws(as.character(CaseID))) %>%
    inner_join(individual_person, by=c("CaseID"="pid")) %>%
    inner_join(individual_respondent, by=c("id"="person_id")) %>%
    inner_join(variables, by=c("varname"="variable_name"))

  df.factor.responses <- df.factor.responses0 %>%
    mutate(CaseID = trimws(as.character(CaseID))) %>%
    inner_join(individual_person, by=c("CaseID"="pid")) %>%
    inner_join(individual_respondent, by=c("id"="person_id")) %>%
    inner_join(variables, by=c("varname"="variable_name"))

  individual_response.numeric <- tibble(
    'survey_id' = df.numeric.responses$survey_id,
    'respondent_id' = df.numeric.responses$respondent_id,
    'variable_id' = df.numeric.responses$variable_id,
    'response' = df.numeric.responses$response
  )
  individual_response.factor <- tibble(
    'survey_id' = df.factor.responses$survey_id,
    'respondent_id' = df.factor.responses$respondent_id,
    'variable_id' = df.factor.responses$variable_id,
    'response' = df.factor.responses$response
  )

  rm(df.megapoll.numeric, df.megapoll.factor, df.numeric.responses0, df.factor.responses0, df.numeric.responses, df.factor.responses)

  ## Updating database: individual_response_numeric
  cat("Writing numeric responses\n")
  n <- 1000
  nr <- nrow(individual_response.numeric)
  individual_response.numeric_chunks <- split(individual_response.numeric, rep(1:ceiling(nr/n), each=n, length.out=nr))
  i=1
  n_chunk <- length(individual_response.numeric_chunks)

  for (a_chunk in individual_response.numeric_chunks){
    cat(paste("Writing chunk", i, "of", n_chunk, "\n"))
    individual_response.numeric <- add_rows_to_table(con, 'individual_response_numeric', '(survey_id, respondent_id, variable_id, response)', as_tibble(a_chunk))
    i=i+1
  }
  # individual_response.numeric <- add_rows_to_table(con, 'individual_response_numeric', '(survey_id, respondent_id, variable_id, response)', individual_response.numeric)

  ## Updating database: individual_response_factor
  cat("Writing factor responses\n")
  n <- 1000
  nr <- nrow(individual_response.factor)
  individual_response.factor_chunks <- split(individual_response.factor, rep(1:ceiling(nr/n), each=n, length.out=nr))
  i=1
  n_chunk <- length(individual_response.factor_chunks)

  for (a_chunk in individual_response.factor_chunks){
    cat(paste("Writing chunk", i, "of", n_chunk, "\n"))
    individual_response.factor <- add_rows_to_table(con, 'individual_response_factor', '(survey_id, respondent_id, variable_id, response)', as_tibble(a_chunk))
    i=i+1
  }

}
