make_mc_question <- function(survey_codebook_row,
                             question_type_text = "MC",
                             response_sep = ";") {

  item_name <- survey_codebook_row$item_name[1]
  item_name_str <- sprintf("\n\n[[Question:%s]]\n[[ID:%s]]", question_type_text, item_name)

  item_text_str <- survey_codebook_row$item_text[1]

  item_response_options <- survey_codebook_row$response_options[1]

  question_as_list <- list()

  question_as_list[[1]] <- item_name_str

  question_as_list[[2]] <- item_text_str

  item_response_options <- paste0("[[Choices]]\n",item_response_options)
  question_as_list[[3]] <- strsplit(item_response_options, response_sep)

 return(question_as_list)
}

