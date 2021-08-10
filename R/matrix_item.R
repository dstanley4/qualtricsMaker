make_matrix_question <- function(survey_codebook_rows,
                                 response_sep = ";") {

  matrix_prefix <- "\n\n[[Question:Matrix]]"
  if (!is.null(survey_codebook_rows$matrix_text)) {
    matrix_text <- survey_codebook_rows$matrix_text[1]
  } else {
    matrix_text <- ""
  }

  item_text_prefix <- "[[Choices]]"
  item_text <- dplyr::pull(survey_codebook_rows, item_text)

  item_response_options_prefix <- "[[AdvancedAnswers]]"
  item_response_options <- strsplit(survey_codebook_rows$response_options[1], response_sep)
  orig_response <- item_response_options[[1]]
  response_with_answer_str <- paste("[[Answer]]\n", orig_response, sep = "")
  item_response_options[[1]] <- response_with_answer_str

  question_as_list <- list()
  question_as_list[[1]] <- matrix_prefix
  question_as_list[[2]] <- matrix_text
  question_as_list[[3]] <- item_text_prefix
  question_as_list[[4]] <- item_text
  question_as_list[[5]] <- item_response_options

  return(question_as_list)
}
