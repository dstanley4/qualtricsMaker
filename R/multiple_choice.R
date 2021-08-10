#' Make a multiple choice question set
#' @param survey_codebook survey_codebook A dataframe with the question in required format
#' @param block_name (optional) Name for a block of questions
#' @param response_sep The character for separating response options. Example is semicolon ";". Usage: Yes;No
#' @param break_after_questions Add a pagebreak after this many questions
#' @return text for import
#' @examples
#' #make_mc
#' @export
make_mc <- function(survey_codebook,
                              block_name = NULL,
                              response_sep = ";",
                              break_after_questions = 0) {

  num_items <- nrow(survey_codebook)

  # convert item name to Qualtrix format for item names
  if (is.null(survey_codebook$item_name)) {
    survey_codebook <- dplyr::mutate(survey_codebook,
                              item_name = paste0("Q",seq(1:num_items)))
  }
  new_item_name <- paste0("[[ID:", survey_codebook$item_name, "]]")
  output_row_numbers <- seq(1, num_items)

  survey_codebook <- dplyr::mutate(survey_codebook,
                            item_name = new_item_name,
                            output_row = output_row_numbers)


  if (break_after_questions>0) {
    pagebreak_row_numbers <- seq(break_after_questions, num_items, by = break_after_questions) + .5
    num_pagebreaks <- length(pagebreak_row_numbers)
    pagebreaks_row_range <- c((num_items+1):(num_items+num_pagebreaks))
    survey_codebook[pagebreaks_row_range, ] <- NA #add rows for pagebreaks
    survey_codebook$item_text[pagebreaks_row_range] <- "[[PageBreak]]\n"
    survey_codebook$output_row[pagebreaks_row_range] <- pagebreak_row_numbers
    survey_codebook <- dplyr::arrange(survey_codebook, output_row)
  }

  survey_codebook <- dplyr::select(survey_codebook, item_name, item_text, response_options, output_row)


  #use select to put columns in right order prior to pivot_longer
  survey_codebook_long <- tidyr::pivot_longer(survey_codebook, cols = !output_row,
                                       names_to = "text_type",
                                       values_to = "text")
  id_na <- is.na(survey_codebook_long$text)
  survey_codebook_long$text[id_na] <- ""

  # add MC question in front of question ID
  id_item_name_rows <- which(survey_codebook_long$text_type == "item_name" & survey_codebook_long$text != "")
  item_name_vector <- survey_codebook_long$text[id_item_name_rows]
  item_name_vector <- paste0("\n\n[[Question:MC]]", "\n", item_name_vector)
  survey_codebook_long$text[id_item_name_rows] <- item_name_vector

  # add "Choices before response options
  id_response_options_rows <- which(survey_codebook_long$text_type == "response_options" & survey_codebook_long$text != "")
  item_reponse_options_vector <- survey_codebook_long$text[id_response_options_rows]
  item_reponse_options_vector <- paste0("[[Choices]]", "\n", item_reponse_options_vector)
  survey_codebook_long$text[id_response_options_rows] <- item_reponse_options_vector

  # break apart response options - side effect: turn df into nested list
  survey_codebook_long <- strsplit(survey_codebook_long$text, response_sep)

  # turn list into lines of text
  survey_codebook_long <- unlist(survey_codebook_long)

  # add this at front (DO SAME APPROACH FOR BLOCK NAME) instead of this ...do advanced format in write command...
  #survey_codebook_long[1] <- paste("[[AdvancedFormat]]\n", survey_codebook_long[1], sep = "\n")
  if (!is.null(block_name)) {
    block_name_text <- sprintf("[[%s]]\n", block_name)
    survey_codebook_long[1] <- paste(block_name_text, survey_codebook_long[1], sep = "\n")
  }


  return(survey_codebook_long)
}


make_mc_question <- function(survey_codebook_row,
                    response_sep = ";") {

  item_name <- survey_codebook_row$item_name[1]
  item_name_str <- sprintf("\n\n[[Question:MC]]\n[[ID:%s]]", item_name)

  item_text_str <- survey_codebook_row$item_text[1]

  item_response_options <- survey_codebook_row$response_options[1]

  question_as_list <- list()

  question_as_list[[1]] <- item_name_str

  question_as_list[[2]] <- item_text_str

  item_response_options <- paste0("[[Choices]]\n",item_response_options)
  question_as_list[[3]] <- strsplit(item_response_options, response_sep)

 return(question_as_list)
}

# [[Question:Matrix]]
# This question is a matrix question.
#
# It has lots of question text on multiple lines and uses
# advanced answers.
#
# [[Choices]]
# statement a
# statement b
# statement c
# [[AdvancedAnswers]]
# [[Answer]]
# answer 1
# [[Answer]]
# answer 2
# [[Answer]]
# answer 3

