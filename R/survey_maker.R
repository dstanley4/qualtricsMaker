#' Make a survey
#' @param survey_codebook survey_codebook A dataframe with the question in required format
#' @param response_sep The character for separating response options. Example is semicolon ";". Usage: Yes;No
#' @param filename Advanced Text Format filename. End file name with .txt. This is the file you import into Qualtrics.
#' @return text for import
#' @examples
#' #make_survey
#' @export
make_survey <- function(survey_codebook, response_sep =";", filename = NULL){
  survey <- survey_codebook
  block_names <- unique(survey$block)

  all_blocks <- NULL

  for (cur_block_name in block_names) {
    cur_block_id <- survey$block == cur_block_name
    cur_block <- survey[cur_block_id, ]

    # process block function
    n_block <- dim(cur_block)[1]

    is_block_matrix <- FALSE
    if (cur_block$type[1] == "matrix") {
      is_block_matrix <- TRUE
    }

    if (is_block_matrix == TRUE) {
      # matrix for block
      out_block <- make_matrix_question(cur_block,
                                         response_sep = response_sep)
      out_block <- unlist(out_block)

    } else {
      out_block <- NULL
      for (i in 1:n_block) {
        cur_question <- cur_block[i, ]
        cur_question_type <- cur_question$type[1]

        if (cur_question_type == "MC") {
          out_question <- make_mc_question(cur_question,
                                           response_sep = response_sep)

        } else if (cur_question_type == "MC_horizontal") {
          out_question <- make_mc_question(cur_question,
                                           question_type_text = "MC:SingleAnswer:Horizontal",
                                           response_sep = response_sep)
        } else if (cur_question_type == "MC_multi_horizontal") {
          out_question <- make_mc_question(cur_question,
                                           question_type_text = "MC:MultipleAnswer:Horizontal",
                                           response_sep = response_sep)

        } else if (cur_question_type == "MC_select") {
          out_question <- make_mc_question(cur_question,
                                           question_type_text = "MC:Select",
                                           response_sep = response_sep)

        } else if (cur_question_type == "MC_multi_select") {
          out_question <- make_mc_question(cur_question,
                                           question_type_text = "MC:MultiSelect",
                                           response_sep = response_sep)

        } else if (cur_question_type == "MC_dropdown") {
          out_question <- make_mc_question(cur_question,
                                           question_type_text = "MC:Dropdown",
                                           response_sep = response_sep)
        }

        if (is.null(out_block)) {
          out_block <- unlist(out_question)
        } else {
          out_block <- c(out_block, unlist(out_question))
        }
      }
    } # end else for non-matrix block
    if (is.null(all_blocks)) {
      all_blocks <- c(sprintf("[[Block:%s]]\n\n", cur_block_name))
      all_blocks <- c(all_blocks, out_block)
    } else {
      all_blocks <- c(all_blocks, sprintf("\n\n[[Block:%s]]\n\n", cur_block_name), out_block)
    }
    out_block <- NULL
  } #end loop for blocks

  all_blocks <- c("[[AdvancedFormat]]\n\n", all_blocks)

  if (!is.null(filename)) {
    writeLines(all_blocks, filename)
  }

  return("")
}


