genomic_extract <- function(doc) {
  main_df <- paragraph_extract(doc)[1]
  patterns <- c(gene = "(.*)Exon",
                position = "Genomic Coordinates: (.*)V",
                exon = "Exon \\d*") |>
    pattern_match(main_df)
  
  main_df <- data.frame(Gene = patterns[["gene"]],
                        Genomic_Position = patterns[["position"]],
                        Exon = patterns[["exon"]])
  return(main_df)
  
}

primer_extract <- function(doc) {
  primer_df <- paragraph_extract(doc)
  string_test <- stringr::str_subset(primer_df, "For_and")
  
  if (length(string_test) == 0) {
    string_test <- stringr::str_subset(primer_df, "_and_")
    gene <- stringr::str_match(string_test, "(.*)Ex.*_and_")[[2]]
    
  } else {
    gene <- stringr::str_match(string_test, "(.*)Exon\\d*_For")[[2]]
    
    if (is.na(gene)) {
      gene <- stringr::str_match(string_test, "(.*)Exon\\d*For")[[2]]
    }
    
  }
  
  sequence <- string_test |>
    stringr::str_extract_all("[ACTG]* [ACTG]*") |>
    unlist() |>
    stringr::str_trim(side = "left")
  
  primer_df <- data.frame(
    Gene = gene,
    For_Primer_Sequence = sequence[[1]],
    Rev_Primer_Sequence = sequence[[2]]
  )
  
  return(primer_df)
}

#' Document Extract
#' Extract Primer Design word documents for sequence-specific information
#' @param doc The path to the word document containing primer information
#' @return A data-frame with the Gene Name, primer sequences and coordinates
#' @export
#' @examples document_extract("primer_doc.docx)
document_extract <- function(doc) {
  main_df <- genomic_extract(doc)
  primer_df <- primer_extract(doc)
  combined_df <-
    merge(primer_df, main_df, all.x = TRUE, all.y = TRUE)
  
  return(combined_df)
  
}

#' Paragraph Extract
#' Extracts only portions of the word document that are text paragraphs
#' @param doc The path to the word document wanting to be extracted
#' @return A vector made up of the lines of paragraph text from the word document
#' @examples paragraph_extract("word_doc.docx")
paragraph_extract <- function(doc) {
  doc <- doc |>
    officer::read_docx() |>
    officer::docx_summary() |>
    subset(content_type == "paragraph", select = "text") |>
    unlist() |>
    as.vector()
  
}

pattern_match <- function(patterns, string) {
  patterns <- patterns |>
    sapply(\(x) stringr::str_match(string, x)) |>
    sapply(\(x) ifelse(length(x) > 1, x[[2]], x[[1]]))
  
  exon_call <- try(match.arg("exon", names(patterns)),
                   silent = TRUE)
  
  if (class(exon_call) == "character") {
    patterns[[exon_call]] <- gsub("Exon ", "E", patterns[[exon_call]])
  }
  
  return(patterns)
  
}
