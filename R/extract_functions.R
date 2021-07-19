genomic_extract <- function(doc) {
  main_df <- paragraph_extract(doc)[1]
  gene_name <- stringr::str_match(main_df, "(.*)Exon")[[2]]
  position <-
    stringr::str_match(main_df, "Genomic Coordinates: (.*)V")[[2]]
  exon <- stringr::str_extract(main_df, "Exon \\d*")
  main_df <- data.frame(Gene = gene_name,
                        Genomic_Position = position,
                        Exon = exon)
  main_df[["Exon"]] <- gsub("Exon ", "E", main_df[["Exon"]])
  return(main_df)
  
}

primer_extract <- function(doc) {
  primer_df <- paragraph_extract(doc)
  string_test <- stringr::str_subset(primer_df, "For_and")
  
  if (length(string_test) == 0) {
    string_test <- str_subset(primer_df, "_and_")
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

document_extract <- function(doc) {
  main_df <- genomic_extract(doc)
  primer_df <- primer_extract(doc)
  combined_df <- dplyr::full_join(primer_df, main_df, by = "Gene")
  return(combined_df)
  
}

#' Paragraph Extract
#' Extracts only portions of the word document that are text paragraphs
#' @param doc The path to the word document wanting to be extracted
#' @return A vector made up of the lines of paragraph text from the word document
#' @export
#' @examples paragraph_extract("word_doc.docx")
paragraph_extract <- function(doc) {
  
  doc <- doc |>
    officer::read_docx() |>
    officer::docx_summary() |>
    subset(content_type == "paragraph", select = "text") |>
    unlist() |>
    as.vector()
  
}
