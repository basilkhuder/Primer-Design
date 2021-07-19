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

paragraph_extract <- function(doc) {
  doc <- officer::read_docx(doc)
  doc <- officer::docx_summary(doc)
  doc <- doc[doc["content_type"] == "paragraph",]
  doc <- doc[["text"]]
  return(doc)
  
}
