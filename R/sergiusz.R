

sergiusz <- function() {

  # print(str(getActiveDocumentContext()))

  selected_text = getActiveDocumentContext()$selection[[1]]$text

  print(head(eval(parse(text = selected_text))))

}
