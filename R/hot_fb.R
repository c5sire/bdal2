# Get fieldbook
# get path to hidapdb
# get path to active (crop / dictionary)
# Get dictionary
# Get dic-subset by fb
# format fb (including readonly & fixed columns; option: display: fulltext column labels)

DF = data.frame(val = seq(1, 1.9, .1), bool = TRUE, big = LETTERS[1:10],
                small = letters[1:10],
                dt = seq(from = Sys.Date(), by = "days", length.out = 10),
                stringsAsFactors = F)


rhandsontable(DF, rowHeaders = NULL) %>%
  hot_col(col = "val", type = 'numeric', format = "#.##") %>%
  hot_col(col = "big", type = "dropdown", source = LETTERS) %>%
  hot_col(col = "small", type = "autocomplete", source = letters,
          strict = FALSE)
