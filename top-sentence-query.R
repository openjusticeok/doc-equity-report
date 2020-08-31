library(ojodb)

# Database query for all prison sentences since FY 2010
connect_ojo()

sd <- ojo_tbl("doc_sentences") %>%
  filter(js_date >= "2009-07-01", # Don't use ymd() here! Database understands the string
             !is.na(doc_incarcerated_term_yrs)) %>%
  collect()

disconnect_ojo()

# Get fiscal year, group by doc_num, county, and fy, and take the sentence with the longest incarceration sentence
sents <- sd %>%
  mutate(doc_num = as.integer(doc_num),
         fy = date_to_fy(js_date),
         county = str_remove(doc_sentencing_county, " COUNTY.*") %>%
           str_trim) %>%
  group_by(doc_num, county, fy) %>%
  arrange(desc(doc_incarcerated_term_yrs)) %>%
  slice(1)
