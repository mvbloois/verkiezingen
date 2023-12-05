read_total_votes <- function(filename) {
  gemeente <- str_extract(str_remove(filename, ".csv"), "[:alpha:]+$")
  
  gn <- str_pad(parse_number(readLines(filename, n = 4)[4]), 4, "left", pad = "0")
  
  read_csv2(filename,
            skip = 5) %>%  
    filter(! is.na(Aanduiding)) %>% 
    filter(! is.na(Lijstnummer)) %>% 
    mutate(gemnaam = str_to_title(gemeente),
           gemcode = gn) %>% 
    select(lijst = Lijstnummer, partij = Aanduiding, gemnaam, stemmen = Totaal, gemcode) 
}

read_osv_file <- function(filename) {
  df <- read_csv2(filename,
                  skip = 5)
  
  df %>% 
    filter(!is.na(Aanduiding)) %>% 
    filter(!is.na(Lijstnummer)) %>% 
    select(-Volgnummer, -`Naam kandidaat`, -Totaal) %>%  
    pivot_longer(cols = 3:last_col(),
                 values_transform = as.integer) %>% 
    select(lijst = Lijstnummer, partij = Aanduiding, gemnaam = name, stemmen = value) %>% 
    inner_join(
      df %>% 
        slice_head(n = 1) %>% 
        select(6:last_col()) %>% 
        t() %>% 
        as.data.frame() %>% 
        rownames_to_column("gemnaam") %>% 
        rename(gemcode = V1) %>% 
        mutate(gemcode = str_pad(gemcode, 4, "left", pad = "0")),
      by = join_by(gemnaam)
    )
}
