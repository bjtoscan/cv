# Regex to locate links in text
find_link <- regex("
  \\[   # Grab opening square bracket
  .+?   # Find smallest internal text as possible
  \\]   # Closing square bracket
  \\(   # Opening parenthesis
  .+?   # Link text, again as small as possible
  \\)   # Closing parenthesis
  ",
                   comments = TRUE
)

# Function that removes links from text and replaces them with superscripts
# referenced in an end-of-document list.
sanitize_links <- function(text){
  if(PDF_EXPORT){
    str_extract_all(text, find_link) %>% 
      pluck(1) %>% 
      walk(function(link_from_text){
        title <- link_from_text %>% str_extract('\\[.+\\]') %>% str_remove_all('\\[|\\]') 
        link <- link_from_text %>% str_extract('\\(.+\\)') %>% str_remove_all('\\(|\\)')
        
        # add link to links array
        links <<- c(links, link)
        
        # Build replacement text
        new_text <- glue('{title}<sup>{length(links)}</sup>')
        
        # Replace text
        text <<- text %>% str_replace(fixed(link_from_text), new_text)
      })
  }
  text
}

# Safe function to remove links from specified columns
strip_links_from_cols <- function(data, cols_to_strip){
  if(nrow(data) == 0) return(data)   # <--- handle empty data safely
  for(i in 1:nrow(data)){
    for(col in cols_to_strip){
      data[i, col] <- sanitize_links(data[i, col])
    }
  }
  data
}

# Print a section of the resume in markdown
print_section <- function(position_data, section_id){
  
  df <- position_data %>%
    filter(section == section_id) %>%
    arrange(desc(end)) %>%
    
    # Pivot description columns into long format
    pivot_longer(
      starts_with('description'),
      names_to = 'description_num',
      values_to = 'description'
    ) %>%
    filter(!is.na(description) | description_num == 'description_1') %>%
    ungroup() %>%
    
    # Assign id safely (handles empty sections)
    mutate(id = if(nrow(.) > 0) 1:n() else integer(0)) %>%
    
    # Group by id to create description lists
    group_by(id) %>%
    mutate(
      descriptions = list(description),
      no_descriptions = is.na(first(description))
    ) %>%
    ungroup() %>%
    
    # Keep only the first description for display
    filter(description_num == 'description_1') %>%
    
    # Create timeline and description bullets
    mutate(
      timeline = ifelse(is.na(start) | start == end,
                        end,
                        glue('{end} - {start}')),
      description_bullets = ifelse(
        no_descriptions,
        ' ',
        map_chr(descriptions, ~paste('-', ., collapse = '\n'))
      )
    ) %>%
    
    # Strip links from text
    strip_links_from_cols(c('title', 'description_bullets')) %>%
    ungroup() %>%
    
    # Replace NAs in all columns except id
    mutate(across(-id, ~ifelse(is.na(.), 'N/A', .)))
  
  # Generate markdown using glue
  glue_data(df,
            "### {title}",
            "\n\n",
            "{loc}",
            "\n\n",
            "{institution}",
            "\n\n",
            "{timeline}", 
            "\n\n",
            "{description_bullets}",
            "\n\n\n"
  )
}

# Construct a bar chart of skills
build_skill_bars <- function(skills, out_of = 5){
  bar_color <- "#969696"
  bar_background <- "#d9d9d9"
  skills %>% 
    mutate(width_percent = round(100*level/out_of)) %>% 
    glue_data(
      "<div class = 'skill-bar'",
      "style = \"background:linear-gradient(to right,",
      "{bar_color} {width_percent}%,",
      "{bar_background} {width_percent}% 100%)\" >",
      "{skill}",
      "</div>"
    )
}
