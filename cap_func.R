create_caption <- function(
  source,
  day,
  prompt,
  proj,
  year = 2026,
  src_txt = "Source",
  base_map = NULL,
  note = NULL
) {
  glue::glue(
    "{if (!is.null(note)) paste0('**Note**: ', note, '<br>')}",
    "**{src_txt}**: {source}<br>",
    "{if (!is.null(base_map)) paste0('**Base map**: ', base_map, ' | ')}**Projection**: {proj}<br>",
    "**#30DayMapChallenge {year}**: Day {day} - {prompt} | ",
    "**Visualizastion**: Andriambelo Rajo | #rstats",
    .null = ""
  )
}
