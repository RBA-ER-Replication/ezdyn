# Help guides shown by the dashboard's info buttons.
#
# To add a guide:
#   1. Add an entry to `usa_help_guides` below, written in markdown. Its first
#      "## " heading is the guide's title; any other "## " headings become
#      sub-headings. Links open in a new browser tab. For maths, use Unicode
#      and HTML, e.g. λ<sup>h</sup>, which displays without loading MathJax.
#   2. Point a button at it in usa_dashboard.R with `usa_help("<name>")`:
#      - Alternative Policy Paths' two buttons on anticipated shocks and the
#        cognitive discounting parameter: configure_dashboard()'s
#        `help = list(anticipated_shocks = ...)`.
#      - Optimal Policy's buttons: `optimal_policy$help`, with names `cd` (the
#        anticipation parameter), `strategy`, `loss_variables`,
#        `loss_horizon`, `instrument_horizon`, `constraints` and
#        `commitment_start`.
#   Buttons without a guide show a generic "no additional help" message.

usa_help_guides <- list(
  anticipated_shocks = r"---(
## Anticipated shocks and the cognitive discounting parameter
For more explanation and technical detail, see [Sullivan and Ryan 2026](https://github.com/RBA-ER-Replication/ezdyn/blob/main/ezdyn.pdf?raw=true).

In forward-looking models, the dynamics of the economy today are influenced by how agents form their expectations for the future state of the economy.

## What is the difference between an anticipated and unanticipated shock?
The monetary policy shocks used to generate the alternative path can be either 'anticipated' or 'unanticipated':
- **If the policy path is fully anticipated**, this is consistent with an announcement or forward guidance occurring at the next policy meeting, which shifts agents' expectations completely to the alternative path.
- **If the policy path is unanticipated**, this is consistent with agents only updating their expectations based on the announced policy rate, and being continually surprised as subsequent policy rate announcements follow the alternative path. This is the method of Sims and Zha 1995.

## Anticipated or unanticipated - the motivation for 'Partial anticipation'
**In most cases, it would be unrealistic to assume that an alternative policy rate path is completely unanticipated.** If policymakers chose an alternative policy strategy, they would likely seek to communicate it to the public, rather than repeatedly surprising them quarter after quarter as they implemented it.

**But it is also probably unrealistic to assume that alternative policy strategies would be fully anticipated.** It is unlikely that policy expectations of the public would always immediately jump to the central bank's new policy path. Further, the especially strong effects of anticipated policy rely on rational expectations assumptions that agents perfectly understand all the general equilibrium consequences of future policy and immediately adjust their behaviour accordingly.

## Partial anticipation and the cognitive discounting parameter
As a result, we implement a 'partial anticipation' approach, which allows the responses of forward-looking models to be modified so that the future alternative monetary policy path is only partially anticipated by agents in the economy.

Our approach assumes that, at any point in time, the shocks generating the change in the policy rate path set to occur in h quarters are λ<sup>h</sup> anticipated, and (1 − λ<sup>h</sup>) unanticipated, for some cognitive discount parameter λ ∈ [0, 1]. The rationale is that shocks occurring in the near future (e.g. next quarter) are likely anticipated by more people, and thus have a greater impact on current behaviour than shocks projected to occur in the distant future (e.g. 10 years time).
)---"
)

# Return a help hook for configure_dashboard(): a function that shows guide
# `name` from `usa_help_guides` in a modal. The guide is rendered once, here.
usa_help <- function(name) {
  guide <- usa_help_guides[[name]]
  if (!is.character(guide) || length(guide) != 1) {
    stop("There's no help guide called `", name, "` in help_text.R.", call. = FALSE)
  }
  lines <- strsplit(trimws(guide), "\n", fixed = TRUE)[[1]]
  title_line <- grep("^## ", lines)[1]
  if (is.na(title_line)) {
    stop("Help guide `", name, "` needs a \"## \" heading for its title.", call. = FALSE)
  }
  title <- trimws(sub("^## ", "", lines[[title_line]]))
  # The modal title is the only big heading: show the others as sub-headings.
  body <- sub("^(#{2,3}) ", "\\1## ", lines[-title_line])
  html <- as.character(shiny::markdown(paste(body, collapse = "\n")))
  html <- shiny::HTML(gsub("<a href=", "<a target=\"_blank\" rel=\"noopener\" href=", html, fixed = TRUE))
  function() {
    shiny::modalDialog(title = title, size = "l", easyClose = TRUE, footer = shiny::modalButton("Close"), html)
  }
}
