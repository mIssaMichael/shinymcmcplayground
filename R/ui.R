#' @importFrom shiny tags

tooltip <- function(label, content) {
  tags$div(
    class = "control-label-tooltip",
    label,
    tags$span(
      class = "control-label-tooltip-content",
      content
    )
  )
}

control_label <- function(label, tooltip_label = NULL, tooltip_content = NULL) {
  if (is.null(tooltip_label)) {
    out <- tags$div(tags$p(label), class = "control-label")
  } else {
    out <- tags$div(
      tags$p(label),
      tooltip(tooltip_label, tooltip_content),
      class = "control-label"
    )
  }
  out
}

select_input_options <- list(
  render = I("
    {
      item: function(item, escape) {
              var html = katex.renderToString(item.label);
              return '<div>' + html + '</div>';
            },
      option: function(item, escape) {
                var html = katex.renderToString(item.label);
                return '<div>' + html + '</div>';
              }
    }"
  )
)

choices <- c("normal_1", "normal_2", "normal_3", "normal_mixture", "banana", "funnel")
choices_names <- c(
  "\\mathcal{N}\\left(\\begin{bmatrix} 0 \\\\ 0 \\end{bmatrix}, \\begin{bmatrix} 1 & 0 \\\\ 0 & 1 \\end{bmatrix}\\right)",
  "\\mathcal{N}\\left(\\begin{bmatrix} 0 \\\\ 0 \\end{bmatrix}, \\begin{bmatrix} 1 & 0.3 \\\\ 0.3 & 1 \\end{bmatrix}\\right)",
  "\\mathcal{N}\\left(\\begin{bmatrix} 0 \\\\ 0 \\end{bmatrix}, \\begin{bmatrix} 1 & 0.7 \\\\ 0.7 & 1 \\end{bmatrix}\\right)",
  "\\frac{1}{2}\\mathcal{N}\\left(\\begin{bmatrix} -1.5 \\\\ -1.7 \\end{bmatrix}, 0.2 \\cdot \\boldsymbol{I}_2 \\right) + \\frac{1}{2}\\mathcal{N}\\left(\\begin{bmatrix} 0.9 \\\\ 0.5 \\end{bmatrix}, 1.25 \\cdot \\boldsymbol{I}_2 \\right)",
  "\\text{Rosenbrock's Banana}",
  "\\text{Neal's Funnel}"
)

choices <- setNames(choices, choices_names)

make_sidebar <- function() {
  tags$div(
    id = "sidebar",
    tags$div(
      class = "item",
      tags$p("Choose a distribution", class = "sidebar-group-header"),
      shiny::selectizeInput(
        "distribution", NULL, choices, options = select_input_options, width = "100%"
      )
    ),
    tags$div(
      class = "item",
      tags$p("Choose a sampling algorithm", class = "sidebar-group-header"),
      shiny::selectizeInput(
        "sampling_algorithm", NULL, 
        choices = c("Hamiltonian Monte Carlo" = "hmc", "Metropolis-Hastings" = "mh"), 
        width = "100%"
      )
    ),
    shiny::conditionalPanel(
      condition = "input.sampling_algorithm == 'hmc'",
      tags$div(
        class = "item",
        tags$p("HMC Controls", class = "sidebar-group-header"),
        tags$div(
          style = "margin-bottom: 35px; margin-top: 45px",
          rangeInput("path_length", min = 0.5, max = 5, step = 0.1, value = 2),
          control_label(
            "Path length",
            shiny::icon("question-circle"),
            "For how long to integrate the trajectory (aka integration time)"
          )
        ),
        tags$div(
          style = "margin-bottom: 35px",
          rangeInput("step_size", min = 0.01, max = 0.5, step = 0.01, value = 0.05),
          control_label(
            "Step size",
            shiny::icon("question-circle"),
            "Time length of the discretization steps"
          ),
        ),
        tags$div(
          rangeInput("speed", step = 1, value = 3, labels = c("Slow", "Medium", "Fast", "Hurry")),
          tags$p("Animation speed", class = "range-input-label")
        ),
        tags$div(
          shiny::checkboxInput("draw_trajectory", "Draw Trajectory", TRUE)
        ),
        tags$div(
          shiny::checkboxInput("manual_momentum", "Set momentum manually", FALSE)
        ),
        tags$div(
          style = "display: flex; gap: 5px",
          shinyjs::disabled(
            shiny::numericInput(
              "momentum_x", label = "X", value = 0.5, min = -4, max = 4, step = 0.1
            )
          ),
          shinyjs::disabled(
            shiny::numericInput(
              "momentum_y", label = "Y", value = 0.5, min = -4, max = 4, step = 0.1
            )
          ),
        )
      )
    ),
    shiny::conditionalPanel(
      condition = "input.sampling_algorithm == 'mh'",
      tags$div(
        class = "item",
        tags$p("MH Controls", class = "sidebar-group-header"),
        tags$div(
          style = "margin-bottom: 35px; margin-top: 45px",
          rangeInput("proposal_sd", min = 0.01, max = 2, step = 0.01, value = 0.1),
          control_label(
            "Proposal Standard Deviation",
            shiny::icon("question-circle"),
            "Controls how far the proposal can move from the current position"
          )
        )
      )
    ),
    # Common Actions section for both algorithms
    tags$div(
      class = "item",
      tags$p("Actions", class = "sidebar-group-header"),
      tags$div(
        style = "display: flex; gap: 5px; margin-top: 20px; margin-bottom: 15px;",
        shiny::actionButton("start_sampling", "Start sampling", width = "100%"),
        shiny::actionButton("stop_sampling", "Stop sampling", disabled = TRUE, width = "100%")
      ),
      tags$div(
        style = "margin: 15px 0px;",
        shiny::actionButton("add_point", "Sample a single point", width = "100%")
      ),
      tags$div(
        style = "margin: 15px 0px;",
        shiny::actionButton("remove_points", "Remove points", width = "100%")
      )
    )
  )
}

tex_panel_1 <- "U (\\boldsymbol{\\theta}) = - \\log[ p^*(\\boldsymbol{\\theta} \\mid \\boldsymbol{y})]"
tex_panel_2 <- "p (\\boldsymbol{\\theta} \\mid \\boldsymbol{y})"

make_body <- function() {
  
  tags$div(
    style = "margin: 10px",
    tags$h2(
      shiny::textOutput("algorithm_title", inline = TRUE), 
      class = "body-header"
    ),
    tags$div(
      class = "plot-container",
      tags$div(htmltools::HTML(katex::katex_html(tex_panel_1, preview = FALSE))),
      tags$div(htmltools::HTML(katex::katex_html(tex_panel_2, preview = FALSE))),
    ),
    tags$div(
      style = "padding: 0px 10px",
      rgl::rglwidgetOutput("rglPlot", width = "100%")
    ),
      shiny::conditionalPanel(
        condition = "input.sampling_algorithm == 'hmc'",
        tags$div(
        style = "padding-left: 10px;",
        tags$h2("Details", class = "details-header"),
        tags$div(
          class = "accordion",
          accordionItem(
            "The Hamiltonian",
            htmltools::HTML(
              katex::render_math_in_html(app_sys("www/assets/hmc/hamiltonian.html"), include_css = FALSE)
            )
          ),
          tags$hr(),
          accordionItem(
            "Hamilton's Equations",
            htmltools::HTML(
              katex::render_math_in_html(app_sys("www/assets/hmc/hamiltons.html"), include_css = FALSE)
            )
          ),
          tags$hr(),
          accordionItem(
            "Leapfrog Integrator",
            htmltools::HTML(
              katex::render_math_in_html(app_sys("www/assets/hmc/leapfrog.html"), include_css = FALSE)
            )
          ),
          tags$hr(),
          accordionItem(
            "Metropolis Acceptance Criteria",
            htmltools::HTML(
              katex::render_math_in_html(app_sys("www/assets/hmc/accept.html"), include_css = FALSE)
            )
          ),
          tags$hr(),
          accordionItem(
            "Summary",
            htmltools::HTML(
              katex::render_math_in_html(app_sys("www/assets/hmc/summary.html"), include_css = FALSE)
            )
          ),
          tags$hr()
        )
      )
    ),
    shiny::conditionalPanel(
      condition = "input.sampling_algorithm =='mh'",
      tags$div(
        style = "padding-left: 10px;",
        tags$h2("Details", class = "details-header"),
        tags$div(
          class = "accordion",
          accordionItem(
            "The Proposal Distiribution",
            htmltools::HTML(
              katex::render_math_in_html(app_sys("www/assets/mh/proposal.html"), include_css = FALSE)
            )
          ),
          tags$hr(),
          accordionItem(
            "Metropolis Acceptance Criteria",
            htmltools::HTML(
              katex::render_math_in_html(app_sys("www/assets/mh/accept.html"), include_css = FALSE)
            )
          ),
          tags$hr(),
          accordionItem(
            "Metropolis-Hastings",
            htmltools::HTML(
              katex::render_math_in_html(app_sys("www/assets/mh/metropolis.html"), include_css = FALSE)
            )
          ),
          tags$hr(),
          accordionItem(
            "Summary",
            htmltools::HTML(
              katex::render_math_in_html(app_sys("www/assets/mh/summary.html"), include_css = FALSE)
            )
          ),
          tags$hr()
        )
      )
    )
  )
}

accordionItem <- function(label, content) {
  tags$div(
    class = "accordion-container",
    tags$div(class = "label", label),
    tags$div(class = "content", content)
  )
}

ui <- function() {
  header <- tags$head(
    tags$title("MCMC Sampling Visualization"),
    tags$link(rel = "icon", type = "image/png", sizes = "64x64", href = "www/alpine.png"),
    tags$script(type = "text/javascript", src = "www/hmc.js"),
    tags$script(type = "text/javascript", src = "www/sidebar.js"),
    tags$script(type = "text/javascript", src = "www/accordion.js"),
    tags$link(type="text/css", rel = "stylesheet", href = "www/styles.css"),
    tags$link(
      rel = "stylesheet",
      href = "https://cdn.jsdelivr.net/npm/katex@0.10.0-beta/dist/katex.min.css",
      integrity = "sha384-9tPv11A+glH/on/wEu99NVwDPwkMQESOocs/ZGXPoIiLE8MU/qkqUcZ3zzL+6DuH",
      crossorigin = "anonymous"
    ),
    tags$script(
      src = "https://cdn.jsdelivr.net/npm/katex@0.10.0-beta/dist/katex.min.js",
      integrity = "sha384-U8Vrjwb8fuHMt6ewaCy8uqeUXv4oitYACKdB0VziCerzt011iQ/0TqlSlv8MReCm",
      crossorigin = "anonymous"
    )
  )

  body <- tags$body(
    shinyjs::useShinyjs(),
    make_sidebar(),
    make_body()
  )

  ui <- do.call(htmltools::tagList, list(body, header))
  htmltools::attachDependencies(ui, shiny::bootstrapLib())
}