#' Graphical interface
#'
#' Opens the graphical interface of the package in the web browser (or the
#' RStudio viewer). It needs the package \pkg{shiny}
#' (\code{install.packages("shiny")}). Everything available in code is also
#' available in the window:
#' \describe{
#'   \item{Test}{open a CSV, TXT or Excel file (or use the example data),
#'     choose a series or tick \emph{test all columns} to screen every column,
#'     choose the transform, set the options, run the test, and download the
#'     report (text) or the results (CSV).}
#'   \item{Sample size & power}{required sample size, power for a given
#'     \eqn{n} and default resolution for \eqn{n}, from Gamma or V (and tau);
#'     \emph{Use last test} takes the estimates of the last test.}
#'   \item{Null distribution}{components, mean, variance and critical values
#'     of the null limit \eqn{L_{m,d}}, and the P-value of any value of Q.}
#'   \item{User guide}{the user guide of the package.}
#' }
#' Close the browser tab and press Esc (or Stop) in R to finish.
#'
#' @param launch.browser Passed to \code{shiny::runApp}.
#' @return Called for its side effect; returns when the app is closed.
#' @examples
#' if (interactive()) ivserial_app()
#' @export
ivserial_app <- function(launch.browser = TRUE) {
  if (!requireNamespace("shiny", quietly = TRUE))
    stop("the graphical interface needs the package shiny: install.packages(\"shiny\")", call. = FALSE)
  shiny::runApp(ivserial_app_object(), launch.browser = launch.browser)
}

ivserial_app_object <- function() {
  num <- function(id, label, value, width = "110px", ...) shiny::numericInput(id, label, value, width = width, ...)
  txt <- function(id, label, value = "", width = "110px") shiny::textInput(id, label, value, width = width)
  row <- function(...) shiny::div(style = "display:flex;flex-wrap:wrap;gap:14px;align-items:flex-end;", ...)
  guide <- system.file("guide", "ivserial-guide.html", package = "ivserial")
  if (nzchar(guide)) shiny::addResourcePath("ivserialdoc", dirname(guide))

  ui <- shiny::fluidPage(
    shiny::tags$head(shiny::tags$style(shiny::HTML(
      "pre{font-size:12.5px;white-space:pre;overflow-x:auto} .help{color:#555;max-width:900px}
       .form-group{margin-bottom:6px}"))),
    shiny::titlePanel(paste0("ivserial ", utils::packageVersion("ivserial"),
                         " - exact symbolic test of serial independence")),
    shiny::tabsetPanel(
      id = "tabs",
      shiny::tabPanel(
        "Test",
        shiny::br(),
        row(shiny::fileInput("file", "Data file (CSV, TXT or Excel; one series per column, in time order)",
                         accept = c(".csv", ".txt", ".dat", ".xlsx", ".xls"), width = "420px"),
            shiny::actionButton("example", "Use example data (20 exchange rates)")),
        shiny::textOutput("fileinfo"),
        row(shiny::selectInput("column", "Series (column)", choices = character(), width = "260px"),
            shiny::checkboxInput("all", "test all columns (screening)", FALSE),
            shiny::radioButtons("transform", "Transform", inline = TRUE,
                            choices = c("none" = "none", "differences" = "diff", "log returns" = "logdiff"))),
        row(num("B", "Permutations B", 9999, min = 1),
            txt("seed", "Seed (blank: random)", "1"),
            txt("m", "Block length m", "auto"),
            txt("d", "Classes d", "auto"),
            num("alpha", "Level", 0.05, min = 0.0001, max = 0.5, step = 0.01),
            num("lags", "Portmanteau lags", 2, min = 1),
            txt("d2", "Portmanteau classes", "auto"),
            num("cells", "Patterns shown", 8, min = 0)),
        row(shiny::actionButton("run", "Run test", class = "btn-primary"),
            shiny::downloadButton("save_txt", "Save report (.txt)"),
            shiny::downloadButton("save_csv", "Save results (.csv)")),
        shiny::br(),
        shiny::verbatimTextOutput("report")
      ),
      shiny::tabPanel(
        "Sample size & power",
        shiny::br(),
        shiny::p(class = "help",
             "Plan the number of observations for the block test (Corollary 4.8 of the paper). Give the strength",
             "of dependence as Gamma or as the serial Cramer coefficient V (0 to 1) at the resolution (m, d),",
             "and tau, the asymptotic standard deviation (0 for weak dependence; otherwise the result is optimistic).",
             "'Use last test' takes Gamma-hat, tau-hat, m and d from the last test, for example on a pilot sample."),
        row(txt("p_gamma", "Gamma", "0.13"), txt("p_v", "or V (0 to 1)", ""),
            num("p_tau", "tau", 1.13, min = 0, step = 0.01),
            num("p_m", "m", 3, min = 2), num("p_d", "d", 4, min = 2),
            num("p_alpha", "Level", 0.05, min = 0.0001, max = 0.5, step = 0.01),
            txt("p_power", "Power(s)", "0.5 0.8 0.9 0.95", width = "160px"),
            num("p_n", "n (for power)", 350, min = 10)),
        row(shiny::actionButton("p_last", "Use last test"),
            shiny::actionButton("p_size", "Required sample size", class = "btn-primary"),
            shiny::actionButton("p_pow", "Power for n"),
            shiny::actionButton("p_res", "Resolution for n")),
        shiny::br(),
        shiny::verbatimTextOutput("plan")
      ),
      shiny::tabPanel(
        "Null distribution",
        shiny::br(),
        shiny::p(class = "help",
             "Under independence Q converges to L(m, d) = sum over s = 2..m of (m - s + 1) chi2 with (d-1)^2 d^(s-2)",
             "degrees of freedom (Theorem 4.1). Enter m, d and, optionally, a value of Q for its asymptotic P-value."),
        row(num("n_m", "m", 3, min = 2), num("n_d", "d", 4, min = 2), txt("n_q", "Q (optional)", "80"),
            shiny::actionButton("n_go", "Compute", class = "btn-primary")),
        shiny::br(),
        shiny::verbatimTextOutput("null")
      ),
      shiny::tabPanel(
        "User guide",
        if (nzchar(guide))
          shiny::tags$iframe(src = "ivserialdoc/ivserial-guide.html", style = "width:100%;height:80vh;border:0;")
        else shiny::p("The guide is available with vignette(\"ivserial\") or ivserial_guide().")
      )
    )
  )

  server <- function(input, output, session) {
    st <- shiny::reactiveValues(data = NULL, name = "", report = "Open a data file and press 'Run test'.",
                            results = NULL, last = NULL, plan = "", null = "")

    auto_int <- function(s, name) {
      s <- trimws(s)
      if (s == "" || tolower(s) == "auto") return(NULL)
      v <- suppressWarnings(as.integer(s))
      if (is.na(v)) stop(name, " must be a whole number or 'auto'", call. = FALSE)
      v
    }
    set_data <- function(df, name) {
      st$data <- df
      st$name <- name
      shiny::updateSelectInput(session, "column", choices = names(df), selected = names(df)[1])
    }
    shiny::observeEvent(input$file, {
      tryCatch(set_data(read_series(input$file$datapath), input$file$name),
               error = function(e) st$report <- paste("Error:", conditionMessage(e)))
    })
    shiny::observeEvent(input$example, {
      set_data(read_series(system.file("extdata", "exchange_rates.csv", package = "ivserial")),
               "exchange_rates.csv (example: prices; use log returns)")
      shiny::updateRadioButtons(session, "transform", selected = "logdiff")
    })
    output$fileinfo <- shiny::renderText({
      if (is.null(st$data)) "" else sprintf("%s: %d rows, %d numeric columns", st$name, nrow(st$data), ncol(st$data))
    })

    shiny::observeEvent(input$run, {
      if (is.null(st$data)) {
        st$report <- "Open a data file first."
        return()
      }
      out <- tryCatch({
        seed <- if (trimws(input$seed) == "") NULL else as.integer(input$seed)
        args <- list(m = auto_int(input$m, "m"), d = auto_int(input$d, "d"), d2 = auto_int(input$d2, "d2"),
                     lags = input$lags, B = input$B, seed = seed, alpha = input$alpha,
                     transform = input$transform, n_cells = max(input$cells, 1))
        shiny::withProgress(message = "Running permutations...", value = 0.5, {
          if (isTRUE(input$all)) {
            sc <- do.call(screen_series, c(list(st$data), args))
            st$results <- sc$results
            st$last <- if (length(sc$results)) sc$results[[1]] else NULL
            paste(c(unlist(lapply(sc$results, function(r) c(format(r, n_cells = input$cells), ""))), format(sc)),
                  collapse = "\n")
          } else {
            msgs <- character()
            r <- withCallingHandlers(
              do.call(serial_test, c(list(st$data[[input$column]], label = input$column), args)),
              warning = function(w) {
                msgs <<- c(msgs, conditionMessage(w))
                invokeRestart("muffleWarning")
              })
            st$results <- list(r)
            st$last <- r
            paste(c(format(r, n_cells = input$cells), if (length(msgs)) paste("Note:", msgs)), collapse = "\n")
          }
        })
      }, error = function(e) paste("Error:", conditionMessage(e)))
      st$report <- out
    })
    output$report <- shiny::renderText(st$report)
    output$save_txt <- shiny::downloadHandler(
      filename = function() "ivserial_report.txt",
      content = function(file) writeLines(st$report, file))
    output$save_csv <- shiny::downloadHandler(
      filename = function() "ivserial_results.csv",
      content = function(file) {
        tab <- if (is.null(st$results)) data.frame() else do.call(rbind, lapply(st$results, as.data.frame))
        if (nrow(tab) > 1) tab$p_adj_BH <- stats::p.adjust(tab$p_C, "BH")
        utils::write.csv(tab, file, row.names = FALSE)
      })

    plan_gamma <- function() {
      m <- input$p_m
      d <- input$p_d
      if (nzchar(trimws(input$p_v))) {
        v <- as.numeric(input$p_v)
        g <- gamma_from_cramer_v(v, m, d)
        attr(g, "note") <- sprintf("V = %g at (m, d) = (%d, %d) corresponds to Gamma = %.4f", v, m, d, g)
        return(g)
      }
      g <- as.numeric(input$p_gamma)
      if (!is.finite(g)) stop("Gamma must be a number (or give V)", call. = FALSE)
      if (g <= 0) stop("Gamma must be positive; a Gamma-hat of 0 or less means that the test found no dependence to plan for",
                       call. = FALSE)
      g
    }
    run_plan <- function(f) st$plan <- tryCatch(f(), error = function(e) paste("Error:", conditionMessage(e)))
    shiny::observeEvent(input$p_last, {
      r <- st$last
      if (is.null(r)) {
        st$plan <- "Run a test first."
        return()
      }
      shiny::updateTextInput(session, "p_gamma", value = sprintf("%.4f", r$gamma_hat))
      shiny::updateTextInput(session, "p_v", value = "")
      shiny::updateNumericInput(session, "p_tau", value = round(r$tau_hat, 3))
      shiny::updateNumericInput(session, "p_m", value = r$m)
      shiny::updateNumericInput(session, "p_d", value = r$d)
      shiny::updateNumericInput(session, "p_n", value = r$n)
      st$plan <- sprintf("Taken from the last test (%s): Gamma-hat = %.4f, V-hat = %.3f, tau-hat = %.3f, m = %d, d = %d, n = %d",
                         if (nzchar(r$label)) r$label else "series", r$gamma_hat, r$cramer_v, r$tau_hat, r$m, r$d, r$n)
    })
    shiny::observeEvent(input$p_size, run_plan(function() {
      g <- plan_gamma()
      pw <- as.numeric(strsplit(trimws(input$p_power), "[ ,;]+")[[1]])
      tab <- sample_size_table(g, input$p_m, input$p_d, input$p_alpha, input$p_tau, pw)
      paste(c(attr(g, "note"),
              sprintf("Required number of observations (Gamma = %.4g, tau = %g, (m, d) = (%d, %d), level %g)",
                      g, input$p_tau, input$p_m, input$p_d, input$p_alpha),
              sprintf("%8s%10s", "power", "n"), sprintf("%8.2f%10d", tab$power, tab$n)), collapse = "\n")
    }))
    shiny::observeEvent(input$p_pow, run_plan(function() {
      g <- plan_gamma()
      paste(c(attr(g, "note"),
              sprintf("Approximate power with n = %d (Gamma = %.4g, tau = %g, (m, d) = (%d, %d), level %g): %.3f",
                      as.integer(input$p_n), g, input$p_tau, input$p_m, input$p_d, input$p_alpha,
                      approximate_power(g, input$p_n, input$p_m, input$p_d, input$p_alpha, input$p_tau))),
            collapse = "\n")
    }))
    shiny::observeEvent(input$p_res, run_plan(function() {
      md <- choose_resolution(input$p_n)
      sprintf("Default resolution for n = %d: m = %d, d = %d (%d patterns, %.1f blocks per pattern)",
              as.integer(input$p_n), md[["m"]], md[["d"]], as.integer(md[["d"]]^md[["m"]]),
              (input$p_n - md[["m"]] + 1) / md[["d"]]^md[["m"]])
    }))
    output$plan <- shiny::renderText(st$plan)

    shiny::observeEvent(input$n_go, {
      st$null <- tryCatch({
        s <- null_limit_summary(input$n_m, input$n_d)
        out <- utils::capture.output(print(s))
        q <- suppressWarnings(as.numeric(input$n_q))
        if (is.finite(q)) out <- c(out, sprintf("P(L > %g) = %.5f", q, null_limit_sf(q, input$n_m, input$n_d)))
        paste(out, collapse = "\n")
      }, error = function(e) paste("Error:", conditionMessage(e)))
    })
    output$null <- shiny::renderText(st$null)
  }

  shiny::shinyApp(ui, server)
}
