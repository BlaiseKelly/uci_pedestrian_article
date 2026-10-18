library(shiny)
library(bslib)
library(ggplot2)

# ---- Vehicle presets ----
# frontal_area (m2), cd (drag coefficient) -- these describe the empty vehicle;
# vehicle_weight (kg) is the unoccupied ("kerb") weight, with passengers added separately.
# rolling_coef, power_source + efficiency (fraction of the power source's energy that
# actually reaches the wheels -- see power_source_efficiency below for the categories
# used in the infographic), max_passengers/default_passengers size the passenger slider.
vehicle_presets <- data.frame(
  vehicle           = c("Small car", "SUV", "Bus", "Truck", "Sports car", "Tram/Train", "Bicycle", "E-scooter"),
  frontal_area      = c(2.2, 2.8, 8.0, 9.0, 1.8, 11.0, 0.4, 0.5),
  cd                = c(0.30, 0.35, 0.60, 0.70, 0.32, 0.40, 0.90, 1.00),
  vehicle_weight    = c(1200, 2100, 12000, 8000, 1400, 11000, 15,13),
  rolling_coef      = c(0.012, 0.013, 0.007, 0.007, 0.013, 0.001, 0.005, 0.02),
  power_source      = c("Petrol/diesel engine", "Petrol/diesel engine", "Petrol/diesel engine",
                         "Petrol/diesel engine", "Petrol/diesel engine", "Grid electric (direct)",
                         "Human power", "Battery electric (indirect)"),
  efficiency        = c(0.30, 0.30, 0.35, 0.35, 0.05, 0.90, 0.25, 0.80),
  max_passengers    = c(5, 7, 60, 2, 2, 300, 1, 1),
  default_passengers = c(1, 1, 10, 1, 1, 20, 1, 1),
  # Typical cruising speed (m/s) for each vehicle type -- e.g. a bus averages
  # much slower than its top speed due to stops, while a sports car defaults
  # to a brisk motorway pace.
  default_speed     = c(15, 15, 8, 20, 25, 12, 5, 6),
  stringsAsFactors = FALSE
)
# Frontal area x Cd (the "drag area") for reference:
#   Small car ~0.66 m2, Bicycle (upright) ~0.36 m2 -- a car pushes more air out of
# the way than a bike despite the bike having a less streamlined shape, because a
# car's frontal area is so much larger. Sports car ~0.58 m2 (1.8 x 0.32), in line with
# real low-slung sports cars like a Mazda RX-7 or C4 Corvette (drag area ~0.55-0.60 m2).

# Rolling resistance coefficients (Crr): cars/SUVs ~0.010-0.015 on standard tyres;
# buses/trucks are notably lower (~0.006-0.008) thanks to larger, purpose-built tyres,
# despite carrying far more weight; steel wheel on rail is ~0.0002-0.001; small hard
# e-scooter wheels run higher (~0.02-0.03) than car tyres.

# Power source categories (matching the infographic), each with a typical efficiency
# used to reset the efficiency slider when the power source is changed manually, and
# a CO2/fuel conversion factor: mg emitted or consumed per Joule of *source* energy.
# Petrol/diesel engines are notoriously inefficient (5-40%). A motor plugged directly
# into the mains (e.g. a tram/train) is very efficient (~90%); a battery electric
# vehicle or e-bike is slightly less efficient once battery charge/discharge losses
# are included (~70-80%). The human body converts food into movement at ~25%.
power_source_efficiency <- c(
  "Petrol/diesel engine" = 0.30,
  "Battery electric (indirect)" = 0.80,
  "Grid electric (direct)" = 0.90,
  "Human power" = 0.25
)
power_source_co2 <- c(
  "Petrol/diesel engine" = 0.06,
  "Battery electric (indirect)" = 0.09,
  "Grid electric (direct)" = 0.09,
  "Human power" = 0.3
)

# What "efficiency" physically represents for each power source category.
power_source_note <- c(
  "Petrol/diesel engine" = "Share of fuel's chemical energy converted to motion (combustion + drivetrain losses).",
  "Battery electric (indirect)" = "Share of battery energy reaching the wheels (charging + battery + motor losses).",
  "Grid electric (direct)" = "Share of mains electricity reaching the wheels (motor + drivetrain losses only, no battery).",
  "Human power" = "Share of food energy the body converts into pedalling/walking power."
)

air_density <- 1.3
g <- 9.81
avg_person_weight <- 75

ui <- page_sidebar(
  title = "The Energy of Transport",
  theme = bs_theme(version = 5, preset = "shiny"),
  sidebar = sidebar(
    width = 400,
    selectInput("vehicle", "Vehicle type", choices = vehicle_presets$vehicle, selected = "Small car"),
    sliderInput("speed", "Speed (m/s)", min = 1, max = 40, value = 13, step = 0.1),
    div(style = "margin-top: -12px; margin-bottom: 10px; color: #666; font-size: 0.85rem;",
        textOutput("speed_equiv", inline = TRUE)),
    sliderInput("distance", "Distance travelled (km)", min = 0.2, max = 100, value = 10, step = 0.2),
    hr(),
    h6("Vehicle"),
    sliderInput("frontal_area", "Frontal area (m²)", min = 0.3, max = 12, value = 2.2, step = 0.1),
    sliderInput("cd", "Coefficient of drag (Cd)", min = 0.1, max = 1, value = 0.3, step = 0.05),
    sliderInput("vehicle_weight", "Vehicle weight, unoccupied (kg)", min = 10, max = 15000, value = 1200, step = 10),
    sliderInput("rolling_coef", "Rolling resistance coefficient", min = 0.001, max = 0.02, value = 0.015, step = 0.001),
    hr(),
    h6("Occupants"),
    sliderInput("passengers", "Number of passengers (incl. driver)", min = 0, max = 5, value = 1, step = 1),
    sliderInput("passenger_weight", "Average weight per passenger (kg)", min = 10, max = 120, value = 75, step = 5),
    hr(),
    h6("Power source"),
    selectInput("power_source", "Power source", choices = names(power_source_efficiency)),
    div(style = "margin-top: -8px; margin-bottom: 10px; color: #666; font-size: 0.82rem;",
        textOutput("efficiency_note", inline = TRUE)),
    sliderInput("efficiency", "Efficiency", min = 0.05, max = 0.95, value = 0.3, step = 0.01)
  ),
  layout_column_wrap(
    width = 1/3,
    fill = FALSE,
    value_box(
      title = "Mechanical energy needed",
      value = textOutput("total_energy"),
      p(style = "font-size: 0.85rem; opacity: 0.85;", textOutput("total_energy_wh", inline = TRUE)),
      theme = "primary"
    ),
    value_box(
      title = "Energy needed from power source",
      value = textOutput("engine_energy"),
      p(style = "font-size: 0.85rem; opacity: 0.85;", textOutput("engine_energy_wh", inline = TRUE)),
      theme = "secondary"
    ),
    value_box(title = "Fuel / CO2 equivalent", value = textOutput("fuel_energy"), theme = "info")
  ),
  card(
    full_screen = TRUE,
    card_header("Energy components (J)"),
    plotOutput("energy_plot")
  ),
  card(
    card_header("How each figure is calculated"),
    uiOutput("eqn_text")
  ),
  card(
    card_header("About this app"),
    p("Based on the simple mechanics formulae, simplified and explained by David MacKay's book",
      tags$a(href = "https://www.withouthotair.com/cA/page_254.shtml", target = "_blank",
             "Sustainable Energy Without the Hot Air"),
      ". Energy is split into three",
      "physical components: drag (air resistance), rolling resistance (tyres/road), and kinetic energy",
      "(needed once, to accelerate up to speed). Vehicle weight and passenger weight are combined for",
      "rolling resistance and kinetic energy. The power source then determines the efficiency with",
      "which that mechanical energy is delivered: petrol/diesel engines are notoriously inefficient",
      "(5-40%), a motor plugged directly into the mains (e.g. a tram) is very efficient (~90%), a",
      "battery electric vehicle or e-bike is slightly less efficient due to battery losses (~70-80%),",
      "and the human body converts food into movement at around 25% (largely meaningless as human activity has enormous benefits)")
  )
)

server <- function(input, output, session) {

  # When the power source is changed via the vehicle preset, the efficiency slider
  # should take the vehicle's specific value -- not the generic default for that
  # power source. This flag suppresses the power_source observer just for that case.
  suppress_power_source_reset <- reactiveVal(FALSE)

  # Update sliders when a vehicle preset is chosen
  observeEvent(input$vehicle, {
    preset <- vehicle_presets[vehicle_presets$vehicle == input$vehicle, ]
    suppress_power_source_reset(TRUE)
    updateSliderInput(session, "speed", value = preset$default_speed)
    updateSliderInput(session, "frontal_area", value = preset$frontal_area)
    updateSliderInput(session, "cd", value = preset$cd)
    updateSliderInput(session, "vehicle_weight", value = preset$vehicle_weight)
    updateSliderInput(session, "rolling_coef", value = preset$rolling_coef)
    updateSelectInput(session, "power_source", selected = preset$power_source)
    updateSliderInput(session, "efficiency", value = preset$efficiency)
    updateSliderInput(session, "passengers", max = preset$max_passengers, value = preset$default_passengers)
  })

  # If the user manually changes the power source (independent of a vehicle change),
  # reset efficiency to that source's typical value.
  observeEvent(input$power_source, {
    if (isTRUE(suppress_power_source_reset())) {
      suppress_power_source_reset(FALSE)
      return()
    }
    updateSliderInput(session, "efficiency", value = power_source_efficiency[[input$power_source]])
  })

  energy <- reactive({
    distance_m <- input$distance * 1000
    total_weight <- input$vehicle_weight + input$passengers * input$passenger_weight

    drag_force <- 0.5 * air_density * input$cd * input$frontal_area * input$speed^2
    drag_energy <- drag_force * distance_m

    rolling_force <- input$rolling_coef * total_weight * g
    rolling_energy <- rolling_force * distance_m

    kinetic_energy <- 0.5 * total_weight * input$speed^2

    total <- drag_energy + rolling_energy + kinetic_energy
    engine <- total / input$efficiency

    list(
      drag = drag_energy,
      rolling = rolling_energy,
      kinetic = kinetic_energy,
      total = total,
      engine = engine,
      total_weight = total_weight
    )
  })

  output$efficiency_note <- renderText({
    power_source_note[[input$power_source]]
  })

  output$speed_equiv <- renderText({
    mph <- input$speed * 2.23694
    kmh <- input$speed * 3.6
    sprintf("= %.1f mph  /  %.1f km/h", mph, kmh)
  })

  output$total_energy <- renderText({
    paste0(format(round(energy()$total), big.mark = ","), " J")
  })

  output$total_energy_wh <- renderText({
    paste0("\u2248 ", format(round(energy()$total / 3600, 2), big.mark = ","), " Wh")
  })

  output$engine_energy <- renderText({
    paste0(format(round(energy()$engine), big.mark = ","), " J")
  })

  output$engine_energy_wh <- renderText({
    paste0("\u2248 ", format(round(energy()$engine / 3600, 2), big.mark = ","), " Wh")
  })

  output$fuel_energy <- renderText({
    mg_per_j <- power_source_co2[[input$power_source]]
    grams <- energy()$engine * mg_per_j / 1000
    paste0(format(round(grams, 1), big.mark = ","), " g")
  })

  output$energy_plot <- renderPlot({
    e <- energy()
    df <- data.frame(
      component = factor(c("Drag", "Rolling resistance", "Kinetic (to accelerate)"),
                          levels = c("Kinetic (to accelerate)", "Rolling resistance", "Drag")),
      energy_j = c(e$drag, e$rolling, e$kinetic)
    )

    ggplot(df, aes(x = energy_j, y = component)) +
      geom_col(fill = "#2c3e50") +
      geom_text(aes(label = format(round(energy_j), big.mark = ",")), hjust = -0.1) +
      labs(x = "Energy (J)", y = NULL) +
      scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
      theme_minimal(base_size = 14)
  })

  # Equations with current slider values substituted in, shown as readable text
  # below the chart (rather than crammed onto the bars themselves).
  output$eqn_text <- renderUI({
    e <- energy()
    d_m <- input$distance * 1000
    v <- input$speed
    total_weight <- e$total_weight

    fmt <- function(x, digits = 2) format(round(x, digits), big.mark = ",", trim = TRUE)

    eqn_row <- function(label, symbolic, substituted, result_j) {
      div(
        style = "flex: 1 1 260px; min-width: 240px; font-size: 1.05rem;",
        strong(label), br(),
        span(style = "font-family: monospace;", symbolic), br(),
        span(style = "font-family: monospace; color: #555;",
             paste0(substituted, " = ", fmt(result_j, 0), " J",
                    " (\u2248 ", fmt(result_j / 3600, 2), " Wh)"))
      )
    }

    div(
      style = "display: flex; flex-wrap: wrap; gap: 24px;",
      eqn_row(
        "Drag",
        "E = \u00bd \u00d7 \u03c1 \u00d7 Cd \u00d7 A \u00d7 v\u00b2 \u00d7 d",
        paste0("= \u00bd \u00d7 ", fmt(air_density), " \u00d7 ", fmt(input$cd), " \u00d7 ", fmt(input$frontal_area),
               " \u00d7 ", fmt(v, 1), "\u00b2 \u00d7 ", fmt(d_m, 0)),
        e$drag
      ),
      eqn_row(
        "Rolling resistance",
        "E = Cr \u00d7 m \u00d7 g \u00d7 d",
        paste0("= ", fmt(input$rolling_coef, 3), " \u00d7 ", fmt(total_weight, 0), " \u00d7 ", fmt(g, 2),
               " \u00d7 ", fmt(d_m, 0)),
        e$rolling
      ),
      eqn_row(
        "Kinetic energy",
        "E = \u00bd \u00d7 m \u00d7 v\u00b2",
        paste0("= \u00bd \u00d7 ", fmt(total_weight, 0), " \u00d7 ", fmt(v, 1), "\u00b2"),
        e$kinetic
      )
    )
  })
}

shinyApp(ui, server)
