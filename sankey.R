library(networkD3)
library(htmlwidgets)
library(webshot2)

# ---------------------------------------------------------------------------
# Kinetic energy Sankey - one acceleration from rest to a set speed.
# No drag, no rolling resistance, no stop-start cycles.
#
#   Fuel energy in -> Lost as heat
#                  -> Car total, incl. passenger -> Car
#                                                -> Person
#
# Kinetic energy = 0.5 * m * v^2, so:
#   - the car/person split is just the mass ratio (speed cancels out)
#   - the SIZE of the whole diagram scales with speed squared
#     (double the speed = four times the energy)
# Fuel energy in = kinetic energy needed / engine efficiency.
# ---------------------------------------------------------------------------

make_ke_sankey <- function(speed_kmh = 50,
                           vehicle_mass = 1500,      # kg
                           occupant_mass = 75,       # kg
                           loss_fraction = 0.70) {   # ICE ~0.70, EV ~0.15
  
  v <- speed_kmh / 3.6                               # m/s
  
  ke_car    <- 0.5 * vehicle_mass  * v^2 / 1000      # kJ
  ke_person <- 0.5 * occupant_mass * v^2 / 1000      # kJ
  ke_total  <- ke_car + ke_person
  
  fuel_in <- ke_total / (1 - loss_fraction)          # kJ
  lost    <- fuel_in - ke_total
  
  nodes <- data.frame(name = c(
    sprintf("Fuel energy in (%.0f kJ)", fuel_in),
    sprintf("Losses (%.0f kJ)", lost),
    sprintf("Bike total, incl. passenger (%.0f kJ)", ke_total),
    sprintf(paste("Bike (%.0f kJ)", vehicle_mass, "kg"), ke_car),
    sprintf(paste("Person (%.1f kJ)", occupant_mass,"kg"), ke_person)
  ))
  
  links <- data.frame(
    source = c(0, 0, 2, 2),
    target = c(1, 2, 3, 4),
    value  = c(lost, ke_total, ke_car, ke_person)
  )
  
  sankeyNetwork(Links = links, Nodes = nodes,fontFamily = "Helvetica",
                Source = "source", Target = "target", Value = "value",
                NodeID = "name", fontSize = 20, nodeWidth = 24,
                sinksRight = TRUE)
}

# kinetic energy sankey diagram for a car
sn = make_ke_sankey(speed_kmh = 50,
                    vehicle_mass = 1500,      # kg
                    occupant_mass = 80,       # kg
                    loss_fraction = 0.75)    # engine efficiency

dir.create("outputs/energy",recursive = TRUE)

# write html
saveWidget(sn, "outputs/energy/sankey.html", selfcontained = TRUE)
# convert to png
webshot("outputs/energy/sankey.html", "outputs/energy/sankey.png", vwidth = 1200, vheight = 700, zoom = 2)
