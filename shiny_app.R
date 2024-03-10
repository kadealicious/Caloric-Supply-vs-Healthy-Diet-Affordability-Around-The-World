# Created by Kade Samson
# Intro. to Data Science
# Prof. Brian Gill
# March 9th, 2024

library(tidyverse)
library(plotly)
library("rnaturalearth")
library(countrycode)
library(ggplot2)
library(shiny)

# The data we will be modeling!
caloric_supply					<- read_csv("daily-per-capita-caloric-supply.csv")
share_healthy_diet_unaffordable	<- read_csv("share-healthy-diet-unaffordable.csv")

# ============================================================================
# Choropleth map for daily supply of calories per person per country by year
# ============================================================================

# These things need to be done one time, so we do them here to minimize 
# update time when changing the slider on our graph.
world			<- ne_countries(scale = "medium", returnclass = "sf") |>
					filter(sovereignt != "Antarctica") |>
					select("sovereignt", "adm0_a3") |>
					left_join(codelist, by = c("adm0_a3" = "iso3c")) |>
					select("sovereignt", "adm0_a3", "continent")
map_theme		<- function() {
	theme(panel.background = element_blank(),
		axis.text = element_blank(),
		axis.ticks = element_blank(),
		axis.title = element_blank()
	)
}
font				<- list(family = "Arial", size = 15, color = "black")
tooltip_label_style	<- list(bgcolor = "white", font = font)


calorie_availability_map <- function(year, region) {
	# Filter our data by year.  Then, create a new data table storing the 
	# numeric value of avg. caloric intake for each country.
	calorie_year			<- caloric_supply |> filter(Year == year)
	calorie_spatial			<- left_join(world, calorie_year, by = c("adm0_a3" = "Code"))
	if(region != "World")	{calorie_spatial <- calorie_spatial |> filter(continent == region)}
	calorie_spatial			<- calorie_spatial |>
								mutate(calories_numeric = as.numeric(Calorie_Supply)) |>
								select("sovereignt", "Year", "calories_numeric")

	# Create the popup blurb.
	calorie_spatial$blurb	<- paste0("<b>", calorie_spatial$sovereignt, "</b> (",
										year, ")<br>Kcal/person/day: <i>",
										calorie_spatial$calories_numeric, "</i>")
	
	# Create the interactive map plot!
	interactive_map	<- ggplot(calorie_spatial, aes(text = blurb)) + 
						geom_sf(aes(fill = calories_numeric), lwd = 0.2, color = "black") +
						map_theme() +
						scale_fill_continuous(low = "#ddddff", high = "#0000ff") +
						labs(fill = "Caloric Supply")
	interactive_map	<- ggplotly(interactive_map, tooltip = "text") |>
						style(hoverlabel = tooltip_label_style)
	
	interactive_map
}


# ============================================================================
# Bar graph for share of population that cannot afford a healthy diet
# ============================================================================

healthy_diet_bar <- function(year, regions) {

	# Filter our data by the selected year and selected regions!
	unaffordable_diet_population_share	<- share_healthy_diet_unaffordable |>
											filter(Entity %in% regions) |>
											filter(Year == year)
	
	# Plot it!  This graph doesn't require any interactibility, so we just 
	# return a regular ggplot from this function.
	diet_plot	<- ggplot(data = unaffordable_diet_population_share,
					aes(x = Share_of_Population_Who_Cannot_Afford_Healthy_Diet,
						y = reorder(Entity, Share_of_Population_Who_Cannot_Afford_Healthy_Diet))) +
					geom_bar(stat = "identity", 
								fill = "#6666ff") +
					geom_text(aes(label = paste(Share_of_Population_Who_Cannot_Afford_Healthy_Diet, "%")), nudge_x = 4) +
					theme(panel.grid.minor.x = element_blank(),
							panel.grid.major.y = element_blank(),
							panel.grid.minor.y = element_blank()) +
					labs(x = "Share of Population That Cannot Afford a Healthy Diet", y = "") +
					xlim(c(0, 103))
	diet_plot
}


# ============================================================================
# Shiny app!
# ============================================================================

region_choices	<- list("World", "Americas", "Europe", "Asia", "Oceania", "Africa")
country_choices	<- unique(share_healthy_diet_unaffordable$Entity)

ui <- fluidPage(

	# App title.
	titlePanel("World Calorie Consumption vs. Nutritional Access"),

    # Create a tab selector panel for switching between visualizations.
    tabsetPanel(id = "tab",
		tabPanel(
			title = "Caloric Consumption", value = "Calories",
			wellPanel(selectInput("continent_filter",
				"Continent/Region",
				choices		= region_choices, 
				selected	= "World")),
			wellPanel(sliderInput("year_map",
				"Year: ",
				min		= 1800,
				max		= 2018,
				value	= 2018,
				sep		= ""))
		),
		
		tabPanel(
			title = "Nutritional Access", value = "Nutrition",
			wellPanel(sliderInput("year_bar",
				"Year: ",
				min		= 2017,
				max		= 2021,
				value	= 2021,
				sep		= "")),
			
			wellPanel(
				wellPanel(fluidRow(
					column(width = 2, checkboxInput("showhide_regions", "Show Regions Filter", value = FALSE)),
					column(width = 1, offset = 0, actionButton("reset_regions", "Reset Regions")))),
				conditionalPanel(condition = "input.showhide_regions == 1", checkboxGroupInput("regions_filter",
					"Selected Regions:",
					choiceNames		= country_choices,
					choiceValues	= country_choices,
					inline			= TRUE,
					select			= list("United States", "Malawi", "Ukraine",
											"World", "Vietnam", "Italy", "Iraq",
											"Burkina Faso", "Djibouti", "Sudan",
											"South Sudan", "United Kingdom",
											"Turkey", "Japan"))))
        )
    ),

	# Create the main panel template for rendering our plots to!
	mainPanel(
		width = "100%",
		h3(textOutput("TitleText")),
		h5(textOutput("SubtitleText")),
		plotlyOutput("InteractivePlot"),
		h5("Original data sources & maps by Our World in Data:", 
			tags$a(href="https://ourworldindata.org/grapher/daily-per-capita-
							caloric-supply?country=USA~IND~AUS~NGA", 
					"[Daily per-capita caloric supply]"),
			tags$a(href="https://ourworldindata.org/grapher/share-healthy-diet-
							unaffordable?tab=chart&country=Middle+East+%26+
							North+Africa+%28WB%29~North+America+%28WB%29~East+
							Asia+%26+Pacific+%28WB%29~Europe+%26+Central+Asia+
							%28WB%29~Sub-Saharan+Africa+%28WB%29~Latin+America+
							%26+Caribbean+%28WB%29", 
					"[Inaccessibility in healthy diets]"))
	)
)

# Define server logic required to create and render the graph.
server <- function(input, output, session) {

	observeEvent(input$reset_regions, {
		updateCheckboxGroupInput(session, "regions_filter", selected = list(""))
	})


	# Generate the correct title text depending on the current tab.
	output$TitleText <- renderText({
		graph_title <- if (input$tab == "Calories") {"Daily supply of calories per person,"}
						else {"Share of population that cannot afford a healthy diet,"}
		year		<- if (input$tab == "Calories") {input$year_map} else {input$year_bar}
		paste(graph_title, year)
	})

	# Generate the, you guessed it, correct subtitle text depending on the current tab.
	output$SubtitleText <- renderText({
		subtitle_text <- if (input$tab == "Calories") {"Measured in 
								kilocalories per person per day. This indicates the 
								caloric availability delivered to households but does 
								not necessarily indicate the number of calories actually 
								consumed."}
							else {"A diet is deemed unaffordable if it costs more 
								than 52% of a household's income. The cost of a 
								healthy diet is the lowest-cost set of foods available 
								that would meet requirements in dietary guidelines 
								from governments and public health agencies."}
		subtitle_text
	})

	# Generate the correct plot, depending on our current tab.  Finally!!!
	output$InteractivePlot <- renderPlotly({
		year			<- if (input$tab == "Calories") {input$year_map} else {input$year_bar}
		plot_to_render	<- if (input$tab == "Calories") {
			calorie_availability_map(year, input$continent_filter)
		} else {
			healthy_diet_bar(year, input$regions_filter)
		}
		plot_to_render
	})
}

# Run our shiny new application!
shinyApp(ui = ui, server = server)