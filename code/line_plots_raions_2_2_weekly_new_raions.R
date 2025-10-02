# Load necessary libraries
library(dplyr)      # For data manipulation
library(ggthemes)   # For additional ggplot2 themes
library(ggplot2)    # For data visualization
library(forcats)    # For handling categorical data
library(showtext)

# Set working directory to the directory of the active R script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Define file path for raions data
raions_file_path <- file.path("data", "updated_raions_2.csv")

# Load fonts
font_add_google("Bitter","bit")
font_add_google("Roboto Condensed", "robotocondensed")
showtext_auto()

# Read data from CSV file into a data frame
adjusted_pop_sizes_for_raions <- read.csv(raions_file_path, header = TRUE)

# Named vectors for the translations
ADM1_translations <- c(
  "Cherkaska" = "Cherkasy", "Chernihivska" = "Chernihiv", "Chernivetska" = "Chernivtsi",
  "Autonomous Republic of Crimea" = "Autonomous Republic of Crimea", "Dnipropetrovska" = "Dnipropetrovsk",
  "Donetska" = "Donetsk", "Ivano-Frankivska" = "Ivano-Frankivsk", "Kharkivska" = "Kharkiv",
  "Khersonska" = "Kherson", "Khmelnytska" = "Khmelnytskyi", "Kyivska" = "Kyiv (Kyivska Oblast)",
  "Kyiv" = "Kyiv (City)", "Kirovohradska" = "Kirovohrad", "Lvivska" = "Lviv", "Luhanska" = "Luhansk",
  "Mykolaivska" = "Mykolaiv", "Odeska" = "Odesa", "Poltavska" = "Poltava", "Rivnenska" = "Rivne",
  "Sumska" = "Sumy", "Ternopilska" = "Ternopil", "Vinnytska" = "Vinnytsia", "Volynska" = "Volyn",
  "Zakarpatska" = "Zakarpattia", "Zaporizka" = "Zaporizhzhia", "Zhytomyrska" = "Zhytomyr"
)

ADM2_translations <- c(
  "Cherkaskyi" = "Cherkasy", "Zolotoniskyi" = "Zolotonosha", "Zvenyhorodskyi" = "Zvenyhorodka",
  "Umanskyi" = "Uman", "Nizhynskyi" = "Nizhyn", "Chernihivskyi" = "Chernihiv",
  "Prylutskyi" = "Pryluky", "Koriukivskyi" = "Koriukivka", "Novhorod-Siverskyi" = "Novhorod-Siverskyi",
  "Chernivetskyi" = "Chernivtsi", "Dnistrovskyi" = "Dnistrovskyi", "Vyzhnytskyi" = "Vyzhnytsia",
  "Perekopskyi" = "Perekop", "Bilohirskyi" = "Bilohirsk", "Kryvorizkyi" = "Kryvyi Rih",
  "Dniprovskyi" = "Dnipro", "Pavlohradskyi" = "Pavlohrad", "Kamianskyi" = "Kamianske",
  "Novomoskovskyi" = "Novomoskovsk", "Nikopolskyi" = "Nikopol", "Synelnykivskyi" = "Synelnykove",
  "Donetskyi" = "Donetsk", "Bakhmutskyi" = "Bakhmut", "Pokrovskyi" = "Pokrovsk",
  "Horlivskyi" = "Horlivka", "Kramatorskyi" = "Kramatorsk", "Mariupolskyi" = "Mariupol",
  "Kalmiuskyi" = "Kalmius", "Volnovaskyi" = "Volnovakha", "Ivano-Frankivskyi" = "Ivano-Frankivsk",
  "Kaluskyi" = "Kalush", "Kolomyiskyi" = "Kolomyia", "Nadvirnianskyi" = "Nadvirna",
  "Kosivskyi" = "Kosiv", "Verkhovynskyi" = "Verkhovyna", "Iziumskyi" = "Izium",
  "Lozivskyi" = "Lozova", "Bohodukhivskyi" = "Bohodukhiv", "Chuhuivskyi" = "Chuhuiv",
  "Kharkivskyi" = "Kharkiv", "Kupianskyi" = "Kupiansk", "Krasnohradskyi" = "Krasnohrad",
  "Beryslavskyi" = "Beryslav", "Khersonskyi" = "Kherson", "Kakhovskyi" = "Kakhovka",
  "Henicheskyi" = "Henichesk", "Skadovskyi" = "Skadovsk", "Shepetivskyi" = "Shepetivka",
  "Kamianets-Podilskyi" = "Kamianets-Podilskyi", "Khmelnytskyi" = "Khmelnytskyi", "Brovarskyi" = "Brovary",
  "Bilotserkivskyi" = "Bila Tserkva", "Obukhivskyi" = "Obukhiv", "Buchanskyi" = "Bucha",
  "Boryspilskyi" = "Boryspil", "Fastivskyi" = "Fastiv", "Chornobyl Exclusion Zone" = "Chornobyl Exclusion Zone",
  "Vyshhorodskyi" = "Vyshhorod", "Kyiv" = "Kyiv", "Kropyvnytskyi" = "Kropyvnytskyi",
  "Novoukrainskyi" = "Novoukrainka", "Holovanivskyi" = "Holovanivsk", "Oleksandriiskyi" = "Oleksandriia",
  "Zolochivskyi" = "Zolochiv", "Chervonohradskyi" = "Chervonohrad", "Drohobytskyi" = "Drohobych",
  "Lvivskyi" = "Lviv", "Yavorivskyi" = "Yavoriv", "Stryiskyi" = "Stryi",
  "Sambirskyi" = "Sambir", "Alchevskyi" = "Alchevsk", "Rovenkivskyi" = "Rovenky",
  "Svativskyi" = "Svatove", "Starobilskyi" = "Starobilsk", "Luhanskyi" = "Luhansk",
  "Sievierodonetskyi" = "Sievierodonetsk", "Shchastynskyi" = "Shchastia", "Dovzhanskyi" = "Dovzhansk",
  "Pervomaiskyi" = "Pervomaisk", "Bashtanskyi" = "Bashtanka", "Mykolaivskyi" = "Mykolaiv",
  "Voznesenskyi" = "Voznesensk", "Podilskyi" = "Podilsk", "Bolhradskyi" = "Bolhrad",
  "Berezivskyi" = "Berezivka", "Bilhorod-Dnistrovskyi" = "Bilhorod-Dnistrovskyi", "Odeskyi" = "Odesa",
  "Rozdilnianskyi" = "Rozdilna", "Izmailskyi" = "Izmail", "Lubenskyi" = "Lubny",
  "Poltavskyi" = "Poltava", "Myrhorodskyi" = "Myrhorod", "Kremenchutskyi" = "Kremenchuk",
  "Rivnenskyi" = "Rivne", "Dubenskyi" = "Dubno", "Sarnenskyi" = "Sarny",
  "Varaskyi" = "Varash", "Sumskyi" = "Sumy", "Konotopskyi" = "Konotop",
  "Shostkynskyi" = "Shostka", "Romenskyi" = "Romny", "Okhtyrskyi" = "Okhtyrka",
  "Ternopilskyi" = "Ternopil", "Chortkivskyi" = "Chortkiv", "Kremenetskyi" = "Kremenets",
  "Zhmerynskyi" = "Zhmerynka", "Haisynskyi" = "Haisyn", "Mohyliv-Podilskyi" = "Mohyliv-Podilskyi",
  "Khmilnytskyi" = "Khmilnyk", "Tulchynskyi" = "Tulchyn", "Vinnytskyi" = "Vinnytsia",
  "Lutskyi" = "Lutsk", "Volodymyrskyi" = "Volodymyr", "Kamin-Kashyrskyi" = "Kamin-Kashyrskyi",
  "Kovelskyi" = "Kovel", "Berehivskyi" = "Berehove", "Khustskyi" = "Khust",
  "Mukachivskyi" = "Mukachevo", "Uzhhorodskyi" = "Uzhhorod", "Rakhivskyi" = "Rakhiv",
  "Tiachivskyi" = "Tiachiv", "Berdianskyi" = "Berdiansk", "Vasylivskyi" = "Vasylivka",
  "Polohivskyi" = "Polohy", "Melitopolskyi" = "Melitopol", "Zaporizkyi" = "Zaporizhzhia",
  "Berdychivskyi" = "Berdychiv", "Zviahelskyi" = "Zviahel", "Zhytomyrskyi" = "Zhytomyr",
  "Korostenskyi" = "Korosten"
)

# Update the ADM1_EN column
adjusted_pop_sizes_for_raions$ADM1_EN <- ADM1_translations[adjusted_pop_sizes_for_raions$ADM1_EN]

# Update the ADM2_EN column
adjusted_pop_sizes_for_raions$ADM2_EN <- ADM2_translations[adjusted_pop_sizes_for_raions$ADM2_EN]

# Group and summarize data for raions to calculate total population changes
adjusted_pop_sizes_for_raions_1 <- adjusted_pop_sizes_for_raions %>%
  group_by(ADM1_EN, ADM2_EN, week_adjusted) %>%
  summarise(median_weekly_change = median(net_pop_change, na.rm = TRUE), .groups = "drop") %>%
  arrange(ADM1_EN, ADM2_EN, week_adjusted)

adjusted_pop_sizes_for_raions_1 <- adjusted_pop_sizes_for_raions_1[complete.cases(adjusted_pop_sizes_for_raions_1),]

# Create a new column ADM1_EN with updated names
adjusted_pop_sizes_for_raions_1$ADM1_EN <- gsub("'", "", adjusted_pop_sizes_for_raions_1$ADM1_EN) 

# Define a function for linear interpolation
lin_interp = function(x, y, length.out = 20000) {
  approx(x, y, xout = seq(min(x), max(x), length.out = length.out))$y
}

# Interpolate data for smoother plots
interpolated_data <- adjusted_pop_sizes_for_raions_1 %>%
  group_by(ADM1_EN, ADM2_EN) %>%
  reframe(
    week_interpolated = lin_interp(week_adjusted, week_adjusted),
    median_weekly_change_interpolated = lin_interp(week_adjusted, median_weekly_change)
  )

# Create a grouping variable for each positive/negative segment
cat.rle = rle(interpolated_data$median_weekly_change_interpolated < 0)
interpolated_data$group = rep.int(1:length(cat.rle$lengths), times = cat.rle$lengths)

# Add a column to indicate net outcome (positive/negative)
interpolated_data <- interpolated_data %>% mutate(net_outcome = if_else(group %% 2 == 1, "neg", "pos"))

# Identify key winners based on total net migration
key_winners <- adjusted_pop_sizes_for_raions_1 %>%
  group_by(ADM1_EN, ADM2_EN) %>%
  reframe(tot_net_migration = sum(median_weekly_change)) %>%
  mutate(combo = paste(ADM1_EN, ADM2_EN, sep = " - ")) %>%
  arrange(-tot_net_migration) %>%
  slice(1:10)

# Identify key losers based on total net migration
key_losers <- adjusted_pop_sizes_for_raions_1 %>%
  group_by(ADM1_EN, ADM2_EN) %>%
  reframe(tot_net_migration = sum(median_weekly_change)) %>%
  mutate(combo = paste(ADM1_EN, ADM2_EN, sep = " - ")) %>%
  arrange(tot_net_migration) %>%
  slice(1:10)

# Filter interpolated data for key winners and key losers
interpolated_data_winners <- interpolated_data %>%
  filter(ADM1_EN %in% key_winners$ADM1_EN, ADM2_EN %in% key_winners$ADM2_EN) %>%
  mutate(combo = paste(ADM1_EN, ADM2_EN, sep = " - "))

interpolated_data_losers <- interpolated_data %>%
  filter(ADM1_EN %in% key_losers$ADM1_EN, ADM2_EN %in% key_losers$ADM2_EN) %>%
  mutate(combo = paste(ADM1_EN, ADM2_EN, sep = " - "))

# Create a new column 'month_name' with month names
adjusted_pop_sizes_for_raions_x <- adjusted_pop_sizes_for_raions %>%
  select(month, day, week_adjusted) %>% 
  unique() %>% 
  mutate(month_name = month.abb[month]) %>%
  arrange(month, day)

first_dates <- adjusted_pop_sizes_for_raions_x %>%
  group_by(week_adjusted) %>%
  summarise_all(first)

last_dates <- adjusted_pop_sizes_for_raions_x %>%
  group_by(week_adjusted) %>%
  summarise_all(last)

combined_dates <- left_join(
  first_dates,
  last_dates,
  by = "week_adjusted"
)

# Function to generate date range
generate_week_starting <- function(month_name_first, day_first) {
  week_starting <- paste(month_name_first, day_first)
  return(week_starting)
}

# Apply the function to create the date_range column
combined_dates <- combined_dates %>%
  mutate(week_starting = mapply(generate_week_starting, month_name.x, day.x))

# Extract the date_range values for the specified weeks
weeks_of_interest <- c(1, 5, 10, 15, 20, 25)  # Update with the weeks you are interested in
date_ranges <- combined_dates$`week_starting`[combined_dates$week_adjusted %in% weeks_of_interest]

# Used this delineation to match previous figures
weeks_of_interest_2 <- c(2,6,10,15,19,24)
date_ranges_2 <- c("March 1", "April 1", "May 1", "June 1", "July 1", "August 1")

plot_line_plots <- function(interpolated_data_2, winners_losers) {
  result <- ggplot(
    interpolated_data_2,
    aes(x = week_interpolated, 
        y = median_weekly_change_interpolated,
        group = group
    )
  ) +
    geom_line(aes(color = factor(combo)), linewidth = 0) +
    geom_ribbon(
      aes(
        ymin = pmin(median_weekly_change_interpolated, 0),
        ymax = pmax(median_weekly_change_interpolated, 0),
        fill = net_outcome
      ),
      alpha = 1,
      position = "identity",
      show.legend = FALSE
    ) +
    labs(x = "Week", y = "Net Migration (Median)") +
    scale_y_continuous(
      labels = function(x)
        ifelse(x == 0, "0", scales::label_number(scale = 1e-6, suffix
                                                 
                                                 = "M")(x))
    ) +
    scale_fill_manual(values = c("lightblue", "lightcoral"),
                      name = "fill") +
    theme_tufte() +
    facet_wrap(
      ~factor(combo, levels = c(winners_losers$combo)),
      scales = "fixed",
      ncol = 1
    ) +  # Set the order of levels
    theme(text = element_text(family = "robotocondensed"),
          axis.title = element_text(size = 30, face ="plain"),
          axis.text = element_text(size = 23),
          strip.text = element_text(face = "plain", size = 23),
          legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))
  result <- result +
    scale_x_continuous(
      breaks = weeks_of_interest_2,
      labels = date_ranges_2
    )
  return(result)
}

# Plot line plots for winners and losers
winners_plot <- plot_line_plots(interpolated_data_winners, key_winners)
losers_plot <- plot_line_plots(interpolated_data_losers, key_losers)

png("../manuscript/figures/2_2/line_plot_hot_spots_weekly_2.png", units="in", width=6, height=20, res=300)
  winners_plot
dev.off()

png("../manuscript/figures/2_2/line_plot_cold_spots_weekly_2.png", units="in", width=6, height=20, res=300)
  losers_plot
dev.off()
