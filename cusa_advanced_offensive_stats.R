# This script grabs each C-USA team's advanced stats and puts them into a data frame. It then generates a gt for both offense and defense. Special teams to come!
#Initial set up

if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}
install.packages("cfbfastR")
pacman::p_load(tidyverse, cfbfastR, zoo, ggimage, gt)

#Grab each team in C-USA and put them into a df (can use this to loop eventually)
#cusa_teams <- cfbd_team_info(conference = "CUSA")

cusa_advanced_stats <- dplyr::bind_rows(
  cfbd_stats_season_advanced(2021, team="Charlotte"),
  cfbd_stats_season_advanced(2021, team="Florida Atlantic"),
  cfbd_stats_season_advanced(2021, team="Florida International"),
  cfbd_stats_season_advanced(2021, team="Louisiana Tech"),
  cfbd_stats_season_advanced(2021, team="Marshall"),
  cfbd_stats_season_advanced(2021, team="Middle Tennessee"),
  cfbd_stats_season_advanced(2021, team="North Texas"),
  cfbd_stats_season_advanced(2021, team="Old Dominion"),
  cfbd_stats_season_advanced(2021, team="Rice"),
  cfbd_stats_season_advanced(2021, team="Southern Mississippi"),
  cfbd_stats_season_advanced(2021, team="UAB"),
  cfbd_stats_season_advanced(2021, team="UTEP"),
  cfbd_stats_season_advanced(2021, team="UT San Antonio"),
  cfbd_stats_season_advanced(2021, team="Western Kentucky")
)

# Grab logos

logos <- read.csv("https://raw.githubusercontent.com/saiemgilani/cfbfastR-data/master/themes/logos.csv")
logos <- logos %>% dplyr::select(-.data$conference)

# Put everything into a single pretty df

offensive_stats <- cusa_advanced_stats %>% 
  dplyr::left_join(logos, by=c("team"="school")) %>%
  dplyr::select(logo,
                team,
                off_ppa,
                off_success_rate,
                off_explosiveness,
                off_power_success,
                off_stuff_rate,
                off_line_yds,
                off_pts_per_opp,
                off_field_pos_avg_start)

# A table with a million digits is hard to read, so let's round to two decimals

rounded_stats <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

offensive_stats <- rounded_stats(offensive_stats, 2)

# Now let's rename UTSA's team name to avoid UTSA Twitter's ire

offensive_stats[13, 2] = "UTSA"

# Draw the table and format it

draw_table <- function() {
  offensive_stats %>% gt() %>%
    tab_header(title = "Conference USA Advanced Stats - Offense", subtitle="Through Week 11 of the 2021 season") %>%
    #tab_source_note(source_note = md("**Table:** @JaredUTSA | **Data:** @CFB_Data with @cfbfastR v1.5.2")) %>%
    tab_source_note(source_note = html("<html><div id='note'>
                                       <p class='alignright'><b>Table:</b> @JaredUTSA | <b>Data:</b> @CFB_Data with @cfbfastR v1.5.2</p>
                                       <p class='alignleft'>Scan this QR code for a statistic glossary</p>
                                       </div>
                                       <style>.alignleft{float: left;}.alignright{float: right;} </style><div style='clear: both;'></div></html>")) %>%
    tab_source_note(source_note = html("<html><div><img src='https://i.ibb.co/0CxZwS5/frame.png' width='100' height='100'/></div></html>")) %>%
    text_transform(
      locations = cells_body(c(logo)),
      fn= function(logo){
        web_image(url= logo)
      }) %>%
    #cols_hide(columns = c(season, off_total_ppa, conference, team_id, mascot, abbreviation, alt_name1, alt_name2, alt_name3, division, color, alt_color, logo_dark, def_plays, def_drives, def_ppa, def_total_ppa,def_success_rate, def_explosiveness, def_power_success, def_stuff_rate, def_line_yds, def_line_yds_total, def_second_lvl_yds, def_second_lvl_yds_total, def_open_field_yds, def_open_field_yds_total, def_total_opportunities, def_pts_per_opp, def_field_pos_avg_start, def_field_pos_avg_predicted_points, def_havoc_total, def_havoc_front_seven, def_havoc_db, def_standard_downs_rate, def_standard_downs_ppa, def_standard_downs_success_rate, def_standard_downs_explosiveness, def_passing_downs_rate, def_passing_downs_ppa, def_passing_downs_total_ppa, def_passing_downs_success_rate, def_passing_downs_explosiveness, def_rushing_plays_rate, def_rushing_plays_ppa, def_rushing_plays_total_ppa, def_rushing_plays_success_rate, def_passing_downs_rate, def_passing_plays_ppa, def_passing_plays_success_rate, def_passing_plays_explosiveness, def_rushing_plays_explosiveness, def_passing_plays_rate, off_plays, off_drives, off_line_yds_total,off_second_lvl_yds, off_second_lvl_yds_total, off_open_field_yds, off_open_field_yds_total, off_total_opportunities, off_field_pos_avg_predicted_points, off_havoc_front_seven, off_havoc_db,off_passing_plays_explosiveness,off_passing_downs_success_rate, off_passing_plays_total_ppa, off_rushing_plays_rate, off_rushing_plays_explosiveness, off_rushing_plays_success_rate, off_rushing_plays_total_ppa, off_passing_downs_explosiveness, off_passing_downs_success_rate, off_passing_plays_rate, off_standard_downs_explosiveness, off_standard_downs_success_rate, off_standard_downs_rate, off_havoc_total, off_standard_downs_ppa, off_passing_downs_rate, off_passing_downs_ppa, off_rushing_plays_ppa, off_passing_downs_ppa, off_passing_plays_success_rate, off_passing_plays_ppa)) %>%
    tab_options(heading.align = "center") %>%
    tab_style(style = cell_borders(sides = "all", color = "#000000", style = "solid", weight = px(1)),
              locations = cells_body(
                columns = everything(),
                rows = everything()
              )
    ) %>%
    cols_align(
      align = c("center"),
      columns = everything()
    ) %>%
    
    cols_label(
      logo="",
      team="Team",
      off_ppa="PPA",
      off_success_rate="Success Rate",
      off_explosiveness="Explosiveness",
      off_power_success="Power Success",
      off_stuff_rate="Stuff Rate",
      off_line_yds="Line Yards",
      off_pts_per_opp="Finishing Drives",
      off_field_pos_avg_start="Field Position"
    ) %>%
    data_color(
      columns = c(off_ppa),
      colors = scales::col_numeric(
        palette = c("#FDFEFE","#28B463"),
        domain = c(-0.03,.35)
      )
    ) %>%
  #   ) %>%
  #   data_color(
  #     columns = c(off_success_rate),
  #     colors = scales::col_numeric(
  #       palette = c("#FDFEFE","#28B463"),
  #       domain = c(floor(off_success_rate),ceiling(off_success_rate))
  #     )
  #   ) %>%
  #   data_color(
  #     columns = c(off_explosiveness),
  #     colors = scales::col_numeric(
  #       palette = c("#FDFEFE","#28B463"),
  #       domain = c(floor(off_explosiveness),ceiling(off_explosiveness))
  #     )
  #   ) #%>%
  #   #data_color(
  #    # columns = c(off_power_success),
  #     #colors = scales::col_bin(
  #      # palette = c("#FDFEFE","#28B463"),
  #      # bins = 4,
  #       #domain = c(floor(off_power_success),ceiling(off_power_success))
  #    # )
  #  # )
  # %>%
  #   data_color(
  #     columns = c(off_line_yds),
  #     colors = scales::col_numeric(
  #       palette = c("#FDFEFE","#28B463"),
  #       domain = c(floor(off_line_yds),ceiling(off_line_yds))
  #     )
    #) 
   # %>%
    data_color(
      columns = c(off_pts_per_opp),
      colors = scales::col_numeric(
        palette = c("#FDFEFE","#28B463"),
        domain = c(2,5)
      )
    ) %>% 
    
    tab_options(
      table.font.size=px(22)
    )
  #%>%
  #   data_color(
  #     columns = c(off_stuff_rate),
  #     colors = scales::col_bin(
  #       palette = c("#28B463","#FDFEFE"),
  #       domain = c(floor(off_stuff_rate),ceiling(off_stuff_rate)),
  #       bins=4
  #     )
  #   ) %>%
  #   data_color(
  #     columns = c(off_field_pos_avg_start),
  #     colors = scales::col_numeric(
  #       palette = c("#28B463","#FDFEFE"),
  #       domain = c(floor(off_field_pos_avg_start),ceiling(off_field_pos_avg_start))
  #     )
  #   ) 
}

draw_table()
