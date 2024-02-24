# Footy Grid Script

# Usage instructions
# Each section creates a dataframe for different category sections
# At the end, all sections are binded together to write the final js file

install.packages('pacman') # install this package to make loading packages easier (only need to install once)

# This will load (and install if needed) all packages used
pacman::p_load(tidyverse, lubridate, rvest, httr, fitzRoy, htmltools, data.table, janitor)

# Load in AFL Tables Data ----
afltables <- fetch_player_stats_afltables(season = 1897:2023)  %>% 
  filter(!(Season==2020&Round==1)) %>% 
  bind_rows(readRDS("AFLTables2020R1.rds")) %>% 
  janitor::clean_names() %>% 
  mutate(
    first_name = case_when(season == 2022 & id == 0 ~ "Hewago", T ~ first_name),
    surname = case_when(season == 2022 & id == 0 ~ "Oea", T ~ surname),
    id = case_when(season == 2022 & id == 0 ~ 13010,T ~ id)
  ) %>% 
  mutate(disposals = kicks + handballs) %>% 
  mutate(home_result = case_when(
    home_score == away_score ~ "Draw",
    home_score > away_score ~ "Win",
    home_score < away_score ~ "Loss"
  )) %>% 
  mutate(away_result = case_when(
    home_score == away_score ~ "Draw",
    home_score < away_score ~ "Win",
    home_score > away_score ~ "Loss"
  )) %>% 
  mutate(result = ifelse(playing_for == home_team,home_result,away_result)) %>% 
  mutate(margin = ifelse(result == "Loss",-1,1)*abs(home_score-away_score)) %>% 
  mutate(fantasy = 3*kicks+2*handballs+3*marks+4*tackles+1*frees_for-3*frees_against+6*goals+1*behinds+1*hit_outs) %>%
  mutate(team_name_actual = playing_for) %>% 
  mutate(playing_for = case_match(
    playing_for,
    "Brisbane Bears" ~ "Brisbane Lions",
    "Fitzroy" ~ "Brisbane Lions",
    .default = playing_for
  )) %>% 
  arrange(date)

# AFL Official IDs ----

afl_details <- purrr::map_dfr(
  c(2001:2024),
  purrr::possibly(
    ~content(
      GET(
        paste0('https://api.afl.com.au/statspro/playersStats/seasons/CD_S',.x,'014'),
        config = httr::add_headers("x-media-mis-token" = fitzRoy::get_afl_cookie())
      ),as = 'text',encoding = 'UTF-8') %>%
      jsonlite::fromJSON(flatten = T) %>% .[['players']] %>% mutate(season = .x),otherwise = data.frame())
)


update_player_ids <- function(update = F) {
  
  if(!update) {
    return(readRDS("footygrid_ids.rds"))
  }
  
  # ID Matching ----
  afl_tables_debut_ids <- read.fwf("https://afltables.com/afl/stats/biglists/bg10.txt",
                                   widths = c(7,29,15,6,3,2,5,12),
                                   col.names = c("ID","Player","DOB","Round","Playing.for","V","Opponent","Date"),
                                   skip = 2) %>% 
    mutate(across(where(is.character), str_trim)) %>% 
    mutate(Playing.for = case_match(Playing.for, "FO" ~ "WB", "SM" ~ "SY", "KA" ~ "NM", "BB" ~ "BL", "FI" ~ "BL",  .default = Playing.for)) %>% 
    filter(!is.na(ID)) %>% 
    mutate(Date = as.Date(Date,format = "%d-%b-%C%y"))
  
  team_abbrev <- data.frame(
    "Playing.for" = c(sort(unique(afl_tables_debut_ids$Playing.for))),
    "debut_team" = c(sort(unique(afltables$playing_for))[c(1:6,8,7,9:17,19,18)]))
  
  players_full_join <- afltables %>% 
    arrange(date) %>% 
    group_by(id) %>% 
    summarise(
      Date = first(date),
      debut_team  = first(playing_for),
      Player = paste(last(first_name),last(surname)),
      .groups = 'drop'
    ) %>% 
    ungroup() %>% 
    left_join(team_abbrev, by = c("debut_team")) %>% 
    full_join(afl_tables_debut_ids %>% select(ID,Player,DOB,Round,Date,Playing.for), by = c("Playing.for","Player","Date"))
  
  players_fix_names <- players_full_join %>% 
    filter(is.na(ID)|is.na(id)) %>% 
    group_by(Date,Playing.for) %>% 
    mutate(across(c(id,debut_team,Player,ID,DOB,Round), ~ last(.x[!is.na(.x)]))) %>% 
    distinct()
  
  players_fix_debut <- players_fix_names %>% 
    filter(is.na(id)|is.na(ID)) %>% 
    group_by(Player,Playing.for) %>% 
    mutate(across(c(id,Date,debut_team,ID,DOB,Round), ~ last(.x[!is.na(.x)]))) %>% 
    ungroup() %>% distinct()
  
  players_final_join <- 
    rbind(players_full_join %>% filter(!is.na(ID)&!is.na(id)),
          players_fix_names %>% filter(!is.na(ID)&!is.na(id)),
          players_fix_debut %>% filter(!is.na(ID)&!is.na(id))) %>% 
    select(debut_id = ID, Player, date_of_birth = DOB, fitzRoy_afl_tables_id = id, debut_date = Date,debut_team) %>% 
    mutate(date_of_birth = as.Date(date_of_birth,format = "%d-%b-%C%y")) %>% 
    arrange(debut_id)
  
  afl_details %>% 
    group_by(playerId) %>% 
    summarise(
      Player = last(paste(max(playerDetails.givenName),playerDetails.surname)),
      date_of_birth = as.Date(last(playerDetails.dateOfBirth),tryFormats = c("%d/%m/%Y")),
      .groups = 'drop'
    ) -> afl_birthdays
  
  career_end <- afltables %>% 
    group_by(id) %>% 
    summarise(
      end = last(season),
      .groups = 'drop'
    )
  
  joined <- players_final_join %>% 
    filter(fitzRoy_afl_tables_id %in% career_end$id[career_end$end>=2001]) %>% 
    fuzzyjoin::stringdist_left_join(afl_birthdays, by = c("Player","date_of_birth"),distance_col = "dist") %>% 
    group_by(debut_id) %>% 
    slice_min(date_of_birth.dist,n=1) %>% 
    ungroup() %>% 
    group_by(debut_id) %>% 
    slice_min(Player.dist,n=1) %>% 
    ungroup() %>% 
    select(debut_id, Player = Player.x, date_of_birth = date_of_birth.x, fitzRoy_afl_tables_id, debut_date, debut_team, playerId, afl_name = Player.y, afl_date_of_birth = date_of_birth.y, first_player_dist = Player.dist, first_dob_dist = date_of_birth.dist)
  
  joined %>% 
    filter(is.na(playerId)|(first_dob_dist>=2&first_player_dist>=2)) %>% 
    fuzzyjoin::stringdist_left_join(afl_birthdays, by = c("Player","date_of_birth"),distance_col = "dist2",max_dist = 6) %>% 
    group_by(debut_id) %>% 
    slice_min(date_of_birth.dist2,n=1) %>% 
    slice_min(Player.dist2,n=1) %>% 
    filter(sum(Player.dist2,date_of_birth.dist2)<6|date_of_birth.dist2==0) %>%
    ungroup()  %>% 
    select(debut_id, Player = Player.x, date_of_birth = date_of_birth.x, fitzRoy_afl_tables_id, debut_date, debut_team, playerId = playerId.y, afl_name = Player.y, afl_date_of_birth = date_of_birth.y) -> second_pass
  
  join_worked <- joined %>% 
    filter(!is.na(playerId)&!(debut_id %in% second_pass$debut_id)) %>% 
    select(-contains("dist"))
  
  footygrid_ids <- rbind(
    players_final_join %>% filter(!(debut_id %in% c(join_worked$debut_id, second_pass$debut_id))) %>% mutate(playerId = NA, afl_name = NA, afl_date_of_birth = NA_Date_),
    join_worked,
    second_pass
  ) %>% 
    arrange(debut_id) %>% 
    left_join(afltables %>% 
                arrange(date) %>% 
                group_by(fitzRoy_afl_tables_id = id) %>% 
                summarise(
                  start = first(season),
                  end = last(season),
                  .groups = 'drop'
                ),
              by = c("fitzRoy_afl_tables_id")
    )
  
  write_rds(footygrid_ids, "footygrid_ids.rds")
  
  return(footygrid_ids)
}

footygrid_ids <- update_player_ids(update = F) # switch to TRUE if new players enter the system (wait for AFL Tables to update)


# CATEGORIES SECTION ----

playedFor <- afltables %>% 
  group_by(fitzRoy_afl_tables_id = id) %>% 
  reframe(
    category = paste0("playedFor",unique(playing_for)),
  )

# AFL Categories ----
afl_id_categories <- afl_details %>% 
  mutate(playerDetails.kickingFoot = ifelse(playerId == "CD_I296279","LEFT",playerDetails.kickingFoot)) %>% 
  mutate(playerDetails.kickingFoot = ifelse(is.na(playerDetails.kickingFoot)&season>=2013,"RIGHT",playerDetails.kickingFoot)) %>%
  mutate(playerDetails.stateOfOrigin = case_when(
    playerId == 'CD_I210043' ~ "WA",
    playerId == 'CD_I240707' ~ "VIC",
    playerId == 'CD_I240716' ~ "INT",
    playerId == 'CD_I260104' ~ "VIC",
    playerId == 'CD_I260258' ~ "VIC",
    playerId == 'CD_I270936' ~ "VIC",
    playerId == 'CD_I270985' ~ "WA",
    playerId == 'CD_I280300' ~ "SA",
    playerId == 'CD_I281292' ~ "SA",
    playerId == 'CD_I281375' ~ "SA",
    playerId == 'CD_I291912' ~ "VIC",
    playerId == 'CD_I291928' ~ "WA",
    playerId == 'CD_I293329' ~ "SA",
    playerId == 'CD_I294198' ~ "WA",
    playerId == 'CD_I980037' ~ "VIC",
    T ~ playerDetails.stateOfOrigin
  )) %>% 
  group_by(playerId) %>% 
  filter(sum(gamesPlayed)>0) %>% 
  ungroup() %>% 
  group_by(playerId) %>% 
  summarise(
    season100intercepts = sum(totals.intercepts>=100,na.rm=T)>0,
    playedIn2023 = sum(season==2023&gamesPlayed>0)>0,
    listedIn2023 = sum(season==2023)>0,
    playedIn2024 = sum(season==2024&gamesPlayed>0)>0,
    listedIn2024 = sum(season==2024)>0,
    leftFooter = sum(playerDetails.kickingFoot=="LEFT",na.rm=T)>0,
    rightFooter = sum(playerDetails.kickingFoot=="RIGHT",na.rm=T)>0,
    stateOfOriginVIC = sum(playerDetails.stateOfOrigin == "VIC",na.rm=T)>0,
    stateOfOriginNSW = sum(playerDetails.stateOfOrigin == "NSW",na.rm=T)>0,
    stateOfOriginSA = sum(playerDetails.stateOfOrigin == "SA",na.rm=T)>0,
    stateOfOriginWA = sum(playerDetails.stateOfOrigin == "WA",na.rm=T)>0,
    stateOfOriginQLD = sum(playerDetails.stateOfOrigin == "QLD",na.rm=T)>0,
    stateOfOriginTAS = sum(playerDetails.stateOfOrigin == "TAS",na.rm=T)>0,
    stateOfOriginNT = sum(playerDetails.stateOfOrigin == "NT",na.rm=T)>0,
    stateOfOriginINT = sum(playerDetails.stateOfOrigin == "INT",na.rm=T)>0,
    .groups = 'drop'
  ) %>% 
  left_join(footygrid_ids %>% select(fitzRoy_afl_tables_id,playerId), by = "playerId") %>% 
  select(-playerId) %>% 
  pivot_longer(cols = -fitzRoy_afl_tables_id, names_to = "category", values_to = "didit") %>% 
  filter(didit) %>%
  select(-didit)

# Categories ----

# Season stats
seasonStats <- afltables %>% 
  group_by(fitzRoy_afl_tables_id = id, season) %>% 
  summarise(
    won15matches = sum(result=="Win")>=15,
    lost15matches = sum(result == "Loss")>=15,
    season50goals = sum(goals,na.rm=T)>=50,
    season100goals = sum(goals,na.rm=T)>=100,
    season300kicks = sum(kicks,na.rm=T)>=300,
    season400disposals = sum(disposals,na.rm=T)>=400,
    season15behinds = sum(behinds,na.rm=T)>=15,
    season30freesAgainst = sum(frees_against,na.rm=T)>=30,
    season100inside50s = sum(inside_50s,na.rm=T)>=100,
    seasonaverage1goals = mean(goals,na.rm=T)>=1,
    seasonaverage2goals = mean(goals,na.rm=T)>=2,
    seasonaverage5hitouts = mean(hit_outs,na.rm=T)>=5,
    seasonaverage5marks = mean(marks,na.rm=T)>=5,
    seasonaverage5tackles = mean(tackles,na.rm=T)>=5,
    seasonaverage7marks = mean(marks,na.rm=T)>=7,
    seasonaverage8handballs = mean(handballs,na.rm=T)>=8,
    seasonaverage25disposals = mean(disposals,na.rm=T)>=25,
    seasonaverage5rebound50s = mean(rebounds,na.rm=T)>=5,
    seasonaverage100fantasy = mean(fantasy,na.rm=T)>=100,
    seasonaverageUnder10disposals = mean(disposals,na.rm=T)<10,
    seasonaverageUnder15disposals = mean(disposals,na.rm=T)<15,
    seasonaverage2kickhandballratio = (sum(kicks,na.rm=T)/sum(handballs,na.rm=T))>=2,
    seasonaverage5clearances = mean(clearances,na.rm=T)>=5,
    seasonaverage3clangers = mean(clangers,na.rm=T)>=3,
    seasonaverage1bounces = mean(bounces,na.rm=T)>=1,
    seasonaverage1marksInside50 = mean(marks_inside_50,na.rm=T)>=1,
    seasonaverage1contestedMarks = mean(contested_marks,na.rm=T)>=1,
    seasonaverage5contestedposs = mean(contested_possessions,na.rm=T)>=5,
    seasonaverage10contestedposs = mean(contested_possessions,na.rm=T)>=10,
    seasonaverage15uncontestedposs = mean(uncontested_possessions,na.rm=T)>=15,
    seasonaverage20uncontestedposs = mean(uncontested_possessions,na.rm=T)>=20,
    seasonaverage7onePercenters = mean(one_percenters,na.rm=T)>=7,
    seasonaverage1goalAssists = mean(goal_assists,na.rm=T)>=1,
    season5brownlowVotes = sum(brownlow_votes,na.rm=T)>=5,
    season10brownlowVotes = sum(brownlow_votes,na.rm=T)>=10,
    season15brownlowVotes = sum(brownlow_votes,na.rm=T)>=15,
    season20brownlowVotes = sum(brownlow_votes,na.rm=T)>=20,
    .groups = 'drop'
  ) %>%
  select(-season) %>% 
  group_by(fitzRoy_afl_tables_id) %>% 
  summarise(
    across(where(is.logical), ~ sum(.x,na.rm=T)>0),
    .groups = 'drop'
  ) %>% 
  pivot_longer(cols = -fitzRoy_afl_tables_id, names_to = "category", values_to = "didit") %>% 
  filter(didit) %>%
  select(-didit)

# Season Rankings
seasonRankings <- afltables %>% 
  group_by(fitzRoy_afl_tables_id = id, season) %>% 
  summarise(
    across(
      c(kicks,handballs,disposals,goals,brownlow_votes,marks,tackles,contested_possessions,uncontested_possessions,inside_50s,clearances),
      ~ sum(.x,na.rm=T)
    ),
    .groups = 'drop'
  ) %>%
  pivot_longer(cols = -c(fitzRoy_afl_tables_id,season), names_to = "stat", values_to= "total") %>% 
  filter(total>0) %>% 
  group_by(season,stat) %>% 
  slice_max(total,n=10) %>% 
  ungroup() %>% 
  mutate(category = paste0("top10",str_remove_all(stat,"_"),"Season")) %>% 
  distinct(fitzRoy_afl_tables_id,category)

# Team Season Rankings ----
teamSeasonRankings <- afltables %>% 
  group_by(fitzRoy_afl_tables_id = id,season,playing_for=team_name_actual) %>%
  summarise(
    across(
      c(kicks,handballs,disposals,goals,brownlow_votes,marks,tackles,contested_possessions,uncontested_possessions,inside_50s,clearances),
      ~ sum(.x,na.rm=T)
    ),
    .groups = 'drop'
  ) %>% 
  pivot_longer(cols = -c(fitzRoy_afl_tables_id,season,playing_for), names_to = "stat", values_to= "total") %>% 
  filter(total>0) %>% 
  group_by(season,playing_for,stat) %>% 
  slice_max(total,n=1) %>% 
  ungroup() %>% 
  mutate(category = paste0("teamMost",str_remove_all(stat,"_"),"Season")) %>% 
  distinct(fitzRoy_afl_tables_id,category)

# Win/loss streaks

win_streaks <- afltables %>% 
  arrange(id,date) %>% 
  group_by(id,run = data.table::rleid(result)) %>% 
  summarise(
    length = n(),
    result = first(result),
    .groups = 'drop'
  ) %>% 
  filter(length>=10) %>% 
  distinct(fitzRoy_afl_tables_id = id,category = result) %>% 
  mutate(category = paste0("consecutive10",tolower(ifelse(category == "Loss","Losse",category)),"s"))
  
# Career Stats ----
careerStats <- afltables %>% 
  group_by(fitzRoy_afl_tables_id=id) %>% 
  summarise(
    match30disposals = sum(disposals>=30,na.rm=T)>0,
    match35disposals = sum(disposals>=35,na.rm=T)>0,
    match40disposals = sum(disposals>=40,na.rm=T)>0,
    match10marks = sum(marks>=10,na.rm=T)>0,
    final10marks = sum(marks>=10&grepl("F",round),na.rm=T)>0,
    grandFinal10marks = sum(marks>=10&grepl("GF",round),na.rm=T)>0,
    match10tackles = sum(tackles>=10,na.rm=T)>0,
    final10tackles = sum(tackles>=10&grepl("F",round),na.rm=T)>0,
    grandFinal10tackles = sum(tackles>=10&grepl("GF",round),na.rm=T)>0,
    final1goals = sum(goals>=1&grepl("F",round),na.rm=T)>0,
    final2goals = sum(goals>=2&grepl("F",round),na.rm=T)>0,
    grandFinal1goals = sum(goals>=1&grepl("GF",round),na.rm=T)>0,
    grandFinal2goals = sum(goals>=2&grepl("GF",round),na.rm=T)>0,
    match3goals = sum(goals>=3,na.rm=T)>0,
    match5goals = sum(goals>=5,na.rm=T)>0,
    match5inside50s = sum(inside_50s>=5,na.rm=T)>0,
    match10clearances = sum(clearances>=10,na.rm=T)>0,
    match5clangers = sum(clangers>=5,na.rm=T)>0,
    match10clangers = sum(clangers>=10,na.rm=T)>0,
    match10contestedPossessions = sum(contested_possessions>=10,na.rm=T)>0,
    match15contestedPossessions = sum(contested_possessions>=15,na.rm=T)>0,
    match20contestedPossessions = sum(contested_possessions>=20,na.rm=T)>0,
    match15uncontestedPossessions = sum(uncontested_possessions>=15,na.rm=T)>0,
    match20uncontestedPossessions = sum(uncontested_possessions>=20,na.rm=T)>0,
    match1bounces = sum(bounces>=1,na.rm=T)>0,
    match3bounces = sum(bounces>=3,na.rm=T)>0,
    match5goalAssists = sum(goal_assists>=5,na.rm=T)>0,
    match5contestedMarks = sum(contested_marks>=5,na.rm=T)>0,
    match5marksInside50 = sum(marks_inside_50>=5,na.rm=T)>0,
    match10marksInside50 = sum(marks_inside_50>=10,na.rm=T)>0,
    match10onePercenters = sum(one_percenters>=10,na.rm=T)>0,
    match15onePercenters = sum(one_percenters>=15,na.rm=T)>0,
    final25disposals = sum(grepl("F",round)&disposals>=25,na.rm=T)>0,
    final30disposals = sum(grepl("F",round)&disposals>=30,na.rm=T)>0,
    grandFinal25disposals = sum(grepl("GF",round)&disposals>=25,na.rm=T)>0,
    grandFinal30disposals = sum(grepl("GF",round)&disposals>=30,na.rm=T)>0,
    debut1goal = first(goals)>=1,
    debut20disposals = first(disposals[!is.na(disposals)])>=20,
    lastGame1goal = last(goals)>=1,
    career50games = n()>=50,
    career100games = n()>=100,
    career200games = n()>=200,
    career250games = n()>=250,
    lessThan20games = n()<20,
    lessThan50games = n()<50,
    career250goals = sum(goals,na.rm=T)>=250,
    career500tackles = sum(tackles,na.rm=T)>=500,
    career2500kicks = sum(kicks,na.rm=T)>=2500,
    career3000disposals = sum(disposals,na.rm=T)>=3000,
    careerUnder1000disposals = sum(disposals,na.rm=T)<1000,
    career0bounces = max(season)>=1999&sum(bounces,na.rm=T)==0,
    won80games = sum(result == "Win",na.rm=T)>=80,
    woreSingleDigitGuernsey = sum(parse_number(jumper_no) %in% 1:9, na.rm=T)>0,
    woreDoubleDigitGuernsey = sum(parse_number(jumper_no) %in% 10:99,na.rm=T)>0,
    woreGuernsey15orAbove = sum(parse_number(jumper_no) %in% 15:99,na.rm=T)>0,
    woreGuernsey15orBelow = sum(parse_number(jumper_no) %in% 1:15,na.rm=T)>0,
    neverScoredaGoal = sum(goals,na.rm=T)==0,
    oneClubPlayer = n_distinct(playing_for)==1,
    multiClubPlayer = n_distinct(playing_for)>=2,
    threeClubPlayer = n_distinct(playing_for)>=3,
    decadeEighties = sum(between(season,1980,1989))>0,
    decadeNineties = sum(between(season,1990,1999))>0,
    decadeNoughties = sum(between(season,2000,2009))>0,
    decadeTwentyTens = sum(between(season,2010,2019))>0,
    decadeTwentyTwenties = sum(between(season,2020,2029))>0,
    playedInElimFinal = sum(round == "EF")>0,
    playedInPrelimFinal = sum(round == "PF")>0,
    playedInGrandFinal = sum(round == "GF")>0,
    margin100pointsWin = sum(margin>=100)>0,
    margin100pointsLoss = sum(margin<=-100)>0,
    wonFinalsMatch = sum(grepl("F",round)&result == "Win")>0,
    wonGrandFinal = sum(round == "GF"&result=="Win")>0,
    lostGrandFinal = sum(round == "GF"&result == "Loss")>0,
    playedInDrawnMatch = sum(result=="Draw")>0,
    playedIn5Finals = sum(grepl("F",round))>=5,
    playedInUnder5Finals = sum(grepl("F",round))<5,
    neverPlayedInAFinal = sum(grepl("F",round))==0,
    namedJack = first(first_name)=="Jack",
    namedJohn = first(first_name)=="John",
    .groups = 'drop'
  ) %>%
  pivot_longer(cols = -fitzRoy_afl_tables_id, names_to = "category", values_to = "didit") %>% 
  filter(didit) %>%
  select(-didit)

age <- afltables %>% 
  arrange(date) %>% 
  select(fitzRoy_afl_tables_id = id, date) %>% 
  left_join(footygrid_ids %>% select(fitzRoy_afl_tables_id,Player,date_of_birth),by = "fitzRoy_afl_tables_id") %>% 
  mutate(age = lubridate::time_length(difftime(date,date_of_birth),"years")) %>% 
  filter(!is.na(date_of_birth)) %>% 
  group_by(fitzRoy_afl_tables_id) %>% 
  summarise(
    ageDebutAsTeenager = sum(age<20,na.rm=T)>0,
    ageNoGamesOver30Y = sum(age>=30,na.rm=T)==0,
    ageMatureDebutOver22Y = min(age,na.rm=T)>=22,
    .groups = 'drop'
  ) %>% 
  pivot_longer(cols = -fitzRoy_afl_tables_id, names_to = "category", values_to = "didit") %>% 
  filter(didit) %>%
  select(-didit)

# Played With ----
special <- c(
  30,    # Andrew McLeod
  1083,  # Simon Black
  1456,  # Kade Simpson
  4182,  # Scott Pendlebury
  304,   # Dustin Fletcher
  544,   # Matthew Pavlich
  11538, # Joel Selwood
  1105,  # Gary Ablett jr
  11677, # Callan Ward
  4065,  # Lance Franklin
  2447,  # Nathan Jones
  788,   # Brent Harvey
  1126,  # Shaun Burgoyne
  11576, # Jack Riewoldt
  930,   # Robert Harvey
  1012,  # Adam Goodes
  4169,  # Josh J Kennedy
  422,   # Chris Grant
  # retirees
  11805, # Luke Shuey
  11666, # Trent Cotchin
  11835, # Ben Cunnington
  12075, # Andrew Phillips
  4162,  # Shannon Hurn
  12041, # Aaron Hall
  11724, # Jack Ziebell
  11941, # Isaac Smith
  11768, # Nic Naitanui
  11576, # Jack Riewoldt
  12035, # Paul Seedsman
  11825, # Phil Davis
  12065, # Josh Bruce
  # fantasy pigs
  12196, # Tom Mitchell
  12217, # Brodie Grundy
  1460,  # Dane Swan
  11787, # Tom Rockliff
  11966, # Max Gawn
  # features
  12159, # Dylan Buckley
  11924, # Daniel Gorringe
  # current common players
  11548, # Tom Hawkins
  11954, # Luke Breust
  11953, # Luke Parker
  11973, # Zach Tuohy
  11904, # Andrew Gaff
  11686, # Todd Goldstein
  11700, # Patrick Dangerfield
  11583, # Travis Boak
  12034, # Brandon Ellis
  12076, # Dayne Zorko
  12055, # Lachie Neale
  12060, # Bradley Hill
  12054, # Adam Treloar
  12017, # Jeremy Cameron
  12269, # Marcus Bontempelli
  12250, # James Aish
  12025, # Toby Greene
  4088,  # David Mundy
  12368, # Jack Steele
  12167, # Jake Stringer
  4144,  # Patrick Ryder
  12165, # Rory Laird
  12430, # Christian Petracca
  11967, # Jeremy Howe
  12330, # Adam Saad
  12261, # Patrick Cripps
  12406, # Darcy Parish
  12582, # Paddy Dow
  12481, # Brayden Fiorini
  12028, # Stephen Coniglio
  12067, # Jarryd Lyons
  990,   # Tony Lockett
  11633, # Kurt Tippett
  4151,  # Bernie Vince
  12074, # Josh Jenkins
  12779, # Luke Jackson
  11605, # Will Schofield
  11710, # Mitch Robinson
  1403,  # Brian Taylor
  11809, # Michael Barlow
  12546, # Alex Witherden
  12277, # Charlie Cameron
  857,   # Kane Cornes
  11539, # Xavier Ellis
  4068,  # Brett Deledio
  336,   # Matthew Lloyd
  12408, # Josh Dunkley
  4132,  # Heath Shaw
  12170, # Sam Docherty
  695,   # Garry Lyon
  3940,  # Adam Cooney
  12410, # Clayton Oliver
  12276, # Blake Acres
  119,   # Jason Akermanis
  1122,  # Chris Judd
  12322, # Angus Brayshaw 
  12589, # Andrew Brayshaw
  12692, # Bailey Smith
  12684, # Sam Walsh
  12862, # Errol Gulden
  981, # Nick Riewoldt
  11794, # Dustin Martin
  604, # Matthew Scarlett
  1443, # Brendon Goddard
  421, # Brad Johnson
  217, # Nathan Buckley
  12485, # Mabior Chol
  12687, # Charlie Constable
  312, # James Hird 
  12452, # Tom Phillips
  759, # David King
  11834, # Nat Fyfe
  4, # Mark Riccuto
  1092, # Jonathan Brown
  1134, # Luke Hodge
  11644, # Scott Selwood
  11538, # Joel Selwood
  102, # Michael Voss
  1135, # Sam Mitchell
  766 # Wayne Carey
)

# Played With function ----

players200games <- afltables %>% 
  group_by(id) %>% 
  filter(n()>=200,max(season)>=1990) %>% 
  ungroup() %>% 
  pull(id) %>% 
  unique()

# All current players with 20+ games
currentPlayers <- afltables %>% 
  filter(
    id %in% (footygrid_ids$fitzRoy_afl_tables_id[footygrid_ids$playerId %in% afl_details$playerId[afl_details$season==max(afl_details$season,na.rm = T)]])
  ) %>% 
  group_by(id) %>% 
  filter(n()>=20) %>% 
  ungroup() %>% 
  pull(id) %>% 
  unique()

playedWith <- purrr::map_dfr(
  unique(c(currentPlayers,players200games)),
  ~
    afltables %>% 
    group_by(season,round,date,home_team,away_team,playing_for) %>% 
    filter(sum(id==.x)>0) %>% 
    ungroup() %>% 
    mutate(playedWith = first(paste(first_name,surname)[id == .x])) %>% 
    filter(id != .x) %>% 
    select(id ,playedWith) %>% 
    distinct(id,.keep_all = T) %>% 
    mutate(playedWith = 
             case_when(
               .x == 567 ~ "Gary Ablett Sr",
               .x == 1105 ~ "Gary Ablett Jr",
               .x == 11636 ~ "Scott D Thompson",
               .x == 12836 ~ "Bailey J Williams",
               .x == 672 ~ "Joel M Smith",
               .x == 4169 ~ "Josh J Kennedy",
               .x == 11672 ~ "Josh P Kennedy",
               T ~ playedWith
             ))
    ) %>% 
  rename(fitzRoy_afl_tables_id = id,category = playedWith) %>% 
  distinct() %>% 
  mutate(category = paste0("playedWith", str_replace_all(category, " ", "")))

# Played Against

against <- c(
  4115, # Ryan Crowley
  617, # Cameron Ling
  857, # Kane Cornes
  1015, # Brett Kirk
  426, # Tony Liberatore
  4145, # Josh Gibson
  11733, # Alex Rance
  12273, # Jeremy McGovern
  604, # Matthew Scarlett
  304, # Dustin Fletcher
  12332, # James Sicily
  12371, # Brayden Maynard
  12402 # Jacob Weitering
)

playedAgainst <- purrr::map_dfr(
  against,
  ~ afltables %>% 
    mutate(playedAgainst = first(paste(first_name,surname)[id == .x])) %>% 
    group_by(season,round,date,home_team,away_team) %>% 
    filter(sum(id==.x)>0, playing_for!=first(playing_for[id == .x])) %>% 
    ungroup() %>% 
    filter(id != .x) %>% 
    select(id, playedAgainst) %>% 
    distinct(id,.keep_all = T)) %>% 
  rename(fitzRoy_afl_tables_id = id,category = playedAgainst) %>% 
  distinct() %>% 
  mutate(category = paste0("playedAgainst", str_replace_all(category, " ", "")))
  
# Coached by ----
coach_games <- readRDS("coaches.rds")

if(rvest::read_html(x = "https://afltables.com/afl/stats/coaches/coaches_idx.html") |>
   rvest::html_element(xpath = '//table[@class="sortable"]') %>% 
   html_table() %>%
   head(-1) %>% 
   janitor::row_to_names(row_number =1) %>% 
   suppressWarnings() %>% 
   janitor::clean_names() %>% 
   mutate(across(w:gf, ~ parse_number(.x)))  %>% 
   pull(t_3) %>% sum() != nrow(coach_games)) {
  source('load_coaches.R')
  write_rds(coach_games, "coaches.rds")
}

coachedBy <- afltables %>% 
  left_join(coach_games, by = c("season"="Season","round"="Round","playing_for" = "Team"),relationship = "many-to-many") %>% 
  filter(Coach %in% c("Mick Malthouse", "Ross Lyon","Brett Ratten","Brad Scott","Michael Voss","Alastair Clarkson","John Worsfold","Kevin Sheedy","Terry Wallace","Leigh Matthews","Ron Barassi","John Longmire","Craig McRae","Sam Mitchell","Justin Longmuir","Matthew Nicks","Adam Kingsley","Simon Goodwin","Chris Fagan","Luke Beveridge","Adam Simpson","Ken Hinkley","Damien Hardwick","Rodney Eade")) %>% 
  distinct(fitzRoy_afl_tables_id=id,category = Coach) %>% 
  mutate(category = paste0("coachedBy",str_replace_all(category," ","")))

# Old Brownlows ----
# brownlow_old_years <- purrr::map_dfr(
#   read_html("https://afltables.com/afl/brownlow/brownlow_idx.html") %>%
#     html_element("table") %>%
#     html_table() %>%
#     head(-1) %>%
#     filter(X1<1984) %>%
#     pull(X1) %>%
#     unique(),
#   ~ read_html(paste0("https://afltables.com/afl/brownlow/brownlow",.x,".html")) %>%
#     html_element("table") %>%
#     html_table() %>%
#     select(1:3) %>%
#     mutate(Year = as.integer(.x)) %>%
#     mutate(Player = sub(x=Player,"(.*),\\s(.*)","\\2 \\1"))
# ) %>%
#   left_join(footygrid_ids %>% select(fitzRoy_afl_tables_id,Player,start,end),by = "Player",relationship = "many-to-many") %>%
#   filter(between(Year,start,end)) %>%
#   left_join(
#     fetch_player_stats_afltables(season = 1924:1983) %>%
#       group_by(fitzRoy_afl_tables_id = ID,Year=Season) %>%
#       reframe(
#         Club = unique(Playing.for)
#       ),
#     by = c("fitzRoy_afl_tables_id","Year"),relationship = "many-to-many") %>%
#   mutate(Teams = str_replace_all(Teams, c("Footscray" = "Western Bulldogs", "South Melbourne" = "Sydney"))) %>%
#   rowwise() %>%
#   filter(grepl(Club,Teams)) %>%
#   ungroup() %>%
#   distinct(Year,fitzRoy_afl_tables_id,Player,Club,Votes)
# 
# write_rds(brownlow_old_years, file = "historical_brownlow_votes.rds")


# Awards ----
brownlow_old_years <- readRDS("historical_brownlow_votes.rds") %>% 
  group_by(Year) %>% 
  mutate(RankSeason = rank(-Votes,ties.method = "min")) %>% 
  ungroup() %>% 
  group_by(Year,Club) %>% 
  mutate(RankClubSeason = rank(-Votes,ties.method = "min")) %>% 
  ungroup() %>% 
  group_by(fitzRoy_afl_tables_id) %>% 
  summarise(
    season5brownlowVotes = sum(Votes>=5,na.rm=T)>0,
    season10brownlowVotes = sum(Votes>=10,na.rm=T)>0,
    season15brownlowVotes = sum(Votes>=15,na.rm=T)>0,
    season20brownlowVotes = sum(Votes>=20,na.rm=T)>0,
    top10brownlowvotesSeason = sum(RankSeason<=10,na.rm=T)>0,
    teamMostbrownlowvotesSeason = sum(RankClubSeason==1,na.rm=T)>0,
    .groups = 'drop'
  ) %>% 
  pivot_longer(cols = -fitzRoy_afl_tables_id, names_to = "category", values_to = "didit") %>% 
  filter(didit) %>%
  select(-didit) 

brownlow <- read_html("https://afltables.com/afl/brownlow/brownlow_idx.html") %>% 
  html_elements("table") %>%
  .[[1]] %>% 
  html_table() %>% 
  filter(nchar(X1)==4) %>% 
  setNames(c("Year","Player","Team","Votes","Games","3","2","1","Polled","Average")) %>% 
  mutate(across(-c(Player,Team), ~ parse_number(.x))) %>% 
  left_join(footygrid_ids %>% select(1:4,start,end),by = "Player",relationship = "many-to-many") %>% 
  filter(between(Year,start,end)) %>% 
  mutate(category = "wonBrownlow") %>% 
  distinct(fitzRoy_afl_tables_id,category)

coleman <- read_html("https://afltables.com/afl/stats/alltime/leadinggk.html") %>% 
  html_elements("table") %>%
  .[[1]] %>% 
  html_table() %>% 
  janitor::row_to_names(row_number = 1) %>% 
  suppressWarnings() %>%
  select(Year = 1, Player = 2, Goals = 3, Team = 4) %>% 
  distinct() %>% 
  mutate(across(c("Year","Goals"), ~ parse_number(.x))) %>% 
  filter(Year!=2024) %>% 
  left_join(footygrid_ids %>% select(1:4,start,end) %>% filter(debut_id != 11677),by = "Player",relationship = "many-to-many") %>% 
  filter(between(Year,start,end)) %>% 
  mutate(category = "wonColeman") %>% 
  distinct(fitzRoy_afl_tables_id,category)

risingStar <- xlsx::read.xlsx("FootyGridAwards.xlsx",sheetName = "RisingStarNomination") %>%
  left_join(footygrid_ids %>% select(1:4,start,end,debut_team), by = "Player",relationship = "many-to-many") %>% 
  filter(between(Year,start,end)) %>% 
  group_by(Year,Round,Player) %>% 
  filter(n()==1|debut_team==Club) %>% 
  ungroup() %>% 
  arrange(Year,Round) %>% 
  mutate(award = "risingStarNomination") %>% 
  select(fitzRoy_afl_tables_id,Player,award,Won) %>% 
  mutate(Won = ifelse(!is.na(Won),"risingStarWinner",Won)) %>% 
  pivot_longer(cols = award:Won,names_to = "names",values_to = "category") %>% 
  filter(!is.na(category)) %>% 
  select(-names) %>% 
  distinct(fitzRoy_afl_tables_id, category) 

allAustralian <- purrr::map_dfr(
  c(2023:1991), 
  ~read_html(paste0("https://www.draftguru.com.au/awards/all-australian/",.x)) %>% 
    html_element("table") %>% 
    html_table() %>% 
    janitor::row_to_names(row_number = 1) %>% 
    mutate(Season = .x)
) %>% 
  select(Season,Position,Player,Club) %>% 
  mutate(Player = gsub(x=Player,"'","")) %>% 
  mutate(Player = gsub(x=Player,"\u00A0"," ",fixed=T)) %>% 
  mutate(Player = gsub(x=Player,"Nathan Fyfe","Nat Fyfe")) %>% 
  left_join(footygrid_ids %>% select(1:4,start,end,debut_team), by = "Player",relationship = "many-to-many") %>%
  filter(between(Season,start,end)) %>% 
  filter(!(Position == "FF"&fitzRoy_afl_tables_id==11672),!(grepl("C",Position)&fitzRoy_afl_tables_id==4169),!(fitzRoy_afl_tables_id %in% c(737,11865)),!(Position=="FB"&fitzRoy_afl_tables_id==755),!(Position=="RR"&fitzRoy_afl_tables_id==11636)) %>% 
  mutate(category = "allAustralian") %>%
  distinct(fitzRoy_afl_tables_id,category)

aflPA_MVP <- read_html("https://en.wikipedia.org/wiki/Leigh_Matthews_Trophy") %>% 
  html_elements("table") %>% 
  .[[1]] %>% 
  html_table() %>% 
  select(Year,Player) %>% 
  mutate(Player = ifelse(Player == "Luke DarcyMichael Voss","Luke Darcy",Player)) %>% 
  mutate(Player = str_replace(Player, " [SJ]r\\.$","")) %>% 
  left_join(footygrid_ids %>% select(1:4,start,end),by = "Player",relationship = "many-to-many") %>%
  filter(between(Year,start,end)) %>% 
  mutate(category = "afl_players_association_MVP") %>% 
  distinct(fitzRoy_afl_tables_id,category)

club_bnf <- purrr::map_dfr(
  read_html("https://www.draftguru.com.au/awards") %>% 
    html_elements("a") %>% 
    html_attr("href") %>% 
    .[grepl("best-and-fairest",.)], 
  ~ read_html(paste0("https://www.draftguru.com.au/",.x)) %>% 
    html_elements("table") %>% 
    html_table()
) %>% 
  select(Year, Player) %>% 
  mutate(Player = gsub(x=Player,"\u00A0"," ",fixed=T)) %>%
  mutate(Player = gsub(x=Player,"'","")) %>% 
  mutate(Player = case_match(
    Player,
    "Nathan Fyfe" ~ "Nat Fyfe",
    "Matt Rendell" ~ "Matthew Rendell",
    "Brad Hill" ~ "Bradley Hill",
    "Steve Malaxos" ~ "Stephen Malaxos",
    .default = Player
  )) %>% 
  rbind(
    data.frame(
      Year = c(2023), 
      Player = c("Jordan Dawson","Jacob Weitering","Caleb Serong","Will Day","Harry Sheezel","Tim Taranto","Zak Butters"))
    ) %>% 
  left_join(footygrid_ids %>% select(1:4,start,end) %>% filter(!(debut_id %in% c(11872,9446,11492))),by = "Player",relationship = "many-to-many") %>%
  filter(between(Year,start,end)) %>% 
  filter(!(debut_id == 11642&Year<2013), !(debut_id == 11139&Year==2013)) %>% 
  mutate(category = "club_best_and_fairest") %>% 
  distinct(fitzRoy_afl_tables_id,category)

normSmith <- read_html("https://www.draftguru.com.au/awards/norm-smith-medal") %>% 
  html_elements("table") %>% 
  .[[1]] %>% 
  html_table() %>% 
  select(Year,Player) %>% 
  mutate(Player = gsub(x=Player,"\u00A0"," ",fixed=T)) %>%
  mutate(Player = gsub(x=Player,"'","")) %>%
  distinct() %>% 
  left_join(footygrid_ids %>% select(1:4,start,end),by = "Player",relationship = "many-to-many") %>% 
  filter(between(Year,start,end)) %>% 
  mutate(category = "normSmithMedal") %>% 
  select(fitzRoy_afl_tables_id, category)

top_50_goalkicker <- afltables %>% 
  group_by(id,first_name,surname) %>% 
  summarise(
    goals = sum(goals,na.rm=T),
    .groups = 'drop'
  ) %>% 
  slice_max(goals,n=50) %>% 
  mutate(category = "top_50_goalkicker") %>% 
  select(fitzRoy_afl_tables_id=id,category)

grandFinalSprint <- xlsx::read.xlsx("FootyGridAwards.xlsx",sheetName = "GrandFinalSprint") %>%
  left_join(footygrid_ids %>% select(1:4), by = "Player",relationship = "many-to-many") %>% 
  mutate(category = "grandFinalSprint") %>% 
  distinct(fitzRoy_afl_tables_id,category)

# Draft Details from Draft Guru ----
draftCategories <- purrr::map_dfr(read_html("https://www.draftguru.com.au/picks") %>% 
                                 html_elements("a") %>% 
                                 html_attr("href") %>% 
                                 .[grepl("^/picks/",.)],
                               ~ read_html(paste0("https://www.draftguru.com.au",.x)) %>% 
                                 html_elements("table") %>% 
                                 .[[1]] %>% 
                                 html_table() %>% 
                                 rename(Type = 2) %>% 
                                 mutate(Games = as.character(Games)) %>% 
                                 mutate(pick = gsub("/picks/","",.x))
) %>% 
  separate(pick, into = c("Pick","Draft"),sep = "/",convert=T) %>% 
  suppressWarnings() %>%
  mutate(Draft = ifelse(is.na(Draft),"national",Draft)) %>% 
  mutate(Player = gsub(x=Player,"\u00A0"," ",fixed=T)) %>%
  mutate(Player = gsub(x=Player,"'","")) %>% 
  filter(nchar(Player)>0,Games!="0") %>% 
  mutate(Player = case_match(
    Player, 
    "Nathan Fyfe" ~ "Nat Fyfe",
    "Matthew Flynn" ~ "Matt Flynn",
    "Jordan De Goey" ~ "Jordan de Goey",
    "Cameron McCarthy" ~ "Cam McCarthy",
    "Matthew Scharenberg" ~ "Matt Scharenberg",
    "Joshua Weddle" ~ "Josh Weddle",
    "Matt Johnson" ~ "Matthew Johnson",
    "Lachlan Fogarty" ~ "Lachie Fogarty",
    "Ian Hill" ~ "Bobby Hill",
    "Brad Hill" ~ "Bradley Hill",
    "Harrison Petty" ~ "Harry Petty",
    "Jay Kennedy-Harris" ~ "Jay Kennedy Harris",
    "Elliot Himmelberg" ~ "Elliott Himmelberg",
    "Matthew Capuano" ~ "Mathew Capuano",
    "Patrick Kerr" ~ "Pat Kerr",
    "Adrian Deluca" ~ "Adrian De Luca",
    "Matt Rendell" ~ "Matthew Rendell",
    "Steven Schwerdt" ~ "Stephen Schwerdt",
    "Setanta Ó hAilpín" ~ "Setanta OhAilpin",
    "Harrison Jones" ~ "Harry Jones",
    "Mitch Hibberd" ~ "Mitchell Hibberd",
    "Bradley Lynch" ~ "Brad Lynch",
    "Ollie Hanrahan" ~ "Oliver Hanrahan",
    "Matt Cottrell" ~ "Matthew Cottrell",
    "Darcy MacPherson" ~ "Darcy Macpherson",
    "Ciarán Sheehan" ~ "Ciaran Sheehan",
    "Ciarán Byrne" ~ "Ciaran Byrne",
    "Lachie Hosie" ~ "Lachlan Hosie",
    "Cameron Sutcliffe" ~ "Cam Sutcliffe",
    .default = Player
  )) %>% 
  select(Year,Type,Player,Draft,Pick,Club,Games) %>% 
  mutate(row = row_number()) %>% 
  left_join(footygrid_ids %>% select(1:4,start,end,debut_team) %>% filter(!(debut_id %in% c(0))) %>% filter(start>=1960),by = "Player",relationship = "many-to-many") %>% 
  filter(!(debut_id == 10477&Year==2003),!(debut_id == 10985&Year == 1999),!(debut_id == 12436&Year==2007),!(debut_id == 10185&Year==1988),!(debut_id %in% c(9910,10292,9692)&Year==1989),!(debut_id == 9566&Year==1987),!(debut_id == 9227&Year == 1988),!(debut_id == 11547&Year!=2006),!(debut_id == 12449&Year==2018),!(debut_id == 12836&Year==2015),!(debut_id %in% c(13038,11991) & Year == 2006),!(debut_id == 12960 & Year == 2009),!(debut_id == 11711&Year == 2009),!(debut_id == 11886 & Year!=2009),!(debut_id == 12909&Year == 2016),!(debut_id == 9947&Year == 1989)) %>% 
  mutate(diff = end-Year,diffStart = start-Year) %>% 
  filter(!(Player %in% c("Harry Jones","Michael Gardiner","Josh Kennedy","Nathan Brown","Tom Lynch","Jesse Smith","Andrew Browne","Chris Waterson","Scott Thompson","Ben Thompson","David King","Nick Smith"))|Club==debut_team) %>% 
  filter(diff>=0,diffStart<=11) %>% 
  mutate(Type = ifelse(nchar(Type)==0,NA,Type)) %>% 
  mutate(Games = as.integer(str_replace(Games," .*",""))) %>% 
  select(fitzRoy_afl_tables_id,draft_year = Year, draft = Draft,draft_team = Club, special = Type, pick = Pick) %>% 
  mutate(draft_year = ifelse(draft == "mid-season"&draft_year>=2018,draft_year+1,draft_year)) %>% 
  group_by(fitzRoy_afl_tables_id) %>% 
  summarise(
    draftPick1 = sum(pick == 1)>0,
    draftTop5 = sum(pick <= 5)>0,
    draftTop10 = sum(pick <= 10)>0,
    draftTop10National = sum(pick <= 10 & draft == "national")>0,
    draftRookie = sum(draft == "rookie")>0,
    draftMidSeason = sum(draft == "mid-season")>0,
    draftPreSeason = sum(draft == "pre-season")>0,
    draftClass2015 = sum(draft_year==2015,na.rm=T)>0,
    draftClass2016 = sum(draft_year==2016,na.rm=T)>0,
    draftClass2017 = sum(draft_year==2017,na.rm=T)>0,
    draftClass2018 = sum(draft_year==2018,na.rm=T)>0,
    draftClass2019 = sum(draft_year==2019,na.rm=T)>0,
    draftClass2020 = sum(draft_year==2020,na.rm=T)>0,
    draftClass2021 = sum(draft_year==2021,na.rm=T)>0,
    draftClass2022 = sum(draft_year==2022,na.rm=T)>0,
    .groups = 'drop'
  ) %>% 
  pivot_longer(cols = -fitzRoy_afl_tables_id, names_to = "category", values_to = "didit") %>% 
  filter(didit) %>%
  select(-didit)

# Player Details from AFL Tables ----

fetch_player_details_afltables <- function(team = NULL) {
  if (is.null(team)) {
    cli_all <- cli::cli_process_start("Fetching player details for all teams")
    
    teams <- c(
      "Adelaide", "Brisbane Lions", "Brisbane Bears",
      "Carlton", "Collingwood", "Essendon", "Fitzroy",
      "Fremantle", "GWS", "Geelong", "Gold Coast",
      "Hawthorn", "Melbourne", "North Melbourne",
      "Port Adelaide", "Richmond", "St Kilda",
      "Sydney", "West Coast", "University",
      "Western Bulldogs"
    )
    
    get_player_details_afltables <- function(team) {
      cli_team <- cli::cli_process_start("Fetching player details for {team}")
      valid_team <- fitzRoy:::team_check_afltables(team)
      
      team_abr <- dplyr::case_when(
        team == "Adelaide" ~ "adelaide",
        team == "Brisbane Lions" ~ "brisbanel",
        team == "Brisbane Bears" ~ "brisbaneb",
        team == "Carlton" ~ "carlton",
        team == "Collingwood" ~ "collingwood",
        team == "Essendon" ~ "essendon",
        team == "Fitzroy" ~ "fitzroy",
        team == "Fremantle" ~ "fremantle",
        team == "GWS" ~ "gws",
        team == "Geelong" ~ "geelong",
        team == "Gold Coast" ~ "goldcoast",
        team == "Hawthorn" ~ "hawthorn",
        team == "Melbourne" ~ "melbourne",
        team == "North Melbourne" ~ "kangaroos",
        team == "Kangaroos" ~ "kangaroos",
        team == "Port Adelaide" ~ "padelaide",
        team == "Richmond" ~ "richmond",
        team == "St Kilda" ~ "stkilda",
        team == "Sydney" ~ "swans",
        team == "South Melbourne" ~ "swans",
        team == "West Coast" ~ "westcoast",
        team == "University" ~ "university",
        team == "Western Bulldogs" ~ "bullldogs",
        team == "Footscray" ~ "bullldogs",
        TRUE ~ ""
      )
      
      url <- paste0("https://afltables.com/afl/stats/alltime/", team_abr, ".html")
      html <- rvest::read_html(url)
      
      df <- html %>%
        rvest::html_table() %>%
        purrr::pluck(1) %>%
        dplyr::mutate(Team = team) %>%
        dplyr::slice(1:dplyr::n() - 1) %>%
        tidyr::separate("Games (W-D-L)",
                        into = c("Games", "Wins", "Draws", "Losses", "x"),
                        fill = "right"
        ) %>%
        dplyr::select(-'x') %>%
        dplyr::mutate(date_accessed = Sys.Date()) %>%
        tidyr::separate("Player",
                        into = c("surname", "firstname"),
                        sep = ",", fill = "right"
        ) %>%
        dplyr::mutate(Player = paste0(
          trimws(.data$firstname),
          " ",
          trimws(.data$surname)
        )) %>%
        dplyr::mutate(dplyr::across(
          dplyr::one_of(c(
            "Cap", "Games", "Wins", "Draws",
            "Losses", "Goals"
          )),
          as.numeric
        )) %>%
        dplyr::select(
          "Player", 
          "Team",
          dplyr::everything(),
          -"surname", 
          -"firstname"
        ) %>%
        dplyr::arrange(.data$Cap)
      
      cli::cli_process_done(cli_team)
      return(df)
    }
    
    details_data <- teams %>%
      purrr::map_dfr(get_player_details_afltables)
    
    cli::cli_process_done(cli_all)
    
    return(details_data)
  } else {
    details_data <- get_player_details_afltables(team)
    
    return(details_data)
  }
}

height_and_weight <- fetch_player_details_afltables() %>% 
  mutate(date_of_birth = as.Date(DOB)) %>% 
  mutate(height_cm = parse_number(HT),weight_kg = parse_number(WT)) %>% 
  distinct(Player,height_cm,weight_kg,date_of_birth) %>% 
  left_join(footygrid_ids %>% select(fitzRoy_afl_tables_id,Player,date_of_birth), by = c("date_of_birth","Player")) %>% 
  select(fitzRoy_afl_tables_id,Player,height_cm,weight_kg,date_of_birth) %>% 
  group_by(fitzRoy_afl_tables_id) %>% 
  summarise(
    heightUnder175cm = first(height_cm)<=175,
    heightUnder185cm = first(height_cm)<=185,
    heightOver195cm = first(height_cm)>=195,
    .groups = 'drop'
  ) %>% 
  pivot_longer(cols = -fitzRoy_afl_tables_id, names_to = "category", values_to = "didit") %>% 
  filter(didit) %>%
  select(-didit)

# Final data write ----

categories <- bind_rows(
  playedFor,
  seasonStats,
  seasonRankings,
  teamSeasonRankings,
  careerStats,
  afl_id_categories,
  playedWith,
  playedAgainst,
  coachedBy,
  win_streaks,
  height_and_weight,
  age,
  brownlow, 
  brownlow_old_years,
  coleman, 
  risingStar, 
  allAustralian, 
  aflPA_MVP, 
  club_bnf, 
  normSmith, 
  top_50_goalkicker, 
  grandFinalSprint,
  draftCategories
) %>% 
  mutate(category = case_match(
    category,
    "decadeNineties" ~ "nineties",
    "decadeNoughties" ~ "noughties",
    "decadeEighties" ~ "eighties",
    "final10tackles" ~ "tackles10final",
    "final10marks" ~ "marks10final",
    "final1goals" ~ "goals1final",
    "final30disposals" ~ "disposals30final",
    "final25disposals" ~ "disposals25final",
    "match30disposals" ~ "disposals30match",
    "match35disposals" ~ "disposals35match",
    "match40disposals" ~ "disposals40match",
    "match10tackles" ~ "tackles10match",
    "match10marks" ~ "marks10match",
    "seasonaverage5rebound50s" ~ "seasonaverage5rebounds",
    "grandFinal10marks" ~ "marks10grandfinal",
    "grandFinal1goals" ~ "goals1grandfinal",
    "grandFinal2goals" ~ "goals2grandfinal",
    "grandFinal25disposals" ~ "disposals25grandfinal",
    "grandFinal30disposals" ~ "disposals30grandfinal",
    "grandFinal10tackles" ~ "tackles10grandfinal",
    "career50games" ~ "games50career",
    "career100games" ~ "games100career",
    "career200games" ~ "games200career",
    "career250games" ~ "games250career",
    "heightUnder185cm" ~ "shorterThan185",
    "heightOver195cm" ~ "tallerThan195",
    "leftFooter" ~ "leftDominant",
    "draftTop10" ~ "top10Draft",
    "playedInGrandFinal" ~ "grandFinal",
    .default = category
  )) %>% #pull(category) %>% unique() %>% sort()
  group_by(fitzRoy_afl_tables_id) %>% 
  nest(categories = category) %>% 
  ungroup()

footygrid_main <- footygrid_ids %>% 
  left_join(categories, by = "fitzRoy_afl_tables_id")

#write_rds(footygrid_main, "footygrid_main.rds")
#write_rds(footygrid_main, "Fantasy App/Fantasy App/footygrid_tester.rds")

paste("const allPlayerIDs =",jsonlite::toJSON(footygrid_main, dataframe = "columns", auto_unbox = T),"\nmodule.exports=allPlayerIDs;") %>%  write(file = "C:/Users/jpopo/Documents/GitHub/ids/allPlayerIDs.js")