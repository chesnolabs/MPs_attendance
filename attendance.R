library(data.table)
library(lubridate)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

count_attendance <- function(r) {
  r <- data.table(r)
  r[, all := length(session), by = name]
  r[, absent := sum(status == "Незареєстрований"), by = name]
  # Пограйся з коментами, якщо хочеш враховувати поважні причини відсутності чи навпаки
  #r[, absent_incl_good_reasons := all - sum(status == "Зареєстрований"), by = name]
  r[, present := all - absent, by = name] 
  #r[, present := sum(status == "Зареєстрований"), by = name] 
  r[, percent := present / all]
  ret <- unique(r[, c("MP_ID","name", "faction", "all", "absent", 
                      "present", "percent"), with = FALSE])
  ret[order(-ret$percent),]
}

plot_by_month <- function(d1 = NULL, d2 = NULL) {
  setwd("/home/pavlo/GitHub/MPs_attendance")
  setClass('myDate')
  setAs("character","myDate", function(from) as.Date(from, format="%d.%m.%Y"))   
  classes = c("factor", "factor", "myDate", "factor", "factor")
  reg <- read.csv("register.csv", header = FALSE, sep = "\t", colClasses = classes)
  names(reg) <- c("name","session","date","type", "status")
  if (is.null(d1)) {
    d1 <- min(reg$date)
  }
  if (is.null(d2)) {
    d2 <- max(reg$date)
  }
  reg <- reg[(reg$date >= d1) & (reg$date <= d2),]
  reg$name <- sapply(reg$name, simpleCap)
  reg$MP_ID <- getIDs(reg$name)
  reg$MP_ID[is.na(reg$MP_ID)] <- sapply(reg$name[is.na(reg$MP_ID)], IDs_NA_handle)
  reg <- get_factions(reg)
  pattern <- substr(reg$status[1],1,2)
  reg$status <- gsub(pattern, "", reg$status)
  reg$status[reg$status == "Зареєстрована"] <- "Зареєстрований"
  reg$status[reg$status == "Незареєстрована"] <- "Незареєстрований"
  reg$date <- as.Date(sapply(reg$date, floor_date, "month"))
  reg <- reg %>% 
    group_by(date) %>%
    mutate(session_number = n()) %>%
    mutate(non_present = sum(status == "Незареєстрований")) %>%
    mutate(percent = 1 - (non_present / session_number)) %>%
    select(date, percent) %>%
    distinct(.keep_all = TRUE)
  reg
}

simpleCap <- function(x) {
  x <- tolower(x)
  s <- strsplit(x, " ")[[1]]
  ret <- paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse=" ")
  s <- strsplit(ret, "-")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse="-")
}

getIDs <- function(x) {
  get_ID <- function(m) {
    ID <- full_name$person_id[full_name$person == m]
    if (length(ID) == 0) {
      NA
    } else {
      ID
    }
  }
  full_name <- read.csv("/home/pavlo/GitHub/declarations_refine/fullname_ids.csv")
  full_name$person <- gsub("'",'’', full_name$person)
  sapply(x, get_ID)
}

IDs_NA_handle <- function(x) {
  if (x == "Огнєвіч Злата Леонідівна") {
    return(24614)
  }
  if (x == "Найєм Мустафа-Масі ") {
    return(26308)
  }
  if (x == "Дубіль Валерій  Олександрович") {
    return(119)
  }
  if (x == "Крулько Іван Іванович") {
    return(1683)
  }
  if (x == "Данілін Владислав Юрійович") {
    return(0)
  }
  print('stop')
}

get_factions <- function(x) {
  load("/home/pavlo/GitHub/draftlaw_analysis/current_factions.Rda")
  new_data <- merge(x, current_factions, by = "MP_ID", all.x = TRUE)
  new_data$faction_title <- NULL
  fids <- read.csv("/home/pavlo/GitHub/draftlaw_analysis/faction_ids.csv")
  new_data <- merge(new_data, fids, by.x = "faction_id", by.y = "ID", all.x = TRUE)
  setnames(new_data, "faction_title", "faction")
  new_data[, c("MP_ID", "name", "faction", "session", "date", "type", "status")]
}

get_attendance_stat <- function(d1 = NULL, d2 = NULL) {
  setwd("/home/pavlo/GitHub/MPs_attendance")
  setClass('myDate')
  setAs("character","myDate", function(from) as.Date(from, format="%d.%m.%Y"))   
  classes = c("factor", "factor", "myDate", "factor", "factor")
  reg <- read.csv("register.csv", header = FALSE, sep = "\t", colClasses = classes)
  names(reg) <- c("name","session","date","type", "status")
  if (is.null(d1)) {
    d1 <- min(reg$date)
  }
  if (is.null(d2)) {
    d2 <- max(reg$date)
  }
  reg <- reg[(reg$date >= d1) & (reg$date <= d2),]
  reg$name <- sapply(reg$name, simpleCap)
  reg$MP_ID <- getIDs(reg$name)
  reg$MP_ID[is.na(reg$MP_ID)] <- sapply(reg$name[is.na(reg$MP_ID)], IDs_NA_handle)
  reg <- get_factions(reg)
  pattern <- substr(reg$status[1],1,2)
  reg$status <- gsub(pattern, "", reg$status)
  reg$status[reg$status == "Зареєстрована"] <- "Зареєстрований"
  reg$status[reg$status == "Незареєстрована"] <- "Незареєстрований"
  stat <- count_attendance(reg)
  stat
}

weekdays_stat <- function(d1 = NULL, d2 = NULL) {
  setwd("/home/pavlo/GitHub/MPs_attendance")
  setClass('myDate')
  setAs("character","myDate", function(from) as.Date(from, format="%d.%m.%Y"))   
  classes = c("factor", "factor", "myDate", "factor", "factor")
  reg <- read.csv("register.csv", header = FALSE, sep = "\t", colClasses = classes)
  names(reg) <- c("name","session","date","type", "status")
  if (is.null(d1)) {
    d1 <- min(reg$date)
  }
  if (is.null(d2)) {
    d2 <- max(reg$date)
  }
  reg <- reg[(reg$date >= d1) & (reg$date <= d2),]
  reg$name <- sapply(reg$name, simpleCap)
  reg$MP_ID <- getIDs(reg$name)
  reg$MP_ID[is.na(reg$MP_ID)] <- sapply(reg$name[is.na(reg$MP_ID)], IDs_NA_handle)
  reg <- get_factions(reg)
  pattern <- substr(reg$status[1],1,2)
  reg$status <- gsub(pattern, "", reg$status)
  reg$status[reg$status == "Зареєстрована"] <- "Зареєстрований"
  reg$status[reg$status == "Незареєстрована"] <- "Незареєстрований"
  reg$weekday <- weekdays(reg$date)
  reg <- reg %>%
   group_by(date) %>%
   mutate(all_numbers = n()) %>%
   group_by(date, status, all_numbers) %>%
   mutate(numbers_status = n()) %>%
   select(numbers_status, weekday, status) %>%
   distinct(.keep_all = T)
  reg <- reg %>%
   mutate(percent = round(numbers_status / all_numbers, 2)) %>%
   group_by(weekday, status) %>%
   summarise(M = mean(percent))
  reg
}

plot_attendance <- function() {
  A <- get_attendance_stat()
  A <- filter(A, !is.na(faction))
  A$faction <- as.character(A$faction)
  A$faction[grepl("Блок", A$faction)] <- "БПП"
  A$faction <- as.factor(A$faction)
  A$faction <- with(A, reorder(faction, faction, function(x) -length(x)))
  pl <- ggplot(A, aes(faction, present, color = faction, fill = faction)) + xlab(NULL) + ylab("Не прогуляно засідань")
  jit <- pl + geom_jitter(width = 0.15, alpha = 0.5, size = 2) + 
    theme_tufte() + theme(panel.grid = element_line(color = "grey"), legend.position = "none", axis.text.x = element_text(face = "bold"), panel.grid.minor = element_blank()) + 
    scale_y_continuous(breaks = c(0,3,6,9,12)) + 
    ggtitle("Відвідування у лютому за даними письмової реєстрації") +
    scale_color_manual( name = "Фракції", values = brewer.pal(9, "Set1")) 
  
  jit <- pl + geom_jitter(aes(x=''), width = 0.15, alpha = 0.5, size = 2) + 
    theme_tufte() + theme(panel.grid = element_line(color = "grey"), legend.position = "none", axis.text.x = element_text(face = "bold"), panel.grid.minor = element_blank()) + 
    scale_y_continuous(breaks = c(0,3,6,9,12)) + 
    scale_x_discrete(expand = c(0,0)) + 
    ggtitle("Відвідування у лютому за даними письмової реєстрації") +
    scale_color_manual( name = "Фракції", values = brewer.pal(9, "Set1")) +
    theme(text = element_text(size = 14), strip.text.x = element_text(face = "bold")) +
    facet_wrap(~faction, scales = "free_x")
  print(jit)
  viol <- pl + geom_violin(color = "grey") + 
    theme_tufte() + theme(panel.grid = element_line(color = "grey"), legend.position = "none", axis.text.x = element_text(face = "bold")) + 
    scale_y_continuous(breaks = c(0,3,6,9,12)) +
    scale_fill_manual( name = "Фракції", values = brewer.pal(9, "Set1"))
  print(viol)
}
