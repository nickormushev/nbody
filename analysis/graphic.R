library(ggplot2)
library(tidyverse)
require(scales)

theme_set(theme_bw())

graphic <- function(data, xlabel, ylabel, name) {
    ggplot(data, aes(x = numProc, y = calcTime, group = 1)) +
       geom_point() +
       geom_line() +
       scale_y_continuous(labels = comma) +
       ggtitle(name) +
       theme(plot.title = element_text(hjust = 0.5)) +
       xlab(xlabel) +
       ylab(ylabel)
}

#gets the minimum total time from the three attempts I made
get_min_total_time  <- function(total_time) {
    min_time_per_proc <- data.frame(numProc = numeric(), calcTime = numeric())
    for (numProc in c(1, 2, 4, 8, 16, 32)) {
        min_time <- max(
            total_time[total_time$processorCount == numProc &
            total_time$iteration == 1, ]$time)

        for (i in c(2, 3)) {
           min_time <- min(min_time, max(
                    total_time[total_time$processorCount == numProc &
                    total_time$iteration == i, ]$time)
           )
        }

        min_time_per_proc[nrow(min_time_per_proc) + 1, ] <-
            c(numProc, min_time)
    }

    min_time_per_proc
}

total_time200 <- read.csv("./200thou/data/totalTime200thou.csv")

min_time_per_proc <- data.frame(numProc = numeric(), calcTime = numeric())
names(min_time_per_proc)

min_time_per_proc  <-  get_min_total_time(total_time200)

#gets the max time for the processors and
#then the min total time for each attempt

graphic(min_time_per_proc, "Брой процеси", "Време в микросекунди",
    "Цялостно време за изпълнение на програмата с 200 хиляди тела")

ggsave("./200thou/png/200thouTime.png", width = 9)

calculation_time <- read.csv("./200thou/data/calculations200thou.csv")
head(calculation_time, n = 20)
iterations <- 10

min_calc_time <- data.frame(numProc = numeric(), calcTime = numeric())
colnames(calculation_time)

#take the m
for (numProc in c(1, 2, 4, 8, 16, 32)) {
    max_times <- c()

    for (i in c(1, 2, 3)) {
        calc_i <- calculation_time[calculation_time$numProc == numProc &
                                   calculation_time$iteration == i, ]

        #create a matrx where every row are the times
        #for each processor during a  different iteration
        m <- matrix(calc_i$time, nrow = iterations, ncol = numProc)

        #Takes the max time for a processor during a single unit of time
        max_times_for_attempt <- apply(m, 1, max, na.rm = T)
        #Takes the sum of the units of time
        max_times[i] <- sum(max_times_for_attempt)
    }

    max_times

    min_calc_time[nrow(min_calc_time) + 1, ] <- c(numProc, min(max_times))
}

min_calc_time
min_time_per_proc

overhead_vs_calc_time <- rbind(min_calc_time, min_time_per_proc)
overhead_vs_calc_time

overhead_vs_calc_time$cat <- c(rep("изчисление", 6), rep("изчисление плюс комуникация", 6))

ggplot(overhead_vs_calc_time, aes(x = numProc, y = calcTime, group = cat, col = cat)) +
   geom_point() +
   geom_line() +
   scale_y_continuous(labels = comma) +
   ggtitle("Времето за изчисление срещу времето за изчисление и комуникация при 200 хиляди тела") +
   theme(plot.title = element_text(hjust = 0.5)) +
   xlab("Брой процесори") +
   ylab("Време в микросекунди")

ggsave("./200thou/png/200thouCalcVsTotalTime.png", width = 10)


#Speedup calculation_time for one process/ calculation_time for n processes
min_time_per_proc$calcTime <- min_time_per_proc[min_time_per_proc$numProc == 1, ]$calcTime / min_time_per_proc$calcTime

graphic(min_time_per_proc, "Брой процеси", "Ускорение",
        "Ускорени на цялостното време за работа при 200 хиляди тела")

ggsave("./200thou/png/200thouTotalTimeSpeedup.png", width = 9)

#Efficency speedup/number of processes
min_time_per_proc$calcTime <- min_time_per_proc$calcTime / min_time_per_proc$numProc

graphic(min_time_per_proc, "Брой процеси", "Ефективност",
        "Ефективност на цялото време за работа при 200 хиляди тела")

ggsave("./200thou/png/200thouTotalTimeEfficency.png", width = 9)

#Barplot for calculations work per processor
calc16 <- calculation_time[calculation_time$numProc == 16, ]
colnames(calc16)

processor_avg <- matrix(nrow = 16, ncol = 3)

#Взимаме колко е работил всеки процесор през всяка итерация
for (i in c(1, 2, 3)) {
    it <- calc16[calc16$iteration == i, ]
    for (id in seq(0, 15)) {
       processor_avg[id + 1, i] <- sum(it[it$id == id, ]$time)
    }
}

#Средно колко е работил процесора на итерация
mean_per_processor <- apply(processor_avg, 1, mean, na.rm = T)

processor_avg <- data.frame(
    processor_id = seq(1, 16),
    avg_time = mean_per_processor
)

options(scipen = 999)

p <- ggplot(data = processor_avg,
            aes(x = as.factor(processor_id), y = avg_time)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_text(aes(label = round(avg_time)), vjust = 1.6,
              color = "white", size = 3.5) +
    xlab("Идентификатор на процесора") +
    ggtitle("Време прекарано в смятане на взаимодействия между телата") +
    theme(plot.title = element_text(hjust = 0.5)) +
    ylab("Време в миркосекунди")
p

ggsave("./200thou/png/timeSpentInCalculationsPerProcessor.png", width = 13, height = 7)


#Get diagram with data not only for 200 processors
total_time500 <- read.csv("./500thou/data/totalTime500thou.csv")
total_time100 <- read.csv("./100thou/data/totalTime100thou.csv")
total_time200 <- read.csv("./200thou/data/totalTime200thou.csv")

min_time500 <- get_min_total_time(total_time500)
min_time200 <- get_min_total_time(total_time200)
min_time100 <- get_min_total_time(total_time100)

min_time100$type <- "100 хиляди тела за 10 единици от време"
min_time500$type <- "500 хиляди тела за 5 единици от време"
min_time200$type <- "200 хиляди тела за 10 единици от време"
min_time100$speedup <- min_time100[min_time100$numProc == 1, ]$calcTime / min_time100$calcTime
min_time200$speedup <- min_time200[min_time200$numProc == 1, ]$calcTime / min_time200$calcTime
min_time500$speedup <- min_time500[min_time500$numProc == 1, ]$calcTime / min_time500$calcTime

total_time_all <- rbind(min_time100, min_time500)
total_time_all <- rbind(total_time_all, min_time200)

graphic_for_total_time <- function (data, gtitle, xlabel, ylabel) {
    ggplot(data, aes(x = numProc, y = toDisplay, group = type, col = type)) +
       geom_point() +
       geom_line() +
       scale_y_continuous(labels = comma) +
       ggtitle(gtitle) +
       theme(plot.title = element_text(hjust = 0.5)) +
       xlab(xlabel) +
       ylab(ylabel)
}

total_time_all$toDisplay <- total_time_all$calcTime

graphic_for_total_time(
        total_time_all,
        "Време за работа на програмата спрямо брой процеси",
        "Брой процеси",
        "Време в микросекунди")


ggsave("./png/totalTime.png", width = 9)
#Convert calc time to speedup
total_time_all$toDisplay <- total_time_all$speedup

graphic_for_total_time(
        total_time_all,
        "Ускорението на програмата спрямо брой процеси",
        "Брой процеси",
        "Ускорение")

ggsave("./png/totalSpeedup.png", width = 9)

#Convert calc time to efficiency
total_time_all$toDisplay <- total_time_all$speedup / total_time_all$numProc

graphic_for_total_time(
        total_time_all,
        "Ефективността на програмата спрямо брой процеси",
        "Брой процеси",
        "Ефективност")

ggsave("./png/totalEfficiency.png", width = 9)
