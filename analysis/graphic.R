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

total_time <- read.csv("./1mil/data/totalTime1mil.csv")

colnames(total_time)
total_time[total_time$processorCount == 32, ]
total_time

max(total_time[total_time$processorCount == 32 & total_time$iteration == 1, ])

mean_time_per_proc <- data.frame(numProc = numeric(), calcTime = numeric())
names(mean_time_per_proc)

for (numProc in c(1, 2, 4, 8, 16, 32)) {
    sum_time <- 0
    for (i in c(1, 2, 3)) {
       sum_time <- sum_time + max(
                total_time[total_time$processorCount == numProc &
                total_time$iteration == i, ]
       )
    }

    mean_time_per_proc[nrow(mean_time_per_proc) + 1, ] <-
        c(numProc, sum_time / 3)
}

mean_time_per_proc
graphic(mean_time_per_proc, "Брой процеси", "Време в микросекунди",
    "Цялостно време за изпълнение на програмата с 1 милион тела")

ggsave("./1milTime.png")

calculation_time <- read.csv("./1mil/data/calculations1mil.csv")

iterations <- 10

max_calc_time <- data.frame(numProc = numeric(), calcTime = numeric())

for (numProc in c(1, 2, 4, 8, 16, 32)) {
    calc_i <- calculation_time[calculation_time$numProc == numProc, ]

    #create a matrx where every row are the times
    #for each processor during a  different iteration
    m <- matrix(calc_i$time, iterations, numProc)

    #Takes the max time for an iteration
    max_times <- apply(m, 1, max, na.rm = T)
    max_calc_time[nrow(max_calc_time) + 1, ] <- c(numProc, mean(max_times))
}

graphic(max_calc_time, "Брой процеси", "Време в микросекунди",
        "Време за изчисление на взаимодействията между 1 милион тела")

ggsave("1milCalcTime.png")


#Speedup calculation_time for one process/ calculation_time for n processes
max_calc_time$calcTime  <- max_calc_time[max_calc_time$numProc == 1, ]$calcTime / max_calc_time$calcTime


graphic(max_calc_time, "Брой процеси", "Ускорение",
    "Ускорение на скоростта за изчисление на взаимодействията между 1 милион тела")
        

ggsave("1milCalcTimeSpeedup.png")

mean_time_per_proc$calcTime <- mean_time_per_proc[mean_time_per_proc$numProc == 1, ]$calcTime / mean_time_per_proc$calcTime


graphic(mean_time_per_proc, "Брой процеси", "Ускорение",
        "Ускорени на цялостното време за работа при 1 милион тела")

ggsave("1milTotalTimeSpeedup.png")

#Efficency speedup/number of processes
mean_time_per_proc$calcTime <- mean_time_per_proc$calcTime / mean_time_per_proc$numProc

graphic(mean_time_per_proc, "Брой процеси", "Ефективност",
        "Ефективност на цялото време за работа при 1 милион тела")

ggsave("1milTotalTimeEfficency.png")

max_calc_time$calcTime  <- max_calc_time$calcTime / max_calc_time$numProc

graphic(max_calc_time, "Брой процеси", "Ефективност",
        "Ефективност на изчеслиението на на взаимодействията между 1 милион тела")

ggsave("1milCalcEfficency.png")
