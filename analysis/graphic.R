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

total_time <- read.csv("./200thou/data/totalTime200thou.csv")

mean_time_per_proc <- data.frame(numProc = numeric(), calcTime = numeric())
names(mean_time_per_proc)

#sum the min total time for each attempt and then take the mean
for (numProc in c(1, 2, 4, 8, 16, 32)) {
    sum_time <- 0
    for (i in c(1, 2, 3)) {
       sum_time <- sum_time + min(
                total_time[total_time$processorCount == numProc &
                total_time$iteration == i, ]$time
       )
    }

    mean_time_per_proc[nrow(mean_time_per_proc) + 1, ] <-
        c(numProc, sum_time / 3)
}

mean_time_per_proc

graphic(mean_time_per_proc, "Брой процеси", "Време в микросекунди",
    "Цялостно време за изпълнение на програмата с 200 хиляди тела")

ggsave("./200thou/png/200thouTime.png", width = 9)

calculation_time <- read.csv("./200thou/data/calculations200thou.csv")

iterations <- 10

min_calc_time <- data.frame(numProc = numeric(), calcTime = numeric())
colnames(calculation_time)

#take the m
for (numProc in c(1, 2, 4, 8, 16, 32)) {
    calc_i <- calculation_time[calculation_time$numProc == numProc, ]
    calc_i

    #create a matrx where every row are the times
    #for each processor during a  different iteration
    m <- matrix(calc_i$time, iterations, numProc)

    #Takes the max time for an iteration
    min_times <- apply(m, 1, min, na.rm = T)
    min_calc_time[nrow(min_calc_time) + 1, ] <- c(numProc, mean(min_times))
}

graphic(min_calc_time, "Брой процеси", "Време в микросекунди",
        "Време за изчисление на взаимодействията между 200 хиляди тела")

ggsave("./200thou/png/200thouCalcTime.png", width = 9)


#Speedup calculation_time for one process/ calculation_time for n processes
min_calc_time$calcTime  <- min_calc_time[min_calc_time$numProc == 1, ]$calcTime / min_calc_time$calcTime


graphic(min_calc_time, "Брой процеси", "Ускорение",
    "Ускорение на скоростта за изчисление на взаимодействията между 200 хиляди тела")
        

ggsave("./200thou/png/200thouCalcTimeSpeedup.png", width = 9)

mean_time_per_proc$calcTime <- mean_time_per_proc[mean_time_per_proc$numProc == 1, ]$calcTime / mean_time_per_proc$calcTime

graphic(mean_time_per_proc, "Брой процеси", "Ускорение",
        "Ускорени на цялостното време за работа при 200 хиляди тела")

ggsave("./200thou/png/200thouTotalTimeSpeedup.png", width = 9)

#Efficency speedup/number of processes
mean_time_per_proc$calcTime <- mean_time_per_proc$calcTime / mean_time_per_proc$numProc

graphic(mean_time_per_proc, "Брой процеси", "Ефективност",
        "Ефективност на цялото време за работа при 200 хиляди тела")

ggsave("./200thou/png/200thouTotalTimeEfficency.png", width = 9)

min_calc_time$calcTime  <- min_calc_time$calcTime / min_calc_time$numProc

graphic(min_calc_time, "Брой процеси", "Ефективност",
        "Ефективност на изчеслиението на на взаимодействията между 1 милион тела")

ggsave("./200thou/png/200thouCalcEfficency.png", width = 9)


#Barplot for calculations work per processor
calc16 <- calculation_time[calculation_time$numProc == 16, ]
colnames(calc16)

processor_avg <- matrix(nrow = 16, ncol = 3)

for (i in c(1, 2, 3)) {
    it <- calc16[calc16$iteration == i, ]
    for (id in seq(0, 15)) {
       processor_avg[id + 1, i] <- mean(it[it$id == id, ]$time)
    }
}

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

ggsave("./200thou/png/timeSpentInCalculationsPerProcessor.png", width = 11, height = 7)
