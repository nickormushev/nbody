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

#gets the max time for the processors and
#then the min total time for each attempt
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

    mean_time_per_proc[nrow(mean_time_per_proc) + 1, ] <-
        c(numProc, min_time)
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
    max_times <- c()

    for (i in c(1, 2, 3)) {
        calc_i <- calculation_time[calculation_time$numProc == numProc &
                                   calculation_time$iteration == i, ]

        #create a matrx where every row are the times
        #for each processor during a  different iteration
        m <- matrix(calc_i$time, iterations, numProc)

        #Takes the max time for a processor during a single unit of time
        max_times_for_attempt <- apply(m, 1, max, na.rm = T)
        #Takes the sum of the units of time
        max_times[i] <- sum(max_times_for_attempt)
    }

    max_times

    min_calc_time[nrow(min_calc_time) + 1, ] <- c(numProc, min(max_times))
}
min_calc_time
mean_time_per_proc

overhead_vs_calc_time <- rbind(min_calc_time, mean_time_per_proc)

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
mean_time_per_proc$calcTime <- mean_time_per_proc[mean_time_per_proc$numProc == 1, ]$calcTime / mean_time_per_proc$calcTime

graphic(mean_time_per_proc, "Брой процеси", "Ускорение",
        "Ускорени на цялостното време за работа при 200 хиляди тела")

ggsave("./200thou/png/200thouTotalTimeSpeedup.png", width = 9)

#Efficency speedup/number of processes
mean_time_per_proc$calcTime <- mean_time_per_proc$calcTime / mean_time_per_proc$numProc

graphic(mean_time_per_proc, "Брой процеси", "Ефективност",
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
