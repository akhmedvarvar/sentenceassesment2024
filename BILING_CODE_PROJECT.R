#install.packages("lme4")
#install.packages("nlme")
library(lmerTest)
library(nlme)
library(lme4)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)

#ОТКРЫВАЕМ ДАТАСЕТ
read.pcibex <- function(filepath, auto.colnames=TRUE, fun.col=function(col,cols){cols[cols==col]<-paste(col,"Ibex",sep=".");return(cols)}) {
  n.cols <- max(count.fields(filepath,sep=",",quote=NULL),na.rm=TRUE)
  if (auto.colnames){
    cols <- c()
    con <- file(filepath, "r")
    while ( TRUE ) {
      line <- readLines(con, n = 1, warn=FALSE)
      if ( length(line) == 0) {
        break
      }
      m <- regmatches(line,regexec("^# (\\d+)\\. (.+)\\.$",line))[[1]]
      if (length(m) == 3) {
        index <- as.numeric(m[2])
        value <- m[3]
        if (is.function(fun.col)){
          cols <- fun.col(value,cols)
        }
        cols[index] <- value
        if (index == n.cols){
          break
        }
      }
    }
    close(con)
    return(read.csv(filepath, comment.char="#", header=FALSE, col.names=cols))
  }
  else{
    return(read.csv(filepath, comment.char="#", header=FALSE, col.names=seq(1:n.cols)))
  }
}

results_biling <- read.pcibex("C:/Lateral software/R-4.3.2/Проект по билингвизму/results(2).csv")
glimpse(results_biling)
results_biling$Value <- as.character(results_biling$Value)

#СОЗДАЮ DF С ЯЗЫКОВОЙ АНКЕТОЙ 
ling_data <- subset(results_biling, PennElementName == "survey_form")
print(ling_data)

# Убираю ненужные столбцы
ling_data1 <- ling_data %>%
  select('Results.reception.time', Hash = 'MD5.hash.of.participant.s.IP.address', Parameter, Value)
print(ling_data1)

ling_data1 <- mutate_all(ling_data1, as.character)

#Смотрю, есть ли дубликаты, удаляю
duplicates <- ling_data1 %>%
  group_by(Results.reception.time, Hash, Parameter) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n > 1L)
print(duplicates)

ling_data_final1 <- ling_data1 %>%
  group_by(Results.reception.time, Hash, Parameter) %>%
  summarise(Value = first(Value), .groups = "drop") %>%
  pivot_wider(names_from = Parameter, values_from = Value)

glimpse(ling_data_final1)

# Выбираю нужные колонки в нужном порядке
ling_data_final1 <- ling_data_final1 %>%
  select(
    Results.reception.time, Hash, age, sex, years_of_education, education,
    englishlevel_hse, englishlevel_ielts, englishlevel_toefl,
    domin1, domin2, domin3, domin4, domin5, 
    acq1, acq2, acq3, acq4, acq5, 
    nat_use_per, '2_use_per', '3_use_per', '4_use_per', '5_use_per', 
    understanden, speaken, readen, reading_en, watching_en, writing_en, 
    writing2_en, listening_en, family_en, friends_en
  )
head(ling_data_final1)

#Записываю результаты языковой анкеты в файл
write_csv(ling_data_final1, "билинг_язык_анкета.csv")

################################################################################

#Дропаю строки с анкетой 
results_biling1 <- results_biling %>%
  filter(results_biling$Label == "experiment")
glimpse(results_biling1)

#Выбираю нужные столбцы
results_biling1 <- results_biling1 %>%
  select('Results.reception.time', Hash = 'MD5.hash.of.participant.s.IP.address', id, Parameter, EventTime, Word, Color, Correct)
glimpse(results_biling1)

# Удаляю дубликаты
results_biling1 <- distinct(results_biling1)

# Удаляю строки с незаполненными данными
results_biling1 <- na.omit(results_biling1)

#ДЕЛАЮ КОЛОНКУ RT
# Отфильтровываем строки с Parameter = "Start" и Parameter = "End"
start_times <- results_biling1 %>% filter(Parameter == "Start") %>%
  group_by(Results.reception.time, Hash, id, Word, Color, Correct) %>%
  slice(1) # Берем только первое вхождение StartTime для каждого уникального сочетания

end_times <- results_biling1 %>% filter(Parameter == "End") %>%
  group_by(Results.reception.time, Hash, id, Word, Color, Correct) %>%
  slice(1) # Берем только первое вхождение EndTime для каждого уникального сочетания

joined_times <- start_times %>%
  select(Results.reception.time, Hash, id, Word, Color, Correct, StartTime = EventTime) %>%
  inner_join(end_times %>% select(Results.reception.time, Hash, id, Word, Color, Correct, EndTime = EventTime), 
             by = c("id", "Word", "Color", "Results.reception.time"))

# Вычисляю RT как разницу между EndTime и StartTime
joined_times <- joined_times %>%
  mutate(RT = EndTime - StartTime)

joined_times <- joined_times %>%
  select(Results.reception.time, Hash = Hash.x, id, Word, Color, Correct = Correct.x, StartTime, EndTime, RT) 

glimpse(joined_times)

#УДАЛЯЮ ВЫБРОСЫ
# Вычисляю квартили для RT
Q1 <- quantile(joined_times$RT, 0.25)
Q3 <- quantile(joined_times$RT, 0.75)

# Вычисляю интерквартильный размах
IQR <- Q3 - Q1

# Определяю верхнюю и нижнюю границы выбросов
upper_bound <- Q3 + 1.5 * IQR
lower_bound <- Q1 - 1.5 * IQR

# Фильтрую таблицу, оставляю только значения внутри границ
joined_times_filtered <- joined_times %>%
  filter(RT >= lower_bound & RT <= upper_bound)

#УДАЛЯЮ НЕНУЖНЫХ РЕСПОНДЕНТОВ (те, у кого английский < B2. те, кто < 18 лет)
# Указываем хэши, которые нужно удалить
hashes_to_remove <- c("45ed5372ac1453d5b42163025f7c25dc",
                      "99bf7c2f32e8c78396ffe992d4f33685",
                      "e9868a3352fcf47e5b15a38e1b7e17a0",
                      "749fe78925289071a4a1dbb2e1a412dc",
                      "eb1b118b9a8e511a31b3012923c5109e",
                      "ee045e8e19154b6c6ba474983f5900a1",
                      "ba26a31734d0e5e3d8d0c29015a8f2ed",
                      "12634c1edb8f8e1445f60faf6a58b29e",
                      "b00b6afe7e5600e47f7f6bcaa8ea9365",
                      "2636a0663e4b78aaf5d444824a44c381",
                      "8dd859d90c35a64773096c04c000068f",
                      "4c17f557e84d14c4e32ad61a87ded608",
                      "ed5b7087bcdeb6e9ca8d3026ece2a605",
                      "7ce59745074c300dc4cb3cb680d7e027")

# Удаляю строки с указанными хэшами
joined_times_filtered <- joined_times_filtered %>%
  filter(!Hash %in% hashes_to_remove)

#ВСТАВЛЯЮ КОЛОНКУ С ЯЗЫКОМ СТИМУЛА
unique(joined_times_filtered$Word)

# Создаю новую колонку Language_stim
joined_times_filtered <- joined_times_filtered %>%
  mutate(Language_stim = ifelse(Word %in% c("black", "brown", "gray", "green", "orange", "pink", "purple", "red", "yellow"), "eng", "rus"))
head(joined_times_filtered)

#ВСТАВЛЯЮ КОЛОНКУ CONGRUENCY
# Создаю словарь соответствий
congruency_dict <- c("black" = "black", 
                     "brown" = "darkred",
                     "gray" = "gray",
                     "green" = "green",
                     "orange" = "orange",
                     "pink" = "pink",
                     "purple" = "purple",
                     "red" = "red",
                     "yellow" = "yellow",
                     "зелёный" = "green",
                     "коричневый" = "darkred",
                     "красный" = "red",
                     "оранжевый" = "orange",
                     "розовый" = "pink",
                     "серый" = "gray",
                     "синий" = "navy",
                     "фиолетовый" = "purple",
                     "чёрный" = "black",
                     "жёлтый" = "yellow")

# Создаю новую колонку Congruency
joined_times_filtered <- joined_times_filtered %>%
  mutate(Congruency = ifelse(Color == congruency_dict[Word], "congr", "incongr"))
head(joined_times_filtered)

#ВСТАВЛЯЮ КОЛОНКУ LENGHT_WORD
joined_times_filtered <- joined_times_filtered %>%
  mutate(length_word = nchar(Word))

#МЕРЫ ЦТ ДЛЯ ENG и RUS
joined_times_rus<- subset(joined_times_filtered, Language_stim == 'rus')
mean(joined_times_rus$RT)       
min(joined_times_rus$RT)    
max(joined_times_rus$RT)          
sd(joined_times_rus$RT)         

joined_times_eng<- subset(joined_times_filtered, Language_stim == 'eng')
mean(joined_times_eng$RT)
min(joined_times_eng$RT)     
max(joined_times_eng$RT)    
sd(joined_times_eng$RT)

#статистически не значимо различие в RT для разных языков стимулов
shapiro.test(joined_times$RT)
wilcox.test(joined_times_rus$RT, joined_times_eng$RT)

#модель со случайным эффектом Hash
#model_lang <- lm(RT ~ Language_stim+length_word , data = joined_times_filtered)
#summary(model_lang)

model_lang_x <- lmer(RT ~ Language_stim + length_word + (1| Hash), data = joined_times_filtered)
summary(model_lang_x)

#МЕРЫ ЦТ ДЛЯ CONGR и NONCONGR
joined_times_congr<- subset(joined_times_filtered, Congruency == 'congr')
mean(joined_times_congr$RT)
min(joined_times_congr$RT)
max(joined_times_congr$RT)
sd(joined_times_congr$RT)

joined_times_incongr<- subset(joined_times_filtered, Congruency == 'incongr')
mean(joined_times_incongr$RT)
min(joined_times_incongr$RT)
max(joined_times_incongr$RT)
sd(joined_times_incongr$RT)

#статистически значимо различие в RT для разных congr/incongr стимулов
wilcox.test(joined_times_congr$RT, joined_times_incongr$RT)
#model_congr<- lm(RT ~ Congruency+length_word , data = joined_times_filtered)
#summary(model_congr)

model_congr_x <- lmer(RT ~ Congruency + length_word + (1| Hash), data = joined_times_filtered)
summary(model_congr_x)

#СМОТРЮ НА ЯЗЫК И КОНГРУЭНТНОСТь
joined_times_congr_rus<- subset(joined_times_rus, Congruency == 'congr')
mean(joined_times_congr_rus$RT)     
min(joined_times_congr_rus$RT)  
max(joined_times_congr_rus$RT)    
sd(joined_times_congr_rus$RT)          

joined_times_congr_eng<- subset(joined_times_eng, Congruency == 'congr')
mean(joined_times_congr_eng$RT)      
min(joined_times_congr_eng$RT)   
max(joined_times_congr_eng$RT)    
sd(joined_times_congr_eng$RT)         

joined_times_incongr_rus<- subset(joined_times_rus, Congruency == 'incongr')
mean(joined_times_incongr_rus$RT)     
min(joined_times_incongr_rus$RT)   
max(joined_times_incongr_rus$RT)    
sd(joined_times_incongr_rus$RT)         

joined_times_incongr_eng<- subset(joined_times_eng, Congruency == 'incongr')
mean(joined_times_incongr_eng$RT)    
min(joined_times_incongr_eng$RT)    
max(joined_times_incongr_eng$RT) 
sd(joined_times_incongr_eng$RT)        

#Статистически значим предиктор неконгруэнтности
#model_lang_congr <- lm(RT ~ Language_stim + Congruency+length_word , data = joined_times_filtered)
#summary(model_lang_congr)

model_lang_congr1_x <- lmer(RT ~ Language_stim + Congruency + length_word + (1|Hash), data = joined_times_filtered)
summary(model_lang_congr1_x)

#СОЕДИНЕНИЕ В ПАРЫ 
# Добавляю столбец Lang_congr, состоящий из комбинации Language_stim и Congruency
joined_times_filtered <- joined_times_filtered %>%
  mutate(Lang_congr = paste(Language_stim, Congruency, sep = "_"))
glimpse(joined_times_filtered)

################################################################################

#ДЕЛАЮ ПАРЫ
# Создаю пустой список для хранения результатов для каждого хэша
hash_datasets <- list()

# Прохожу по каждому хэшу в joined_times_filtered
for (hash_value in unique(joined_times_filtered$Hash)) {
  # Фильтрую данные для текущего хэша
  hash_data <- subset(joined_times_filtered, Hash == hash_value)
  
  # Определяю словарь
  word_color_pairs <- data.frame(
    Word = c("серый", "оранжевый", "gray", "purple", "красный", "чёрный", "зелёный", 
             'orange', 'синий', 'red', 'pink', 'black', 'жёлтый', 'синий', 'серый', 'green', 
             'gray', 'коричневый', 'фиолетовый', "orange",'gray', 'розовый','black', 'red', 
             'yellow', 'оранжевый', 'чёрный', 'yellow', 'зелёный', 'brown', 'pink', 'коричневый', 
             'зелёный'), 
    Color = c("purple", "black", "purple", "purple", "red", "black", "green", 
              "black", "navy", "red", "yellow", "navy", "yellow", 'navy', 'navy', "green", 
              "gray", 'black', 'gray', 'pink', 'purple', "darkred", "black", 'red', 'yellow', 'pink', 
              'black', 'yellow', 'green', 'black', 'darkred', 'green', 'navy')
  )
  
  # Создаю колонку word_color в word_color_pairs
  word_color_pairs <- word_color_pairs %>%
    mutate(word_color = paste(Word, Color, sep = "_"))
  
  # Создаю колонку word_color в hash_data
  hash_data <- hash_data %>%
    mutate(word_color = paste(Word, Color, sep = "_"))
  
  # Присоединяю hash_data к word_color_pairs для сортировки
  hash_data_sorted <- word_color_pairs %>%
    left_join(hash_data, by = "word_color")
  
  # Удаляю столбцы Word.y, Color.y, pair
  hash_data_sorted <- hash_data_sorted %>%
  select(-Word.y, -Color.y) #-pair)
  
  # Переименовываю столбцы Word.x и Color.x
  names(hash_data_sorted)[names(hash_data_sorted) == "Word.x"] <- "Word"
  names(hash_data_sorted)[names(hash_data_sorted) == "Color.x"] <- "Color"
  
  # Заполняю столбец Hash
  hash_data_sorted$Hash <- hash_value
  
  # Переупорядочиваю столбцы
  hash_data_sorted <- hash_data_sorted %>%
    select(Results.reception.time, Hash, id, Word, Color, word_color, Correct, StartTime, EndTime, RT, Language_stim, Congruency, Lang_congr)
  
  # Добавляю текущий датасет к списку результатов
  hash_datasets[[hash_value]] <- hash_data_sorted
}

# Делаю пары для каждого хэша
for (i in seq_along(hash_datasets)) {
  hash_datasets[[i]] <- hash_datasets[[i]] %>%
    mutate(Pair_word_color = paste(word_color, lead(word_color), sep = "_"),
           Word_color1 = word_color,
           Word_color2 = lead(word_color),
           Correct_pair = paste(Correct, lead(Correct), sep = "_"),
           RT_prime = RT,
           RT_stim = lead(RT),
           Language_pair = paste(Language_stim, lead(Language_stim), sep = "_"),
           Congruency_pair = paste(Congruency, lead(Congruency), sep = "_"),
           Lang_congr_pair = paste(Lang_congr, lead(Lang_congr), sep = "_")) %>%
    select(Hash, id, Pair_word_color, Word_color1, Word_color2, Correct_pair, RT_prime, RT_stim, Language_pair, Congruency_pair, Lang_congr_pair) %>%
    mutate(Correct_pair = if_else(is.na(Correct_pair), "NA", Correct_pair),
           Language_pair = if_else(is.na(Language_pair), "NA", Language_pair),
           Congruency_pair = if_else(is.na(Congruency_pair), "NA", Congruency_pair),
           Lang_congr_pair = if_else(is.na(Lang_congr_pair), "NA", Lang_congr_pair),
           RT_prime = if_else(is.na(RT_prime), "NA", as.character(RT_prime)),
           RT_stim = if_else(is.na(RT_stim), "NA", as.character(RT_stim)),
           Word_color2 = if_else(is.na(Word_color2), "NA", Word_color2))
}

final_dataset <- bind_rows(hash_datasets)

# Добавляю столбец length_stim
final_dataset <- final_dataset %>%
  mutate(length_stim = nchar(substring(Word_color2, 1, regexpr("_", Word_color2) - 1)))

glimpse(final_dataset)

################################################################################

#4 датасета (рус+рус, рус+англ, англ+рус, англ+англ)
final_dataset$RT_prime <- as.numeric(final_dataset$RT_prime, na.rm = TRUE)
final_dataset$RT_stim <- as.numeric(final_dataset$RT_stim, na.rm = TRUE)

rus_rus <- subset(final_dataset, Language_pair == "rus_rus")
mean(rus_rus$RT_stim)      
min(rus_rus$RT_stim)    
max(rus_rus$RT_stim)          
sd(rus_rus$RT_stim) 

rus_eng <- subset(final_dataset, Language_pair == "rus_eng")
mean(rus_eng$RT_stim)        
min(rus_eng$RT_stim)    
max(rus_eng$RT_stim)          
sd(rus_eng$RT_stim) 

eng_rus <- subset(final_dataset, Language_pair == "eng_rus")
mean(eng_rus$RT_stim)        
min(eng_rus$RT_stim)    
max(eng_rus$RT_stim)          
sd(eng_rus$RT_stim) 

eng_eng <- subset(final_dataset, Language_pair == "eng_eng")
mean(eng_eng$RT_stim)        
min(eng_eng$RT_stim)    
max(eng_eng$RT_stim)          
sd(eng_eng$RT_stim) 

# Удаляю строки с NA
final_dataset_no_NA <- final_dataset %>%
  drop_na()

glimpse(final_dataset_no_NA)

#МОДЕЛЬ ДЛЯ РЕАКЦИИ НА СТИМУЛ RTstim
#model_stim_lang <- lm(RT_stim ~ Language_pair+length_stim, data = final_dataset_no_NA)
#summary(model_stim_lang)

model_stim_lang1_x <- lmer(RT_stim ~ Language_pair + length_stim + (1|Hash), data = final_dataset_no_NA)#, REML=FALSE)
summary(model_stim_lang1_x)

#4 датасета на конгруэнтность
congr_congr <- subset(final_dataset, Congruency_pair == "congr_congr")
mean(congr_congr$RT_stim)        
min(congr_congr$RT_stim)    
max(congr_congr$RT_stim)          
sd(congr_congr$RT_stim) 

congr_incongr <- subset(final_dataset, Congruency_pair == "congr_incongr")
mean(congr_incongr$RT_stim)        
min(congr_incongr$RT_stim)    
max(congr_incongr$RT_stim)          
sd(congr_incongr$RT_stim) 

incongr_congr <- subset(final_dataset, Congruency_pair == "incongr_congr")
mean(incongr_congr$RT_stim)       
min(incongr_congr$RT_stim)    
max(incongr_congr$RT_stim)          
sd(incongr_congr$RT_stim) 

incongr_incongr <- subset(final_dataset, Congruency_pair == "incongr_incongr")
mean(incongr_incongr$RT_stim)        
min(incongr_incongr$RT_stim)    
max(incongr_incongr$RT_stim)          
sd(incongr_incongr$RT_stim)

#МОДЕЛЬ ДЛЯ ВРЕМЕНИ РЕАКЦИИ НА СТИМУЛ + конгруэнтность пары
#model_stim_congr <- lm(RT_stim ~ Congruency_pair*length_stim, data = final_dataset_no_NA)
#summary(model_stim_congr)

model_stim_congr1_x <- lmer(RT_stim ~ Congruency_pair + length_stim + (1|Hash), data = final_dataset_no_NA)
summary(model_stim_congr1_x)

#Модель для языка+конгруэнтности
#model_stim_lang_congr <- lm(RT_stim ~ Lang_congr_pair+length_stim, data = final_dataset_no_NA)
#summary(model_stim_lang_congr)

model_stim_lang_congr1_x <- lmer(RT_stim ~ Lang_congr_pair + length_stim + (1|Hash), data = final_dataset_no_NA)
summary(model_stim_lang_congr1_x)
  