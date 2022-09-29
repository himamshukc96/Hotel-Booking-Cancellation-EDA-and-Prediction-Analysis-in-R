#==============================================================================#
#                                LIBRARIES                                     #
#==============================================================================#
library(tidyverse)
library(readxl)
library(stringr)
library(purrr)
library(ggplot2)
library(janitor) #needed to use tabyl
library(quanteda)
library(quanteda.textplots)
library(countrycode)
library(tm)
library(wordcloud)
library(wordcloud2)
library(MLeval)
library(funModeling)
#==============================================================================#
#                              DATA INITIALIZATION                             #
#==============================================================================#

hoteldata <- data.frame(read_csv("https://intro-datascience.s3.us-east-2.amazonaws.com/Resort01.csv")) #puts CSV into a dataframe called data
anyNA(hoteldata) #checks for NAs in the Dataset (if returns false, it's good)
hoteldata <- mutate_if(hoteldata, is.character, factor) #makes the categoricals as factors
hoteldata$IsCanceled <-  as.factor(hoteldata$IsCanceled) #makes the binary as factors
hoteldata$IsRepeatedGuest <-  as.factor(hoteldata$IsRepeatedGuest) #makes the binary as factors
levels(hoteldata$Meal) <- list(UndefinedSC=c("Undefined","SC"),BB=c("BB"),FB=c("FB"),HB=c("HB")) #combine Undefined and SC into one (since they're the same)

CanceledBookingData<- hoteldata %>% filter(IsCanceled == 1) #subset of canceled bookings
NonCanceledBookingData <- hoteldata %>% filter(IsCanceled == 0) #subset of noncanceled bookings
RepeatGuestData <- hoteldata %>%  filter(IsRepeatedGuest == 1) #subset of repeat or loyal customers
FirstTimeGuestData <- hoteldata %>%  filter(IsRepeatedGuest == 0) #subset of first time customers

IsCanceledBookingData<- CanceledBookingData #backup data


#==============================================================================#
#                              DATA EXPLORATION                                #
#==============================================================================#

##################Histograms################
hist_1 <- ggplot(hoteldata, aes(x= LeadTime, binwidth = 15)) + geom_histogram()
hist_2 <- ggplot(hoteldata, aes(x= StaysInWeekendNights, binwidth = 2)) + geom_histogram()
hist_3 <- ggplot(hoteldata, aes(x= Adults, binwidth = 2)) + geom_histogram()
hist_4 <- ggplot(hoteldata, aes(x= Children, binwidth = 15)) + geom_histogram()
hist_5 <- ggplot(hoteldata, aes(x= Babies, binwidth = 15)) + geom_histogram()
hist_6 <- ggplot(hoteldata, aes(x= PreviousCancellations, binwidth = 15)) + geom_histogram()
hist_7 <- ggplot(hoteldata, aes(x= PreviousBokingsNotCancelled, binwidth = 15)) + geom_histogram()
hist_8 <- ggplot(hoteldata, aes(x= BookingChanges, binwidth = 15)) + geom_histogram()
hist_9 <- ggplot(hoteldata, aes(x= RequiredCarParkingSpaces)) + geom_histogram()
hist_10 <- ggplot(hoteldata, aes(x= TotalSpecialRequests, binwidth = 15)) + geom_histogram()
##################End of Histograms########



##################Column graphs#############3
colgrpah_1 <- ggplot(hoteldata, aes(x = IsCanceled, y = LeadTime, fill = IsCanceled)) +geom_col()
colgrpah_2 <- ggplot(hoteldata, aes(x = IsCanceled, y = StaysInWeekendNights, fill = IsCanceled)) +geom_col()
colgrpah_3 <- ggplot(hoteldata, aes(x = IsCanceled, y = Adults, fill = IsCanceled)) +geom_col()
colgrpah_4 <- ggplot(hoteldata, aes(x = IsCanceled, y = Children, fill = IsCanceled)) +geom_col()
colgrpah_5 <- ggplot(hoteldata, aes(x = IsCanceled, y = Babies, fill = IsCanceled)) +geom_col()
colgrpah_6 <- ggplot(hoteldata, aes(x = IsCanceled, y = PreviousCancellations, fill = IsCanceled)) +geom_col()
colgrpah_7 <- ggplot(hoteldata, aes(x = IsCanceled, y = PreviousBokingsNotCancelled, fill = IsCanceled)) +geom_col()
colgrpah_8 <- ggplot(hoteldata, aes(x = IsCanceled, y = BookingChanges, fill = IsCanceled)) +geom_col()
colgrpah_9 <- ggplot(hoteldata, aes(x = IsCanceled, y = RequiredCardParkingSpaces, fill = IsCanceled)) +geom_col()
colgrpah_10 <- ggplot(hoteldata, aes(x = IsCanceled, y = TotalSpecialRequests, fill = IsCanceled)) +geom_col()


###############Bar Graphs############################
ggplot(cancellationbasedonBabies, aes(x = Freq, y = Var1, main="Number of Cancellations based on Number of Babies")) + geom_bar(stat = "identity") +
  ylab("Number of Babies")+xlab("Number of Cancellations") +theme_get()

ggplot(noncancellationbasedonBabies, aes(x = Freq, y = Var1, main="Number of confirmed reservations based on Number of Babies")) + 
geom_bar(stat = "identity") +ylab("Number of Babies")+xlab("Number of Reservations") +theme_get() 






##################Column of graphs##########3


##################Boxplots#############3



# Does the total strength of the family impact the decision to cancel the booking? 
#   (A table representing number of babies/children and Iscancelled column) 

childboxplot <- function(behavior){ 
  
  hoteldata %>% 
    
    ggplot(aes(x = IsCanceled, y = Children)) + 
    
    geom_boxplot() + 
    
    geom_jitter(width = .15, alpha = .2) + 
    
    labs(title = "Families with children cancellinge", 
         
         subtitle = Children, 
         
         x = "IsCancelled",  
         
         y = "children") + 
    
    theme(panel.grid.major = element_blank(), 
          
          panel.grid.minor = element_blank(), 
          
          panel.background = element_blank(),  
          
          axis.line = element_line(colour = "black")) 
  
} 



guestbehaboxplot (hoteldata$Children) 






babyboxplot <- function(behavior){ 
  
  hoteldata %>% 
    
    ggplot(aes(x = IsCanceled, y = Babies)) + 
    
    geom_boxplot() + 
    
    geom_jitter(width = .15, alpha = .2) + 
    
    labs(title = "Families with babies cancellinge", 
         
         subtitle = "Babies", 
         
         x = "IsCancelled",  
         
         y = "Babies") + 
    
    theme(panel.grid.major = element_blank(), 
          
          panel.grid.minor = element_blank(), 
          
          panel.background = element_blank(),  
          
          axis.line = element_line(colour = "black")) 
  
} 



babyboxplot(hoteldata$Babies) 














boxplot_1 <- ggplot(hoteldata, aes(x = IsCanceled, y = LeadTime, fill = IsCanceled)) +geom_boxplot()
boxplot_2 <- ggplot(hoteldata, aes(x = IsCanceled, y = StaysInWeekendNights, fill = IsCanceled)) +geom_boxplot()
boxplot_3 <- ggplot(hoteldata, aes(x = IsCanceled, y = Adults, fill = IsCanceled)) +geom_boxplot()
boxplot_4 <- ggplot(hoteldata, aes(x = IsCanceled, y = Children, fill = IsCanceled)) +geom_boxplot()
boxplot_5 <- ggplot(hoteldata, aes(x = IsCanceled, y = Babies, fill = IsCanceled)) +geom_boxplot()
boxplot_6 <- ggplot(hoteldata, aes(x = IsCanceled, y = PreviousCancellations, fill = IsCanceled)) +geom_boxplot()
boxplot_7 <- ggplot(hoteldata, aes(x = IsCanceled, y = PreviousBokingsNotCancelled, fill = IsCanceled)) +geom_boxplot()
boxplot_8 <- ggplot(hoteldata, aes(x = IsCanceled, y = BookingChanges, fill = IsCanceled)) +geom_boxplot()
boxplot_9 <- ggplot(hoteldata, aes(x = IsCanceled, y = RequiredCardParkingSpaces, fill = IsCanceled)) +geom_boxplot()
boxplot_10 <- ggplot(hoteldata, aes(x = IsCanceled, y = TotalSpecialRequests, fill = IsCanceled)) +geom_boxplot()
##################End of Boxplots##########3



##################Tables#################

hoteldata %>% tabyl(IsCanceled) %>% adorn_totals("row") %>% adorn_pct_formatting()
hoteldata %>% tabyl(Meal) %>% adorn_totals("row") %>% adorn_pct_formatting()
hoteldata %>% tabyl(Country) %>% adorn_totals("row") %>% adorn_pct_formatting()
hoteldata %>% tabyl(MarketSegment) %>% adorn_totals("row") %>% adorn_pct_formatting()
hoteldata %>% tabyl(IsRepeatedGuest) %>% adorn_totals("row") %>% adorn_pct_formatting()
hoteldata %>% tabyl(ReservedRoomType) %>% adorn_totals("row") %>% adorn_pct_formatting()
hoteldata %>% tabyl(AssignedRoomType) %>% adorn_totals("row") %>% adorn_pct_formatting()
hoteldata %>% tabyl(DepositType) %>% adorn_totals("row") %>% adorn_pct_formatting()
hoteldata %>% tabyl(CustomerType) %>% adorn_totals("row") %>% adorn_pct_formatting()


#Q. How does the of babies affect booking cancellations? 
noncancellationbasedonChildren<- data.frame(table(NonCanceledBookingData$Children)) 
cancellationbasedonBabies<- data.frame(table(CanceledBookingData$Babies)) 

#Q. How does the FnB facet affect the overall hospitality that is expected? 
cancellationbasedonMeal<- data.frame(table(CanceledBookingData$Meal)) 

pielabels= cancellationbasedonMeal$Var1 
ggplot(cancellationbasedonMeal, aes(x="", y=Freq, fill= pielabels)) +  geom_bar(stat="identity") + coord_polar("y") 





#Q. How does the FnB facet affect the overall hospitality that is expected? 
CollateddfFnB<- data.frame(table(hoteldata$Meal)) 

CollateddfFnB$cancelledpercentage<- (cancellationbasedonMeal$Freq/CollateddfFnB$Freq)*100 

CollateddfFnB$noncancelledpercentage<- 100 - CollateddfFnB$cancelledpercentage 

CollateddfFnB<- rename(CollateddfFnB, Meal_Plans= Var1) 






###Repeat Guests

RepeatGuestData %>% tabyl(IsCanceled) %>% adorn_totals("row") %>% adorn_pct_formatting() 
FirstTimeGuestData %>% tabyl(IsCanceled) %>% adorn_totals("row") %>% adorn_pct_formatting() 


#################Tables (IsCancelled vs NotCancelled)#############################
IsCanceledBookingData<- hoteldata %>% filter(IsCanceled == 1)
NonCanceledBookingData <- hoteldata %>% filter(IsCanceled == 0)

IsCanceledBookingData %>% tabyl(LeadTime) %>% adorn_totals("row") %>% adorn_pct_formatting()
NonCanceledBookingData %>% tabyl(LeadTime) %>% adorn_totals("row") %>% adorn_pct_formatting()

IsCanceledBookingData %>% tabyl(StaysInWeekendNights) %>% adorn_totals("row") %>% adorn_pct_formatting()
NonCanceledBookingData %>% tabyl(StaysInWeekendNights) %>% adorn_totals("row") %>% adorn_pct_formatting()

IsCanceledBookingData %>% tabyl(StaysInWeekNights) %>% adorn_totals("row") %>% adorn_pct_formatting()
NonCanceledBookingData %>% tabyl(StaysInWeekNights) %>% adorn_totals("row") %>% adorn_pct_formatting()

IsCanceledBookingData %>% tabyl(Adults) %>% adorn_totals("row") %>% adorn_pct_formatting()
NonCanceledBookingData %>% tabyl(Adults) %>% adorn_totals("row") %>% adorn_pct_formatting()

IsCanceledBookingData %>% tabyl(Children) %>% adorn_totals("row") %>% adorn_pct_formatting()
NonCanceledBookingData %>% tabyl(Children) %>% adorn_totals("row") %>% adorn_pct_formatting()

IsCanceledBookingData %>% tabyl(Babies) %>% adorn_totals("row") %>% adorn_pct_formatting()
NonCanceledBookingData %>% tabyl(Babies) %>% adorn_totals("row") %>% adorn_pct_formatting()

IsCanceledBookingData %>% tabyl(Meal) %>% adorn_totals("row") %>% adorn_pct_formatting()
NonCanceledBookingData %>% tabyl(Meal) %>% adorn_totals("row") %>% adorn_pct_formatting()

IsCanceledBookingData %>% tabyl(Country) %>% adorn_totals("row") %>% adorn_pct_formatting()
NonCanceledBookingData %>% tabyl(Country) %>% adorn_totals("row") %>% adorn_pct_formatting()

IsCanceledBookingData %>% tabyl(MarketSegment) %>% adorn_totals("row") %>% adorn_pct_formatting()
NonCanceledBookingData %>% tabyl(MarketSegment) %>% adorn_totals("row") %>% adorn_pct_formatting()

IsCanceledBookingData %>% tabyl(IsRepeatedGuest) %>% adorn_totals("row") %>% adorn_pct_formatting()
NonCanceledBookingData %>% tabyl(IsRepeatedGuest) %>% adorn_totals("row") %>% adorn_pct_formatting()

IsCanceledBookingData %>% tabyl(PreviousCancellations) %>% adorn_totals("row") %>% adorn_pct_formatting()
NonCanceledBookingData %>% tabyl(PreviousCancellations) %>% adorn_totals("row") %>% adorn_pct_formatting()

IsCanceledBookingData %>% tabyl(ReservedRoomType) %>% adorn_totals("row") %>% adorn_pct_formatting()
NonCanceledBookingData %>% tabyl(ReservedRoomType) %>% adorn_totals("row") %>% adorn_pct_formatting()

IsCanceledBookingData %>% tabyl(AssignedRoomType) %>% adorn_totals("row") %>% adorn_pct_formatting()
NonCanceledBookingData %>% tabyl(AssignedRoomType) %>% adorn_totals("row") %>% adorn_pct_formatting()

IsCanceledBookingData %>% tabyl(BookingChanges) %>% adorn_totals("row") %>% adorn_pct_formatting()
NonCanceledBookingData %>% tabyl(BookingChanges) %>% adorn_totals("row") %>% adorn_pct_formatting()

IsCanceledBookingData %>% tabyl(DepositType) %>% adorn_totals("row") %>% adorn_pct_formatting()
NonCanceledBookingData %>% tabyl(DepositType) %>% adorn_totals("row") %>% adorn_pct_formatting()

IsCanceledBookingData %>% tabyl(CustomerType) %>% adorn_totals("row") %>% adorn_pct_formatting()
NonCanceledBookingData %>% tabyl(CustomerType) %>% adorn_totals("row") %>% adorn_pct_formatting()

IsCanceledBookingData %>% tabyl(RequiredCarParkingSpaces) %>% adorn_totals("row") %>% adorn_pct_formatting()
NonCanceledBookingData %>% tabyl(RequiredCarParkingSpaces) %>% adorn_totals("row") %>% adorn_pct_formatting()

IsCanceledBookingData %>% tabyl(TotalOfSpecialRequests) %>% adorn_totals("row") %>% adorn_pct_formatting()
NonCanceledBookingData %>% tabyl(TotalOfSpecialRequests) %>% adorn_totals("row") %>% adorn_pct_formatting()

################## End of Tables (IsCanceled vs NotCanceled) ###################
#==============================================================================#
#                                   CoRRELATION TEST                           #
#==============================================================================#

#Import data from part 1: 

dfm=hoteldata 



#Create local copy of dfm for manipulation 

dfm_temp <- dfm 



#Change 'IsCanceled' back to numeric 

dfm_temp$IsCanceled <- as.numeric(dfm_temp$IsCanceled) 

#Check correlation: 

dfnum = dplyr::select_if(dfm_temp, is.numeric) #select only numeric variables 
dfnum = data.frame(lapply(dfnum, function(x) as.numeric(as.character(x)))) #loop through columns and change factor variables into numeric 
res=cor(dfnum) 
dev.new() 
corrplot(res, method="color", type="upper", tl.col="black") 


#==============================================================================#
#                                    MAP                                       #
#==============================================================================#
library(rworldmap)

CountryCountData <- hoteldata %>% mutate(count=1) %>% group_by(Country) %>% summarise(bookingCounts = sum(count))



NoCountryCountData <- hoteldata %>% mutate(count=1) %>% group_by(Country) %>% summarise(bookingCounts = sum(count))

CountryCountData$group <- ntile(CountryCountData$bookingCounts, 50)
CountryCountData$rank <-rank(CountryCountData$bookingCounts)
mean(CountryCountData$rank) #317.93, the average #######################################!!!I GOT 63.5 ON MY RUN!!

mapdata <- joinCountryData2Map(CountryCountData,"ISO3","Country")
mapCountryData(mapdata,
               nameColumnToPlot = "bookingCounts" ,
               catMethod = "fixedWidth",
               colourPalette = "terrain",
               mapRegion = "eurasia",
               numCats = 126,
               # missingCountryCol = 'Green'
)

#==============================================================================#
#                                 WORDCLOUD                                    #
#==============================================================================#
countries <- as.character(hoteldata$Country)

#change all CNs to CHNs because they both refer to China:
countries <- replace(countries, which(countries=="CN"), "CHN")

#Change all NULLs to 'Unknown':
countries <- replace(countries, which(countries=="NULL"),"Unknown")

tabyl(countries) %>% adorn_totals("row") %>% adorn_pct_formatting()

countries <- countrycode(countries, "iso3c", "country.name")

countries <- str_to_title(countries)

countries <- str_replace_all(countries, " ", "")

docs <- Corpus(VectorSource(countries))
#creates corpus from 'meaning' column of the dataframe

docs <- docs %>%
  tm_map(stripWhitespace)

dtm <- TermDocumentMatrix(docs)
matrix <- as.matrix(dtm)
words<- sort(rowSums(matrix),decreasing=TRUE)
df<-data.frame(word=names(words), freq=words)
dev.new()
wordcloud(words=df$word, freq=df$freq, min.freq=1,
          max.words=50000, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8,"Dark2"))

