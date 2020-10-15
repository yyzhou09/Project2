This repo is a host for reports for Monday to Sunday.  

I used the following code to automate the reports.

DayofWeek<-unique(Data$weekday)
render_one<-function(weekday){
  rmarkdown::render(
    "Project2.Rmd",output_file = paste0(weekday,".md"), params = list(weekday=weekday)
  )
}

for (weekday in DayofWeek){
  render_one(weekday)
}


The analysis for [Monday is available here](weekday_is_monday.md)   

The analysis for [Tuesday is available here](weekday_is_tuesday.md)  

The analysis for [Wednesday is available here](weekday_is_wednesday.md)    

The analysis for [Thursday is available here](weekday_is_thursday.md)  

The analysis for [Friday is available here](weekday_is_friday.md)  

The analysis for [Saturday is available here](weekday_is_saturday.md)  

The analysis for [Sunday is available here](weekday_is_sunday.md)  

