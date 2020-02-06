#How to get a new season of play-by-play data

#go to Chadwick webpage and get cwevent.exe

#create folder retrosheet in working directory

#create zipped and unzipped subfolders

#put cwevent.exe in unzipped folder

#get parse_retrosheet_pbp function from github

script <- getURL("https://raw.githubusercontent.com/beanumber/baseball_R/master/scripts/parse_retrosheet_pbp.R")

eval(parse(text = script))


parse_retrosheet_pbp(2016)