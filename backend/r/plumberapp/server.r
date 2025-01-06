library(plumber)
pr <- plumb('./app/plumberapp.R')
port=as.numeric(Sys.getenv("PORT"))
pr$run(host='0.0.0.0', port=port)
