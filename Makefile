all: rcourse.html stats_with_R.html

rcourse.html: rcourse.Rmd
	R -e 'rmarkdown::render("rcourse.Rmd")'

stats_with_R.html: stats_with_R.Rmd 
	R -e 'rmarkdown::render("stats_with_R.Rmd")'
