%.rtf: %.rmd
	echo "rmarkdown::render(\"$<\")" | R --slave
