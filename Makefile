%.rtf: ../%.rmd
	echo "rmarkdown::render(\"$<\")" | R --slave

%.html: ../%.rmd
	echo "rmarkdown::render(\"$<\",output_format='html_document',output_dir='.')" | R --slave
