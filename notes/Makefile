%.rtf: %.rmd
	echo "rmarkdown::render(\"$<\")" | R --slave

%.html: %.rmd
	echo "rmarkdown::render(\"$<\",output_format='html_document')" | R --slave

%.pdf: %.rmd
	echo "rmarkdown::render(\"$<\",output_format='pdf_document')" | R --slave
