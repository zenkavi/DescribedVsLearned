#!/usr/bin/make -f

.PHONY: all clean
.DELETE_ON_ERROR:
.SECONDARY:

all: APA_Draft.pdf

clean:
	rm -rf APA_Draft.pdf

APA_Draft.pdf: APA_Draft.Rmd
	Rscript -e 'rmarkdown::render("$<")'