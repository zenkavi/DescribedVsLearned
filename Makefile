#!/usr/bin/make -f

.PHONY: all clean
.DELETE_ON_ERROR:
.SECONDARY:

all: APA_Draft.pdf

clean:
	rm -rf APA_Draft.pdf APA_Draft.tex

APA_Draft.Rmd: 00_get_behavioral_data.R 01_clean_behavioral_data.R 02_fit_twoValSystemsWithRL.R

APA_Draft.pdf: APA_Draft.Rmd
	Rscript -e 'rmarkdown::render("$<")'