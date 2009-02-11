all: intro system apps

intro: publications.bib
	bib2bib -oc intro -ob intro.bib -c '$$key = "Rnews:Feinerer:2008"' publications.bib
	bibtex2html -d -r -nodoc -citefile intro -linebreak -nofooter intro.bib

system: publications.bib
	bib2bib -oc system -ob system.bib -c '$$key = "Feinerer_etal_2008" or $$key = "Feinerer_2008"' publications.bib
	bibtex2html -d -r -nodoc -citefile system -linebreak -nofooter -nf talk talk system.bib

apps: publications.bib
	bib2bib -oc apps -ob apps.bib -c '$$key = "Karatzoglou_Feinerer_2007" or $$key = "Feinerer_Hornik_2008" or $$key = "Feinerer_Karatzoglou_2008" or $$key = "Feinerer_Wild_2007" or $$key = "Karatzoglou_etal_2009"' publications.bib
	bibtex2html -d -r -nodoc -citefile apps -linebreak -nofooter -nf talk talk -nf tar tar.gz apps.bib

.PHONY: clean
clean:
	-rm -f intro intro.bib system system.bib apps apps.bib

.PHONY: realclean
realclean:
	-rm -f intro intro.bib intro_bib.html intro.html system system.bib system_bib.html system.html apps apps.bib apps_bib.html apps.html
