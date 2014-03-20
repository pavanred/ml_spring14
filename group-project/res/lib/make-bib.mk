BIB=		bibtex
LATDEPS+=	$(TEX).bbl
OBJS_TO_DEL+=	$(TEX).bbl $(TEX).blg

$(TEX).bbl:	$(TEX).bib
		$(LAT) $(TEX).tex
		$(BIB) $(TEX)
