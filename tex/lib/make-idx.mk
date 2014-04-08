OBJS_TO_DEL+=	$(TEX).ilg $(TEX).idx $(TEX).ind
LATDEPS+=	$(TEX).ilg
#NO_SECOND_RUN=	1

$(TEX).ilg:	$(TEX).tex
		$(LAT) $(TEX).tex
		makeindex $(TEX).idx
		$(LAT) $(TEX).tex
		makeindex $(TEX).idx
