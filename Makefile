TARGET := result.pdf
PAGES  := $(wildcard pages/*.pdf)
VOLUME := pages/99-volume

$(TARGET): $(PAGES) $(VOLUME).pdf
	pdfunite $? $@

pages/02-500-introduction.pdf: pages/02-500-introduction.html
	docker run --rm -v "`pwd`":/data michaelperrin/prince:latest -o /data/$@ /data/$?

$(VOLUME).pdf: $(VOLUME).html
	docker run --rm -v "`pwd`":/data michaelperrin/prince:latest -o /data/$@ /data/$?

$(VOLUME).html: build
	cabal run > $@
	@echo "FIXME: removing html entities with actual chars"
	sed -i '1,4d'         $@
	sed -i 's/&amp;/\&/g' $@
	sed -i 's/&lt;/</g'   $@
	sed -i 's/&gt;/>/g'   $@
	sed -i 's/&quot;/"/g' $@

build:
	cabal build

edit: $(TARGET)
	okular $?

.PHONY: clean
clean:
	rm -rf $(VOLUME).html $(VOLUME).pdf $(TARGET)
