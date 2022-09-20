TARGET := result.pdf
INFO   := pages/02-koine.pdf pages/03-500.pdf
VOLUME := pages/99-volume
PAGES  := pages/00-title.pdf pages/01-title.pdf pages/blank.pdf $(INFO) $(VOLUME).pdf

$(TARGET): $(PAGES)
	pdfunite $? $@

pages/%.pdf: pages/%.html
	docker run --rm -v "`pwd`":/data michaelperrin/prince:latest -o /data/$@ /data/$<

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
	rm -rf $(VOLUME).html $(VOLUME).pdf $(INFO) $(TARGET)
