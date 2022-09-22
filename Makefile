.PHONY: clean edit

target   := result.pdf
articles := static/00-header.html static/01-koine.html static/02-format.html static/03-j.html static/99-footer.html
pages    := pages/00-title.pdf pages/01-info.pdf pages/02-blank.pdf pages/03-articles.pdf pages/99-volume.pdf

$(target): $(pages)
	pdfunite $? $@

pages/%.pdf: pages/%.html
	docker run --rm -v "`pwd`":/data michaelperrin/prince:latest -o /data/$@ /data/$<

pages/03-articles.html: $(articles)
	cat $? > $@

pages/99-volume.html: build
	cabal run > $@
	@echo "FIXME: removing html entities with actual chars"
	sed -i '1,4d'         $@
	sed -i 's/&amp;/\&/g' $@
	sed -i 's/&lt;/</g'   $@
	sed -i 's/&gt;/>/g'   $@
	sed -i 's/&quot;/"/g' $@

build:
	cabal build

edit: $(target)
	okular $?

clean:
	rm -rf pages/99-volume.html pages/99-volume.pdf pages/03-articles.html pages/03-articles.pdf $(target)
