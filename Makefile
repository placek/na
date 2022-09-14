all: book.pdf

app:
	cabal build

volume.html: app
	cabal run > volume.html
	sed -i '1,3d'         volume.html
	sed -i 's/&amp;/\&/g' volume.html
	sed -i 's/&lt;/</g'   volume.html
	sed -i 's/&gt;/>/g'   volume.html
	sed -i 's/&quot;/"/g' volume.html

volume.pdf: volume.html
	docker run --rm -v "`pwd`":/data michaelperrin/prince:latest -o /data/volume.pdf /data/volume.html

book.pdf: volume.pdf
	pdfunite pages/title1.pdf pages/title2.pdf volume.pdf book.pdf

clean:
	rm -rf volume.html volume.pdf book.pdf
