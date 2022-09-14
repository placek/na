all: result.pdf

app:
	cabal build

pages/99-volume.html: app
	cabal run > pages/99-volume.html
	sed -i '1,3d'         pages/99-volume.html
	sed -i 's/&amp;/\&/g' pages/99-volume.html
	sed -i 's/&lt;/</g'   pages/99-volume.html
	sed -i 's/&gt;/>/g'   pages/99-volume.html
	sed -i 's/&quot;/"/g' pages/99-volume.html

pages/99-volume.pdf: pages/99-volume.html
	docker run --rm -v "`pwd`":/data michaelperrin/prince:latest -o /data/pages/99-volume.pdf /data/pages/99-volume.html

result.pdf: pages/99-volume.pdf
	pdfunite pages/*.pdf result.pdf

clean:
	rm -rf pages/99-volume.html pages/99-volume.pdf result.pdf
