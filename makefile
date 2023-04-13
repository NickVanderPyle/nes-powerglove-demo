.PHONY: clean main rebuild
.DEFAULT_GOAL := rebuild

main: out/main.o
	ld65 --target nes -o "out/main.nes" -m "out/main.nes.map" -Ln "out/main.nes.lbl" --dbgfile "out/main.nes.dbg" "out/main.o"
	python3 ./scripts/fceux-labels-to-nl.py ./out/main.nes.lbl
	python3 ./scripts/fceux-dbg-to-nl.py ./out/main.nes.dbg

out/main.o:
	ca65 --target nes "src/main.asm" -g -o "out/main.o"

clean:
	rm -f out/*.o \
	      out/*.nes \
	      out/*.dbg \
		  out/*.txt \
		  out/*.nl

rebuild:
	$(MAKE) clean
	$(MAKE) main
