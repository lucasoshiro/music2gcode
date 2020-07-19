music2gcode: *.hs
	ghc -dynamic music2gcode.hs -o music2gcode

.PHONY: clean
clean:
	rm -f **.o **.hi music2gcode

