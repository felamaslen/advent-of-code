clean:
	rm -rf lib

build:
	mkdir -p lib && ghc -dynamic ./src/Main.hs -o ./lib/Main

run:
	./lib/Main

build_alt:
	mkdir -p lib && gcc ./src/alt.c -o ./lib/alt

run_alt:
	./lib/alt
