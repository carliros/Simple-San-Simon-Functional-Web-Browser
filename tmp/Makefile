
OUT = ./out/
AG  = ./src-ag/
SRC = ./src/
DERIVED = ./src-derived/

all: browser

browser: browser-data gen-data
	ghc -i$(SRC):$(DERIVED) --make $(SRC)MainWebBrowserGui.hs -outputdir $(OUT) -o 3sfwbrowser -O2

boxer: browser-data gen-data
	ghc -i$(SRC):$(DERIVED) --make $(SRC)MainBoxerGui.hs -outputdir $(OUT) -o boxer -O2

browser-data:
	uuagc --module --catas --semfuns --data $(AG)FSTreeFase1.ag -P$(AG)
	uuagc --module --catas --semfuns --data $(AG)FSTreeFase2.ag -P$(AG)
	mv $(AG)*.hs $(DERIVED)

gen-data:
	uuagc --module --data $(AG)DataTreeHTML.ag -P$(AG)
	uuagc --module --data $(AG)DataTreeCSS.ag -P$(AG)
	uuagc --signatures --catas --semfuns $(AG)ProcesarEstilo.ag -P$(AG)
	uuagc --signatures --module --catas --semfuns $(AG)NTree.ag -P$(AG)
	uuagc --signatures --catas --semfuns $(AG)FormatTree.ag -P$(AG)
	uuagc --signatures --catas --semfuns --data $(AG)FSBox.ag -P$(AG)
	mv $(AG)*.hs $(DERIVED)

clean:
	rm 3sfwbrowser boxer

