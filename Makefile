OUT = ./out/
AG  = ./src/ag/
SRC = ./src/

all: gen-data browser boxer

hpc: gen-data browser-data
	ghc -i$(SRC) -fhpc --make -outputdir $(OUT) -o 3swbrowser $(SRC)MainWebBrowserGui.hs

ehpc:
	hpc markup --destdir ./hpc/html 3swbrowser

prof: gen-data browser-data
	ghc -i$(SRC) -prof -auto-all --make -outputdir $(OUT) -o 3swbrowser $(SRC)MainWebBrowserGui.hs

browser: gen-data browser-data
	ghc -i$(SRC) --make $(SRC)MainWebBrowserGui.hs -outputdir $(OUT) -o 3swbrowser -O2

boxer: gen-data boxer-data
	ghc -i$(SRC) --make $(SRC)MainBoxerF2Gui.hs -outputdir $(OUT) -o boxer -O2

boxer-data:
	uuagc --module --catas --semfuns --data $(AG)FSBox.ag -P$(AG)
	mv $(AG)*.hs $(SRC)

browser-data:
	uuagc --module --catas --semfuns --data $(AG)FSTreeFase1.ag -P$(AG)
	uuagc --module --catas --semfuns --data $(AG)FSTreeFase2.ag -P$(AG)
	mv $(AG)*.hs $(SRC)

gen-data:
	#uuagc --module --data $(AG)DataTreeHTML.ag -P$(AG)
	uuagc --module --data $(AG)DataTreeCSS.ag -P$(AG)
	uuagc --module --catas --semfuns        $(AG)StyleProcess.ag -P$(AG)
	uuagc --module --catas --semfuns --data $(AG)NTree.ag -P$(AG)
	mv $(AG)*.hs $(SRC)
