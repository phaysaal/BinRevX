SRC_DIR:=src

TOOLS:=binrevx

TARGET:=native
OCAMLBUILDFLAGS:=-classic-display -j 0 -libs unix
OCAMLBUILD:=ocamlbuild

.PHONY: $(TOOLS) default

default: $(TOOLS)

$(TOOLS):
	$(OCAMLBUILD) $(OCAMLBUILDFLAGS) -I $(SRC_DIR) -build-dir build/$@ $@.$(TARGET)
	ln -sf build/$@/src/$@.$(TARGET) $@
