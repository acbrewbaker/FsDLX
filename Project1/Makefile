# Makefile
PROJ_DIR=/u/css/ab67597/5483/Project1/
SRC_DIR=$(PROJ_DIR)FsDLX/src/FsDLX.Assembler/

FSC=fsharpc

FSC_LIB_FLAGS=--target:library
LIB=$(SRC_DIR)Interface.fs
DLL=fsdlx.dll

FSC_EXE_FLAGS=-r $(PROJ_DIR)$(DLL)
SRC_EXE=$(PROJ_DIR)assemble.fs
EXE=assemble.exe

all: $(DLL) $(EXE)

$(EXE): $(SRC_EXE)
	$(FSC) $(SRC_EXE) $(FSC_EXE_FLAGS) -o $(EXE)

$(DLL): 
	$(FSC) $(FSC_LIB_FLAGS) $(LIB) -o $(DLL)

clean:
	rm -rf *.dll
	rm -rf *.exe