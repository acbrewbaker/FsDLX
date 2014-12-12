# Makefile
PROJ_DIR=/u/css/ab67597/5483/FsDLX/Project2/
PROJ_DIR_WIN=H:/FsDLX/Project2/

SRC_DIR=$(PROJ_DIR)src/FsDLX.Tomasulo/
SRC_DIR_WIN=$(PROJ_DIR_WIN)src/FsDLX.Tomasulo/

FSC=fsharpc
FSC_WIN=fsc

FSC_LIB_FLAGS=--target:library

LIB=$(SRC_DIR)Common.fs $(SRC_DIR)Grammar.fs $(SRC_DIR)Patterns.fs $(SRC_DIR)Interface.fs 
LIB_WIN=$(SRC_DIR_WIN)Common.fs $(SRC_DIR_WIN)Grammar.fs $(SRC_DIR_WIN)Patterns.fs $(SRC_DIR_WIN)Interface.fs

DLL=fsdlx.dll

FSC_EXE_FLAGS=-r $(PROJ_DIR)$(DLL)
FSC_EXE_FLAGS_WIN=-r $(PROJ_DIR_WIN)$(DLL)

ASM_SRC=$(PROJ_DIR)assemble.fs
ASM_SRC_WIN=$(PROJ_DIR_WIN)assemble.fs

ASM_EXE=$(PROJ_DIR)assemble.exe
ASM_EXE_WIN=$(PROJ_DIR_WIN)assemble.exe

all: $(DLL) $(ASM_EXE)

$(ASM_EXE): $(ASM_SRC)
	$(FSC) $(ASM_SRC) $(FSC_EXE_FLAGS) -o $(ASM_EXE)

$(DLL): 
	$(FSC) $(FSC_LIB_FLAGS) $(LIB) -o $(DLL)

clean:
	rm -rf *.dll
	rm -rf *.exe

win:
	$(FSC_WIN) $(FSC_LIB_FLAGS) $(LIB_WIN) -o $(DLL)
	$(FSC_WIN) $(ASM_SRC_WIN) $(FSC_EXE_FLAGS_WIN) -o $(ASM_EXE_WIN)