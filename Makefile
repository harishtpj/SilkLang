# Makefile program to build SilkLang project
# This makefile is written with accordance to the nmake.exe
CFLAGS = /nologo /EHsc
SRC_DIR = src
BIN_DIR = bin

SRCS = $(SRC_DIR)\*.c
EXE = $(BIN_DIR)\silk.exe

all: refresh $(EXE)

run: refresh $(EXE)
	@$(MAKE) clean
	@echo "---> Running..."
	@$(EXE)

$(EXE): $(OBJS)
	@echo "---> Compiling..."
	@$(CC) $(CFLAGS) $(SRCS) /Fe$@
	@$(MAKE) clean
	@echo "---> Built program"

clean:
	@IF EXIST "*.obj" del /s /q *.obj

refresh: clean
	@IF EXIST $(BIN_DIR)\*.exe del /s /q $(BIN_DIR)\*.exe
	@echo "---> Refreshed directory"