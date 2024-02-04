# Makefile program to build SilkLang project
CC = tcc
CFLAGS = 
SRC_DIR = src
BIN_DIR = bin

SRCS = $(wildcard $(SRC_DIR)/*.c)
EXE = $(BIN_DIR)/silk.exe

all: refresh $(EXE)

run: refresh $(EXE)
	@$(MAKE) clean
	@echo "---> Running the interpreter..."
	@$(EXE)

$(EXE): $(SRCS)
	@echo "---> Compiling files..."
	@$(CC) $(CFLAGS) $(SRCS) -o $@
	@$(MAKE) clean
	@echo "---> Built the interpreter"

clean:
	@IF EXIST "$(BIN_DIR)\*.obj" del /s /q "$(BIN_DIR)\*.obj"

refresh: clean
	@IF EXIST "$(BIN_DIR)\*.exe" del /s /q "$(BIN_DIR)\*.exe"
	@echo "---> Refreshed directory"
