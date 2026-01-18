CXX = g++
CC  = gcc

SRC_DIR     = src
RUNTIME_DIR = runtime
LIB_DIR     = lib

BNFC_FILES = Absyn.C Buffer.C Lexer.C Parser.C Printer.C

COMPILER_SRC = $(addprefix $(SRC_DIR)/, $(BNFC_FILES)) \
               $(SRC_DIR)/TypeChecker.C \
               $(SRC_DIR)/AsmGenerator.C \
               $(SRC_DIR)/Main.C

COMPILER_OBJ = $(COMPILER_SRC:.C=.o)

RUNTIME_SRC = $(RUNTIME_DIR)/string.c \
              $(RUNTIME_DIR)/io.c

RUNTIME_OBJ_TMP = $(RUNTIME_SRC:.c=.o)

TARGET = latc_x86_64
RUNTIME_LIB = $(LIB_DIR)/runtime.o

all: $(TARGET) $(RUNTIME_LIB)

$(TARGET): $(COMPILER_OBJ)
	$(CXX) -o $@ $(COMPILER_OBJ)

$(SRC_DIR)/%.o: $(SRC_DIR)/%.C
	$(CXX) -c $< -o $@

$(LIB_DIR):
	mkdir -p $(LIB_DIR)

$(RUNTIME_LIB): $(RUNTIME_OBJ_TMP) | $(LIB_DIR)
	ld -r $(RUNTIME_OBJ_TMP) -o $(RUNTIME_LIB)

$(RUNTIME_DIR)/%.o: $(RUNTIME_DIR)/%.c
	$(CC) -c $< -o $@

clean:
	rm -f $(TARGET)
	rm -f $(SRC_DIR)/*.o
	rm -f $(RUNTIME_DIR)/*.o
	rm -f $(LIB_DIR)/runtime.o