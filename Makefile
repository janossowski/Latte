CXX = g++
BNFC_FILES = Absyn.C Buffer.C Lexer.C Parser.C Printer.C TypeChecker.C
TARGET = latte

all: $(TARGET)

$(TARGET): $(BNFC_FILES)
	$(CXX) -o $(TARGET) $(BNFC_FILES)

clean:
	rm -f $(TARGET) *.o
