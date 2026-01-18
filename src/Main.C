#include <cstdio>
#include <cstring>
#include <iostream>
#include <fstream>
#include <exception>
#include <cstdlib>

#include "Absyn.H"
#include "Parser.H"
#include "ParserError.H"

#include "TypeChecker.H"
#include "AsmGenerator.H"

static void splitPath(const std::string &path,
                      std::string &dir,
                      std::string &base)
{
    size_t pos = path.find_last_of("/\\");
    if (pos == std::string::npos) {
        dir = ".";
        base = path;
    } else {
        dir = path.substr(0, pos);
        base = path.substr(pos + 1);
    }

    size_t dot = base.find_last_of('.');
    if (dot != std::string::npos)
        base = base.substr(0, dot);
}

int main(int argc, char **argv)
{
    FILE *input = stdin;
    char *filename = nullptr;
    int quiet = 0;

    if (argc > 1) {
        if (strcmp(argv[1], "-s") == 0) {
            quiet = 1;
            if (argc > 2)
                filename = argv[2];
        } else {
            filename = argv[1];
        }
    }

    if (!filename) {
        std::cerr << "ERROR\nNo input file provided\n";
        return 1;
    }

    input = fopen(filename, "r");
    if (!input) {
        std::cerr << "ERROR\nCould not open file " << filename << "\n";
        return 1;
    }

    Program *parse_tree = nullptr;

    try {
        parse_tree = pProgram(input);
    } catch (parse_error &e) {
        std::cerr << "ERROR\nParse error on line " << e.getLine() << "\n";
        fclose(input);
        return 1;
    }

    if (!parse_tree) {
        std::cerr << "ERROR\nParsing failed\n";
        fclose(input);
        return 1;
    }

    try {
        TypeChecker checker;
        parse_tree->accept(&checker);
    } catch (const std::exception &e) {
        std::cerr << "ERROR\nType checking failed: " << e.what() << "\n";
        delete parse_tree;
        fclose(input);
        return 1;
    }

    std::string dir, base;
    splitPath(filename, dir, base);

    std::string asmFile = dir + "/" + base + ".s";
    std::string exeFile = dir + "/" + base;

    try {
        std::ofstream asmOut(asmFile);
        if (!asmOut) {
            throw std::runtime_error("Cannot open output file " + asmFile);
        }

        AsmGenerator gen(asmOut);
        parse_tree->accept(&gen);
        gen.emitStringLiterals();

        asmOut.close();
    } catch (const std::exception &e) {
        std::cerr << "ERROR\nCode generation failed: " << e.what() << "\n";
        delete parse_tree;
        fclose(input);
        return 1;
    }

    std::string linkCmd =
        "gcc " + asmFile + " lib/runtime.o -o " + exeFile;

    int rc = system(linkCmd.c_str());
    if (rc != 0) {
        std::cerr << "ERROR\nLinking failed\n";
        delete parse_tree;
        fclose(input);
        return 1;
    }

    if (!quiet)
        std::cout << "OK\n";

    delete parse_tree;
    fclose(input);
    return 0;
}
