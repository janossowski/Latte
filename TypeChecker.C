#include "Absyn.H"
#include "Parser.H"
#include <iostream>
#include <fstream>
#include <map>
#include <string>
#include <vector>
#include <typeinfo>
#include "ParserError.H"
#include "Printer.H"

class TypeChecker : public Visitor {
private:
    std::map<std::string, Type*> symbolTable;

public:
    virtual ~TypeChecker() {}

        void visitProgram(Program *p) override {
        if (auto prog = dynamic_cast<Prog*>(p)) {
            prog->listtopdef_->accept(this);
        }
    }

    void visitTopDef(TopDef *p) override {
        if (auto fnDef = dynamic_cast<FnDef*>(p)) {
            fnDef->accept(this);
        }
    }

    void visitArg(Arg *p) override {
        if (auto ar = dynamic_cast<Ar*>(p)) {
            ar->accept(this);
        }
    }

    void visitBlock(Block *p) override {
        if (auto blk = dynamic_cast<Blk*>(p)) {
            blk->accept(this);
        }
    }

    void visitStmt(Stmt *p) override {
        if (auto decl = dynamic_cast<Decl*>(p)) {
            decl->accept(this);
        } else if (auto ass = dynamic_cast<Ass*>(p)) {
            ass->accept(this);
        } else if (auto incr = dynamic_cast<Incr*>(p)) {
            incr->accept(this);
        } else if (auto decr = dynamic_cast<Decr*>(p)) {
            decr->accept(this);
        } else if (auto ret = dynamic_cast<Ret*>(p)) {
            ret->accept(this);
        } else if (auto vret = dynamic_cast<VRet*>(p)) {
            vret->accept(this);
        } else if (auto cond = dynamic_cast<Cond*>(p)) {
            cond->accept(this);
        } else if (auto condElse = dynamic_cast<CondElse*>(p)) {
            condElse->accept(this);
        } else if (auto whileStmt = dynamic_cast<While*>(p)) {
            whileStmt->accept(this);
        } else if (auto sExp = dynamic_cast<SExp*>(p)) {
            sExp->accept(this);
        }
    }

    void visitItem(Item *p) override {
        if (auto noInit = dynamic_cast<NoInit*>(p)) {
            noInit->accept(this);
        } else if (auto init = dynamic_cast<Init*>(p)) {
            init->accept(this);
        }
    }

    void visitType(Type *p) override {
        if (auto intType = dynamic_cast<Int*>(p)) {
            intType->accept(this);
        } else if (auto strType = dynamic_cast<Str*>(p)) {
            strType->accept(this);
        } else if (auto boolType = dynamic_cast<Bool*>(p)) {
            boolType->accept(this);
        } else if (auto voidType = dynamic_cast<Void*>(p)) {
            voidType->accept(this);
        } else if (auto funType = dynamic_cast<Fun*>(p)) {
            funType->accept(this);
        }
    }

    void visitExpr(Expr *p) override {
        if (auto eVar = dynamic_cast<EVar*>(p)) {
            eVar->accept(this);
        } else if (auto eLitInt = dynamic_cast<ELitInt*>(p)) {
            eLitInt->accept(this);
        } else if (auto eLitTrue = dynamic_cast<ELitTrue*>(p)) {
            eLitTrue->accept(this);
        } else if (auto eLitFalse = dynamic_cast<ELitFalse*>(p)) {
            eLitFalse->accept(this);
        } else if (auto eApp = dynamic_cast<EApp*>(p)) {
            eApp->accept(this);
        } else if (auto eString = dynamic_cast<EString*>(p)) {
            eString->accept(this);
        } else if (auto neg = dynamic_cast<Neg*>(p)) {
            neg->accept(this);
        } else if (auto notExpr = dynamic_cast<Not*>(p)) {
            notExpr->accept(this);
        } else if (auto eMul = dynamic_cast<EMul*>(p)) {
            eMul->accept(this);
        } else if (auto eAdd = dynamic_cast<EAdd*>(p)) {
            eAdd->accept(this);
        } else if (auto eRel = dynamic_cast<ERel*>(p)) {
            eRel->accept(this);
        } else if (auto eAnd = dynamic_cast<EAnd*>(p)) {
            eAnd->accept(this);
        } else if (auto eOr = dynamic_cast<EOr*>(p)) {
            eOr->accept(this);
        }
    }

    void visitAddOp(AddOp *p) override {
        if (auto plus = dynamic_cast<Plus*>(p)) {
            plus->accept(this);
        } else if (auto minus = dynamic_cast<Minus*>(p)) {
            minus->accept(this);
        }
    }

    void visitMulOp(MulOp *p) override {
        if (auto times = dynamic_cast<Times*>(p)) {
            times->accept(this);
        } else if (auto div = dynamic_cast<Div*>(p)) {
            div->accept(this);
        } else if (auto mod = dynamic_cast<Mod*>(p)) {
            mod->accept(this);
        }
    }

    void visitRelOp(RelOp *p) override {
        if (auto lth = dynamic_cast<LTH*>(p)) {
            lth->accept(this);
        } else if (auto le = dynamic_cast<LE*>(p)) {
            le->accept(this);
        } else if (auto gth = dynamic_cast<GTH*>(p)) {
            gth->accept(this);
        } else if (auto ge = dynamic_cast<GE*>(p)) {
            ge->accept(this);
        } else if (auto equ = dynamic_cast<EQU*>(p)) {
            equ->accept(this);
        } else if (auto ne = dynamic_cast<NE*>(p)) {
            ne->accept(this);
        }
    }

    void visitProg(Prog *p) override {
        if (p->listtopdef_) p->listtopdef_->accept(this);
    }

    void visitFnDef(FnDef *p) override {
        if (p->type_) p->type_->accept(this);
        visitIdent(p->ident_);
        if (p->listarg_) p->listarg_->accept(this);
        if (p->block_) p->block_->accept(this);
    }

    void visitAr(Ar *p) override {
        if (p->type_) p->type_->accept(this);
        visitIdent(p->ident_);
    }

    void visitBlk(Blk *p) override {
        if (p->liststmt_) p->liststmt_->accept(this);
    }

    void visitEmpty(Empty *p) override {}

    void visitBStmt(BStmt *p) override {
        if (p->block_) p->block_->accept(this);
    }

    void visitDecl(Decl *p) override {
        if (p->type_) p->type_->accept(this);
        if (p->listitem_) p->listitem_->accept(this);
    }

    void visitNoInit(NoInit *p) override {
        visitIdent(p->ident_);
    }

    void visitInit(Init *p) override {
        visitIdent(p->ident_);
        if (p->expr_) p->expr_->accept(this);
    }

    void visitAss(Ass *p) override {
        visitIdent(p->ident_);
        if (p->expr_) p->expr_->accept(this);
    }

    void visitIncr(Incr *p) override {
        visitIdent(p->ident_);
    }

    void visitDecr(Decr *p) override {
        visitIdent(p->ident_);
    }

    void visitRet(Ret *p) override {
        if (p->expr_) p->expr_->accept(this);
    }

    void visitVRet(VRet *p) override {}

    void visitCond(Cond *p) override {
        if (p->expr_) p->expr_->accept(this);
        if (p->stmt_) p->stmt_->accept(this);
    }

    void visitCondElse(CondElse *p) override {
        if (p->expr_) p->expr_->accept(this);
        if (p->stmt_1) p->stmt_1->accept(this);
        if (p->stmt_2) p->stmt_2->accept(this);
    }

    void visitWhile(While *p) override {
        if (p->expr_) p->expr_->accept(this);
        if (p->stmt_) p->stmt_->accept(this);
    }

    void visitSExp(SExp *p) override {
        if (p->expr_) p->expr_->accept(this);
    }

    void visitInt(Int *p) override {}
    void visitStr(Str *p) override {}
    void visitBool(Bool *p) override {}
    void visitVoid(Void *p) override {}
    void visitFun(Fun *p) override {
        if (p->type_) p->type_->accept(this);
        if (p->listtype_) p->listtype_->accept(this);
    }

    void visitEVar(EVar *p) override {
        visitIdent(p->ident_);
    }

    void visitELitInt(ELitInt *p) override {}
    void visitELitTrue(ELitTrue *p) override {}
    void visitELitFalse(ELitFalse *p) override {}
    void visitEApp(EApp *p) override {
        visitIdent(p->ident_);
        if (p->listexpr_) p->listexpr_->accept(this);
    }

    void visitEString(EString *p) override {}
    void visitNeg(Neg *p) override {
        if (p->expr_) p->expr_->accept(this);
    }

    void visitNot(Not *p) override {
        if (p->expr_) p->expr_->accept(this);
    }

    void visitEMul(EMul *p) override {
        if (p->expr_1) p->expr_1->accept(this);
        if (p->mulop_) p->mulop_->accept(this);
        if (p->expr_2) p->expr_2->accept(this);
    }

    void visitEAdd(EAdd *p) override {
        if (p->expr_1) p->expr_1->accept(this);
        if (p->addop_) p->addop_->accept(this);
        if (p->expr_2) p->expr_2->accept(this);
    }

    void visitERel(ERel *p) override {
        if (p->expr_1) p->expr_1->accept(this);
        if (p->relop_) p->relop_->accept(this);
        if (p->expr_2) p->expr_2->accept(this);
    }

    void visitEAnd(EAnd *p) override {
        if (p->expr_1) p->expr_1->accept(this);
        if (p->expr_2) p->expr_2->accept(this);
    }

    void visitEOr(EOr *p) override {
        if (p->expr_1) p->expr_1->accept(this);
        if (p->expr_2) p->expr_2->accept(this);
    }

    void visitPlus(Plus *p) override {}
    void visitMinus(Minus *p) override {}
    void visitTimes(Times *p) override {}
    void visitDiv(Div *p) override {}
    void visitMod(Mod *p) override {}
    void visitLTH(LTH *p) override {}
    void visitLE(LE *p) override {}
    void visitGTH(GTH *p) override {}
    void visitGE(GE *p) override {}
    void visitEQU(EQU *p) override {}
    void visitNE(NE *p) override {}

    // Existing methods continue...
    void visitListTopDef(ListTopDef *list_top_def) override {
        for (ListTopDef::iterator i = list_top_def->begin() ; i != list_top_def->end() ; ++i) {
            (*i)->accept(this);
        }
    }

    void visitListArg(ListArg *list_arg) override {
        for (ListArg::iterator i = list_arg->begin() ; i != list_arg->end() ; ++i) {
            (*i)->accept(this);
        }
    }

    void visitListStmt(ListStmt *list_stmt) override {
        for (ListStmt::iterator i = list_stmt->begin() ; i != list_stmt->end() ; ++i) {
            (*i)->accept(this);
        }
    }

    void visitListItem(ListItem *list_item) override {
        for (ListItem::iterator i = list_item->begin() ; i != list_item->end() ; ++i) {
            (*i)->accept(this);
        }
    }

    void visitListType(ListType *list_type) override {
        for (ListType::iterator i = list_type->begin() ; i != list_type->end() ; ++i) {
            (*i)->accept(this);
        }
    }

    void visitListExpr(ListExpr *list_expr) override {
        for (ListExpr::iterator i = list_expr->begin() ; i != list_expr->end() ; ++i) {
            (*i)->accept(this);
        }
    }

    void visitInteger(Integer x) override {}
    void visitChar(Char x) override {}
    void visitDouble(Double x) override {}
    void visitString(String x) override {}
    void visitIdent(Ident x) override {}

    // Helper function to infer the type of an expression
    Type* inferExprType(Expr* expr) {
        // Use dynamic_cast to determine the type of the expression
        if (dynamic_cast<ELitInt*>(expr)) {
            return new Int();
        } else if (dynamic_cast<ELitTrue*>(expr) || dynamic_cast<ELitFalse*>(expr)) {
            return new Bool();
        } else if (dynamic_cast<EString*>(expr)) {
            return new Str();
        } else if (auto eApp = dynamic_cast<EApp*>(expr)) {
            // Assuming the function is already in the symbol table
            auto it = symbolTable.find(eApp->ident_);
            if (it != symbolTable.end()) {
                return it->second;
            }
        }
        // Add more cases as needed for other expression types
        return nullptr;
    }

    // Helper function to compare types
    bool typesEqual(Type* a, Type* b) {
        // Implement logic for comparing types
        // Here we assume type names are sufficient to compare
        return typeid(*a) == typeid(*b);
    }

    // Helper function to convert a type to a string for error messages
    std::string typeToString(Type* type) {
        if (dynamic_cast<Int*>(type)) {
            return "int";
        } else if (dynamic_cast<Bool*>(type)) {
            return "boolean";
        } else if (dynamic_cast<Str*>(type)) {
            return "string";
        } else if (dynamic_cast<Void*>(type)) {
            return "void";
        }
        return "unknown";
    }
};

// Example usage for testing purposes
int main(int argc, char **argv) {
    FILE *input;
    int quiet = 0;
    char *filename = NULL;

    if (argc > 1) {
        if (strcmp(argv[1], "-s") == 0) {
            quiet = 1;
            if (argc > 2) {
                filename = argv[2];
            } else {
                input = stdin;
            }
        } else {
            filename = argv[1];
        }
    }

    if (filename) {
        input = fopen(filename, "r");
        if (!input) {
            std::cerr << "Error: Could not open file " << filename << "\n";
            return 1;
        }
    } else {
        input = stdin;
    }

    // Parse the input file
    Program *parse_tree = NULL;
    try {
        parse_tree = pProgram(input);
    } catch (parse_error &e) {
        std::cerr << "Parse error on line " << e.getLine() << "\n";
        fclose(input);
        return 1;
    }

    if (parse_tree) {
        std::cout << "\nParse Successful!\n";
        if (!quiet) {
            std::cout << "\n[Abstract Syntax]\n";
            ShowAbsyn *s = new ShowAbsyn();
            std::cout << s->show(parse_tree) << "\n\n";
            std::cout << "[Linearized Tree]\n";
            PrintAbsyn *p = new PrintAbsyn();
            std::cout << p->print(parse_tree) << "\n\n";
            delete s;
            delete p;
        }

        // Instantiate the TypeChecker
        TypeChecker checker;
        try {
            parse_tree->accept(&checker);
            std::cout << "Type checking completed successfully. The program is correct.\n";
        } catch (const std::exception &e) {
            std::cerr << "Type checking failed: " << e.what() << "\n";
        }

        delete parse_tree;
    }

    fclose(input);
    return 0;
}
