#include "Absyn.H"
#include "Parser.H"
#include <iostream>
#include <fstream>
#include <map>
#include <set>
#include <string>
#include <vector>
#include <typeinfo>
#include "ParserError.H"
#include "Printer.H"

struct FunctionSignature {
    Type* returnType;
    std::vector<Type*> argumentTypes;
};

class TypeChecker : public Visitor {
private:
    std::map<std::string, Type*> symbolTable;
    std::map<std::string, FunctionSignature> functionTable;
    std::set<std::string> currentBlockDeclarations;
    Type* currentFunctionReturnType = nullptr;
    bool hasReturnStatement = false;    
    
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
        functionTable["printString"] = {new Void(), {new Str()}};
        functionTable["printInt"] = {new Void(), {new Int()}};

        if (p->listtopdef_) {
            // First pass: Add function signatures to the function table for all functions
            for (TopDef* topDef : *p->listtopdef_) {
                FnDef* fnDef = dynamic_cast<FnDef*>(topDef);
                if (fnDef) {
                    Type* returnType = fnDef->type_;
                    std::string functionName = fnDef->ident_;

                    if (functionTable.find(functionName) != functionTable.end()) {
                        throw std::runtime_error("Naming conflict: A function with the name '" + functionName + "' already exists.");
                    }

                    std::vector<Type*> argumentTypes;
                    std::set<std::string> argumentNames;
                    if (fnDef->listarg_) {
                        for (Arg* arg : *fnDef->listarg_) {
                            if (Ar* argument = dynamic_cast<Ar*>(arg)) {
                                if (argumentNames.find(argument->ident_) != argumentNames.end()) {
                                    throw std::runtime_error("Naming conflict: Duplicate argument name '" + argument->ident_ + "' in function '" + functionName + "'.");
                                }
                                argumentNames.insert(argument->ident_);
                                argumentTypes.push_back(argument->type_);
                            }
                        }
                    }

                    functionTable[functionName] = {returnType, argumentTypes};
                }
            }

            // Second pass: Type check each function
            for (TopDef* topDef : *p->listtopdef_) {
                topDef->accept(this);
            }

            // Ensure there is a valid 'main' function of type int
            auto mainIt = functionTable.find("main");
            if (mainIt == functionTable.end()) {
                throw std::runtime_error("Program is missing a 'main' function.");
            }
            if (!dynamic_cast<Int*>(mainIt->second.returnType)) {
                throw std::runtime_error("The 'main' function must have a return type of int.");
            }
            if (!mainIt->second.argumentTypes.empty()) {
                throw std::runtime_error("The 'main' function must not take any arguments.");
            }
        }
    }

    void visitFnDef(FnDef *p) override {
        Type* returnType = p->type_;
        std::string functionName = p->ident_;

        currentFunctionReturnType = returnType;
        hasReturnStatement = false;

        std::vector<Type*> argumentTypes;
        std::set<std::string> argumentNames;
        if (p->listarg_) {
            for (Arg* arg : *p->listarg_) {
                if (Ar* argument = dynamic_cast<Ar*>(arg)) {
                    if (argumentNames.find(argument->ident_) != argumentNames.end()) {
                        throw std::runtime_error("Naming conflict: Duplicate argument name '" + argument->ident_ + "' in function '" + functionName + "'.");
                    }
                    argumentNames.insert(argument->ident_);
                    argumentTypes.push_back(argument->type_);
                    symbolTable[argument->ident_] = argument->type_;
                }
            }
        }

        if (p->block_) {
            p->block_->accept(this);
        }

        if (!dynamic_cast<Void*>(returnType) && !hasReturnStatement) {
            throw std::runtime_error("Type error: Function '" + functionName + "' must have a return statement.");
        }

        // Reset current function return type
        currentFunctionReturnType = nullptr;
    }

    void visitAr(Ar *p) override {
        if (p->type_) p->type_->accept(this);
        visitIdent(p->ident_);
    }

    void visitBlk(Blk *p) override {
        std::map<std::string, Type*> savedSymbolTable = symbolTable; // Save the current symbol table
        std::set<std::string> savedBlockDeclarations = currentBlockDeclarations; // Save the current block declarations
        bool savedHasReturnStatement = hasReturnStatement; // Save the return statement flag
        currentBlockDeclarations.clear(); // Start a fresh set for the new block

        if (p->liststmt_) p->liststmt_->accept(this);

        symbolTable = savedSymbolTable; // Restore the symbol table
        currentBlockDeclarations = savedBlockDeclarations;
        hasReturnStatement = hasReturnStatement || savedHasReturnStatement;
    }

    void visitEmpty(Empty *p) override {}

    void visitBStmt(BStmt *p) override {
        if (p->block_) p->block_->accept(this);
    }

    void visitDecl(Decl *p) override {
        if (p->type_) p->type_->accept(this);
        for (ListItem::iterator i = p->listitem_->begin(); i != p->listitem_->end(); ++i) {
            (*i)->accept(this);
            NoInit *noInit = dynamic_cast<NoInit *>(*i);
            Init *init = dynamic_cast<Init *>(*i);
            if (noInit) {
                std::string varName = noInit->ident_;
                if (currentBlockDeclarations.find(varName) != currentBlockDeclarations.end()) {
                    throw std::runtime_error("Naming conflict: Variable '" + varName + "' is already declared in this block.");
                }
                currentBlockDeclarations.insert(varName);
                symbolTable[varName] = p->type_;
            } else if (init) {
                std::string varName = init->ident_;
                if (currentBlockDeclarations.find(varName) != currentBlockDeclarations.end()) {
                    throw std::runtime_error("Naming conflict: Variable '" + varName + "' is already declared in this block.");
                }
                currentBlockDeclarations.insert(varName);
                symbolTable[varName] = p->type_;

                Type* varType = p->type_;
                Type* exprType = inferExprType(init->expr_);
                if (!typesEqual(varType, exprType)) {
                    throw std::runtime_error("Type error: Type mismatch in initialization of variable '" + varName + "'.");
                }
            }
        }
    }

    void visitNoInit(NoInit *p) override {
        visitIdent(p->ident_);
    }

    void visitInit(Init *p) override {
        visitIdent(p->ident_);
        if (p->expr_) p->expr_->accept(this);
    }

    void visitAss(Ass *p) override {
        if (symbolTable.find(p->ident_) == symbolTable.end()) {
            throw std::runtime_error("Type error: Variable '" + p->ident_ + "' is not declared.");
        }

        p->expr_->accept(this);

        Type* varType = symbolTable[p->ident_];
        Type* exprType = inferExprType(p->expr_);
        if (!typesEqual(varType, exprType)) {
            throw std::runtime_error("Type error: Type mismatch in assignment to variable '" + p->ident_ + "'.");
        }
    }

    void visitIncr(Incr *p) override {
        auto it = symbolTable.find(p->ident_);
        if (it == symbolTable.end()) {
            throw std::runtime_error("Type error: Variable '" + p->ident_ + "' is not declared.");
        }

        if (!dynamic_cast<Int*>(it->second)) {
            throw std::runtime_error("Type error: Increment can only be performed on variables of type int.");
        }
    }

    void visitDecr(Decr *p) override {
        auto it = symbolTable.find(p->ident_);
        if (it == symbolTable.end()) {
            throw std::runtime_error("Type error: Variable '" + p->ident_ + "' is not declared.");
        }

        if (!dynamic_cast<Int*>(it->second)) {
            throw std::runtime_error("Type error: Decrement can only be performed on variables of type int.");
        }
    }

    void visitRet(Ret *p) override {
        Type* exprType = inferExprType(p->expr_);
        if (!typesEqual(currentFunctionReturnType, exprType)) {
            throw std::runtime_error("Type error: Return type does not match function return type.");
        }
        hasReturnStatement = true;
    }

    void visitVRet(VRet *p) override {
        if (!dynamic_cast<Void*>(currentFunctionReturnType)) {
            throw std::runtime_error("Type error: Void return statement in a non-void function.");
        }
        hasReturnStatement = true;
    }

    void visitCond(Cond *p) override {
        Type* condType = inferExprType(p->expr_);
        if (!dynamic_cast<Bool*>(condType)) {
            throw std::runtime_error("Type error: Condition in 'if' statement must be a boolean.");
        }
        bool initialHasReturnStatement = hasReturnStatement;
        if (isAlwaysTrue(p->expr_)) {
            p->stmt_->accept(this);
            hasReturnStatement = initialHasReturnStatement || hasReturnStatement;
        } else if (!isAlwaysFalse(p->expr_)) {
            p->stmt_->accept(this);
        }
    }

    void visitCondElse(CondElse *p) override {
        Type* condType = inferExprType(p->expr_);
        if (!dynamic_cast<Bool*>(condType)) {
            throw std::runtime_error("Type error: Condition in 'if-else' statement must be a boolean.");
        }
        bool thenHasReturn = false;
        bool elseHasReturn = false;

        // If the condition is always true or always false only the respective branch needs to return
        if (isAlwaysTrue(p->expr_)) {
            p->stmt_1->accept(this);
        } else if (isAlwaysFalse(p->expr_)) {
            p->stmt_2->accept(this);
        } else {
            p->stmt_1->accept(this);
            thenHasReturn = hasReturnStatement;
            hasReturnStatement = false;
            p->stmt_2->accept(this);
            elseHasReturn = hasReturnStatement;
            hasReturnStatement = thenHasReturn && elseHasReturn;
        }
    }

    void visitWhile(While *p) override {
        // Visit the condition and check if it can be evaluated
        Type* condType = inferExprType(p->expr_);
        if (!dynamic_cast<Bool*>(condType)) {
            throw std::runtime_error("Type error: Condition in 'while' statement must be a boolean.");
        }
        if (!isAlwaysFalse(p->expr_)) {
            bool savedHasReturnStatement = hasReturnStatement;
            p->stmt_->accept(this);
            hasReturnStatement = savedHasReturnStatement || hasReturnStatement;
        }
    }


    bool isAlwaysTrue(Expr* expr) {
        if (ELitTrue* litTrue = dynamic_cast<ELitTrue*>(expr)) {
            return true;
        }
        return false;
    }

    bool isAlwaysFalse(Expr* expr) {
        if (ELitFalse* litFalse = dynamic_cast<ELitFalse*>(expr)) {
            return true;
        }
        return false;
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
        auto it = functionTable.find(p->ident_);
        if (it == functionTable.end()) {
            throw std::runtime_error("Type error: Function '" + std::string(p->ident_) + "' not declared.");
        }

        FunctionSignature signature = it->second;
        std::vector<Type*> argumentTypes = signature.argumentTypes;

        if (p->listexpr_->size() != argumentTypes.size()) {
            throw std::runtime_error("Type error: Incorrect number of arguments for function '" + std::string(p->ident_) + "'.");
        }

        int index = 0;
        for (ListExpr::iterator i = p->listexpr_->begin(); i != p->listexpr_->end(); ++i, ++index) {
            (*i)->accept(this);
            Type* exprType = inferExprType(*i);
            if (!typesEqual(exprType, argumentTypes[index])) {
                throw std::runtime_error("Type error: Argument " + std::to_string(index + 1) + " of function '" + std::string(p->ident_) + "' has incorrect type.");
            }
        }
    }

    void visitEString(EString *p) override {}
    void visitNeg(Neg *p) override {
        if (p->expr_) p->expr_->accept(this);
    }

    void visitNot(Not *p) override {
        if (p->expr_) p->expr_->accept(this);
    }

    void visitEMul(EMul *p) override {
        p->expr_1->accept(this);
        p->expr_2->accept(this);
        Type *leftType = inferExprType(p->expr_1);
        Type *rightType = inferExprType(p->expr_2);

        if (!dynamic_cast<Int *>(leftType) || !dynamic_cast<Int *>(rightType)) {
            throw std::runtime_error("Type error: Multiplication is only allowed on integers.");
        }
    }

    void visitEAdd(EAdd *p) override {
        p->expr_1->accept(this);
        p->expr_2->accept(this);
        Type *leftType = inferExprType(p->expr_1);
        Type *rightType = inferExprType(p->expr_2);

        Plus *plusOp = dynamic_cast<Plus *>(p->addop_);
        Minus *minusOp = dynamic_cast<Minus *>(p->addop_);

        if (plusOp) {
            if (dynamic_cast<Int *>(leftType) && dynamic_cast<Int *>(rightType)) {
            } else if (dynamic_cast<Str *>(leftType) && dynamic_cast<Str *>(rightType)) {
            } else {
                throw std::runtime_error("Type error: Addition is only allowed on pairs of integers or strings.");
            }
        } else if (minusOp) {
            if (!dynamic_cast<Int *>(leftType) || !dynamic_cast<Int *>(rightType)) {
                throw std::runtime_error("Type error: Subtraction is only allowed on integers.");
            }
        } else {
            throw std::runtime_error("Unknown add operation.");
        }
    }

    void visitERel(ERel *p) override {
    Type* leftType = inferExprType(p->expr_1);
    Type* rightType = inferExprType(p->expr_2);
    RelOp* relop = p->relop_;

        if (dynamic_cast<LTH*>(relop) || dynamic_cast<LE*>(relop) || dynamic_cast<GTH*>(relop) || dynamic_cast<GE*>(relop)) {
            // Operators <, <=, >, >=
            if (!dynamic_cast<Int *>(leftType) || !dynamic_cast<Int *>(rightType)) {
                throw std::runtime_error("Type error: Relational operators <, <=, >, >= are only allowed on integers.");
            }
        } else if (dynamic_cast<EQU*>(relop) || dynamic_cast<NE*>(relop)) {
            // Operators ==, !=
            if (!(dynamic_cast<Int*>(leftType) && dynamic_cast<Int*>(rightType)) &&
                !(dynamic_cast<Bool*>(leftType) && dynamic_cast<Bool*>(rightType))) {
                throw std::runtime_error("Type error: Equality operators ==, != are only allowed on pairs of integers or booleans.");
            }
        } else {
            throw std::runtime_error("Unknown relational operation.");
        }
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
    if (dynamic_cast<ELitInt*>(expr)) {
        return new Int();
    } else if (dynamic_cast<EString*>(expr)) {
        return new Str();
    } else if (dynamic_cast<EVar*>(expr)) {
        EVar* var = dynamic_cast<EVar*>(expr);
        auto it = symbolTable.find(var->ident_);
        if (it != symbolTable.end()) {
            return it->second;
        } else {
            throw std::runtime_error("Type error: Variable '" + std::string(var->ident_) + "' not declared.");
        }
    } else if (dynamic_cast<ELitTrue*>(expr) || dynamic_cast<ELitFalse*>(expr)) {
        return new Bool();
    } else if (dynamic_cast<EApp*>(expr)) {
        EApp* app = dynamic_cast<EApp*>(expr);
        auto it = functionTable.find(app->ident_);
        if (it != functionTable.end()) {
            return it->second.returnType;
        } else {
            throw std::runtime_error("Type error: Function '" + std::string(app->ident_) + "' not declared.");
        }
    } else if (dynamic_cast<EAdd*>(expr)) {
        EAdd* addExpr = dynamic_cast<EAdd*>(expr);
        Type* leftType = inferExprType(addExpr->expr_1);
        Type* rightType = inferExprType(addExpr->expr_2);
        Plus* plusOp = dynamic_cast<Plus*>(addExpr->addop_);
        Minus* minusOp = dynamic_cast<Minus*>(addExpr->addop_);

        if (plusOp) {
            if (dynamic_cast<Int*>(leftType) && dynamic_cast<Int*>(rightType)) {
                return new Int();
            } else if (dynamic_cast<Str*>(leftType) && dynamic_cast<Str*>(rightType)) {
                return new Str();
            } else {
                throw std::runtime_error("Type error: Addition is only allowed on integers or strings.");
            }
        } else if (minusOp) {
            if (dynamic_cast<Int*>(leftType) && dynamic_cast<Int*>(rightType)) {
                return new Int();
            } else {
                throw std::runtime_error("Type error: Subtraction is only allowed on integers.");
            }
        }
    } else if (dynamic_cast<EMul*>(expr)) {
        EMul* mulExpr = dynamic_cast<EMul*>(expr);
        Type* leftType = inferExprType(mulExpr->expr_1);
        Type* rightType = inferExprType(mulExpr->expr_2);
        if (dynamic_cast<Int*>(leftType) && dynamic_cast<Int*>(rightType)) {
            return new Int();
        } else {
            throw std::runtime_error("Type error: Multiplication is only allowed on integers.");
        }
    } else if (dynamic_cast<ERel*>(expr)) {
        ERel* relExpr = dynamic_cast<ERel*>(expr);
        Type* leftType = inferExprType(relExpr->expr_1);
        Type* rightType = inferExprType(relExpr->expr_2);
        RelOp* relop = relExpr->relop_;

        if (dynamic_cast<LTH*>(relop) || dynamic_cast<LE*>(relop) || dynamic_cast<GTH*>(relop) || dynamic_cast<GE*>(relop)) {
            if (!dynamic_cast<Int*>(leftType) || !dynamic_cast<Int*>(rightType)) {
                throw std::runtime_error("Type error: Relational operators <, <=, >, >= are only allowed on integers.");
            }
            return new Bool();
        } else if (dynamic_cast<EQU*>(relop) || dynamic_cast<NE*>(relop)) {
            if ((dynamic_cast<Int*>(leftType) && dynamic_cast<Int*>(rightType)) ||
                (dynamic_cast<Bool*>(leftType) && dynamic_cast<Bool*>(rightType))) {
                return new Bool();
            } else {
                throw std::runtime_error("Type error: Equality operators ==, != are only allowed on integers or booleans.");
            }
        } else {
            throw std::runtime_error("Unknown relational operation.");
        }
    }
    // Additional type inference cases as needed
    return nullptr;
}

    // Helper function to compare types
    bool typesEqual(Type* a, Type* b) {
        return typeid(*a) == typeid(*b);
    }

    // Helper function to convert a type to a string
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

    // Parse the input
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
