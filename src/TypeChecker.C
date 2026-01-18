#include "TypeChecker.H"
#include "Parser.H"
#include "ParserError.H"
#include "Printer.H"

#include <iostream>
#include <fstream>
#include <typeinfo>
#include <stdexcept>
#include <algorithm>

TypeChecker::TypeChecker()
    : currentFunctionReturnType(nullptr),
      hasReturnStatement(false) {}

TypeChecker::~TypeChecker() {}

void TypeChecker::visitProgram(Program *p)
{
    if (auto prog = dynamic_cast<Prog *>(p))
    {
        prog->listtopdef_->accept(this);
    }
}

void TypeChecker::visitTopDef(TopDef *p)
{
    if (auto fnDef = dynamic_cast<FnDef *>(p))
    {
        fnDef->accept(this);
    }
}

void TypeChecker::visitArg(Arg *p)
{
    if (auto ar = dynamic_cast<Ar *>(p))
    {
        ar->accept(this);
    }
}

void TypeChecker::visitBlock(Block *p)
{
    if (auto blk = dynamic_cast<Blk *>(p))
    {
        blk->accept(this);
    }
}

void TypeChecker::visitStmt(Stmt *p)
{
    if (auto decl = dynamic_cast<Decl *>(p))
    {
        decl->accept(this);
    }
    else if (auto ass = dynamic_cast<Ass *>(p))
    {
        ass->accept(this);
    }
    else if (auto incr = dynamic_cast<Incr *>(p))
    {
        incr->accept(this);
    }
    else if (auto decr = dynamic_cast<Decr *>(p))
    {
        decr->accept(this);
    }
    else if (auto ret = dynamic_cast<Ret *>(p))
    {
        ret->accept(this);
    }
    else if (auto vret = dynamic_cast<VRet *>(p))
    {
        vret->accept(this);
    }
    else if (auto cond = dynamic_cast<Cond *>(p))
    {
        cond->accept(this);
    }
    else if (auto condElse = dynamic_cast<CondElse *>(p))
    {
        condElse->accept(this);
    }
    else if (auto whileStmt = dynamic_cast<While *>(p))
    {
        whileStmt->accept(this);
    }
    else if (auto sExp = dynamic_cast<SExp *>(p))
    {
        sExp->accept(this);
    }
}

void TypeChecker::visitItem(Item *p)
{
    if (auto noInit = dynamic_cast<NoInit *>(p))
    {
        noInit->accept(this);
    }
    else if (auto init = dynamic_cast<Init *>(p))
    {
        init->accept(this);
    }
}

void TypeChecker::visitType(Type *p)
{
    if (auto intType = dynamic_cast<Int *>(p))
    {
        intType->accept(this);
    }
    else if (auto strType = dynamic_cast<Str *>(p))
    {
        strType->accept(this);
    }
    else if (auto boolType = dynamic_cast<Bool *>(p))
    {
        boolType->accept(this);
    }
    else if (auto voidType = dynamic_cast<Void *>(p))
    {
        voidType->accept(this);
    }
    else if (auto funType = dynamic_cast<Fun *>(p))
    {
        funType->accept(this);
    }
}

void TypeChecker::visitExpr(Expr *p)
{
    inferExprType(p);
    if (auto eVar = dynamic_cast<EVar *>(p))
    {
        eVar->accept(this);
    }
    else if (auto eLitInt = dynamic_cast<ELitInt *>(p))
    {
        eLitInt->accept(this);
    }
    else if (auto eLitTrue = dynamic_cast<ELitTrue *>(p))
    {
        eLitTrue->accept(this);
    }
    else if (auto eLitFalse = dynamic_cast<ELitFalse *>(p))
    {
        eLitFalse->accept(this);
    }
    else if (auto eApp = dynamic_cast<EApp *>(p))
    {
        eApp->accept(this);
    }
    else if (auto eString = dynamic_cast<EString *>(p))
    {
        eString->accept(this);
    }
    else if (auto neg = dynamic_cast<Neg *>(p))
    {
        neg->accept(this);
    }
    else if (auto notExpr = dynamic_cast<Not *>(p))
    {
        notExpr->accept(this);
    }
    else if (auto eMul = dynamic_cast<EMul *>(p))
    {
        eMul->accept(this);
    }
    else if (auto eAdd = dynamic_cast<EAdd *>(p))
    {
        eAdd->accept(this);
    }
    else if (auto eRel = dynamic_cast<ERel *>(p))
    {
        eRel->accept(this);
    }
    else if (auto eAnd = dynamic_cast<EAnd *>(p))
    {
        eAnd->accept(this);
    }
    else if (auto eOr = dynamic_cast<EOr *>(p))
    {
        eOr->accept(this);
    }
}

void TypeChecker::visitAddOp(AddOp *p)
{
    if (auto plus = dynamic_cast<Plus *>(p))
    {
        plus->accept(this);
    }
    else if (auto minus = dynamic_cast<Minus *>(p))
    {
        minus->accept(this);
    }
}

void TypeChecker::visitMulOp(MulOp *p)
{
    if (auto times = dynamic_cast<Times *>(p))
    {
        times->accept(this);
    }
    else if (auto div = dynamic_cast<Div *>(p))
    {
        div->accept(this);
    }
    else if (auto mod = dynamic_cast<Mod *>(p))
    {
        mod->accept(this);
    }
}

void TypeChecker::visitRelOp(RelOp *p)
{
    if (auto lth = dynamic_cast<LTH *>(p))
    {
        lth->accept(this);
    }
    else if (auto le = dynamic_cast<LE *>(p))
    {
        le->accept(this);
    }
    else if (auto gth = dynamic_cast<GTH *>(p))
    {
        gth->accept(this);
    }
    else if (auto ge = dynamic_cast<GE *>(p))
    {
        ge->accept(this);
    }
    else if (auto equ = dynamic_cast<EQU *>(p))
    {
        equ->accept(this);
    }
    else if (auto ne = dynamic_cast<NE *>(p))
    {
        ne->accept(this);
    }
}

void TypeChecker::visitProg(Prog *p)
{
    functionTable["printString"] = {new Void(), {new Str()}};
    functionTable["printInt"] = {new Void(), {new Int()}};
    functionTable["error"] = {new Void(), {}};
    functionTable["readString"] = {new Str(), {}};
    functionTable["readInt"] = {new Int(), {}};

    if (p->listtopdef_)
    {
        // First pass: Add function signatures to the function table for all functions
        std::map<std::string, FnDef*> functionDefs;
        for (TopDef *topDef : *p->listtopdef_)
        {
            FnDef *fnDef = dynamic_cast<FnDef *>(topDef);
            if (fnDef)
            {
                functionDefs[fnDef->ident_] = fnDef;
                Type *returnType = fnDef->type_;
                std::string functionName = fnDef->ident_;

                if (functionTable.find(functionName) != functionTable.end())
                {
                    throw std::runtime_error("Naming conflict: Duplicate function with the name '" + functionName + "' at line " + std::to_string(fnDef->line_number) + ", column " + std::to_string(fnDef->char_number) + ".");
                }

                std::vector<Type *> argumentTypes;
                std::set<std::string> argumentNames;
                if (fnDef->listarg_)
                {
                    for (Arg *arg : *fnDef->listarg_)
                    {
                        if (Ar *argument = dynamic_cast<Ar *>(arg))
                        {
                            if (dynamic_cast<Void *>(argument->type_))
                            {
                                throw std::runtime_error("Type error: Void type is not allowed as an argument type in function '" + functionName + "' at line " + std::to_string(argument->line_number) + ", column " + std::to_string(argument->char_number) + ".");
                            }
                            if (argumentNames.find(argument->ident_) != argumentNames.end())
                            {
                                throw std::runtime_error("Naming conflict: Duplicate argument name '" + argument->ident_ + "' in function '" + functionName + "' at line " + std::to_string(argument->line_number) + ", column " + std::to_string(argument->char_number) + ".");
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
        for (TopDef *topDef : *p->listtopdef_)
        {
            topDef->accept(this);
        }

        // Ensure there is a valid 'main' function of type int
        auto mainIt = functionTable.find("main");
        if (mainIt == functionTable.end())
        {
            throw std::runtime_error("Program is missing a 'main' function.");
        }
        if (!dynamic_cast<Int *>(mainIt->second.returnType))
        {
            throw std::runtime_error("The 'main' function must have a return type of int.");
        }
        if (!mainIt->second.argumentTypes.empty())
        {
            throw std::runtime_error("The 'main' function must not take any arguments.");
        }


        // Propagate maximum required tail call argument counts
        int fndefsCount = 0;
        fndefsCount = p->listtopdef_->size();
        printf("Total number of function definitions: %d\n", fndefsCount);
        for (int i = 0; i < fndefsCount; i++) {
            for (TopDef *topDef : *p->listtopdef_) {
                FnDef *fnDef = dynamic_cast<FnDef *>(topDef);
                if (fnDef) {
                    for (const std::string &tailCalled : fnDef->tailCalledFunctions){
                    printf("Function '%s' contains a tail call to function '%s'.\n", fnDef->ident_.c_str(), tailCalled.c_str());
                    fnDef->tailArgCount = std::max(fnDef->tailArgCount, functionDefs[tailCalled]->tailArgCount);
                    }
                }
            }
        }

        printf("Computing tail call argument requirements:\n");

        for (TopDef *topDef : *p->listtopdef_) {
            FnDef *fnDef = dynamic_cast<FnDef *>(topDef);
            if (fnDef) {
                for (Ident called : fnDef->calledFunctions) {
                    printf("Function '%s' calls function '%s'.\n", fnDef->ident_.c_str(), called.c_str());
                    if (called != "printString" &&
                        called != "printInt" &&
                        called != "error" &&
                        called != "readString" &&
                        called != "readInt") {
                        fnDef->tailRequiredArgCounts[called] = functionDefs[called]->tailArgCount;
                    }
                }
                fnDef->tailRequiredArgCounts["printString"] = 1;
                fnDef->tailRequiredArgCounts["printInt"] = 1;
                fnDef->tailRequiredArgCounts["error"] = 0;
                fnDef->tailRequiredArgCounts["readString"] = 0;
                fnDef->tailRequiredArgCounts["readInt"] = 0;
            }
        }

        printf("Computed tail call argument requirements:\n");

        for (TopDef *topDef : *p->listtopdef_) {
            FnDef *fnDef = dynamic_cast<FnDef *>(topDef);
            if (fnDef) {
                printf("Function '%s' requires a maximum of %d arguments for tail calls.\n", fnDef->ident_.c_str(), fnDef->tailArgCount);
            }
        }
    }
}

void TypeChecker::visitFnDef(FnDef *p)
{
    Type *returnType = p->type_;
    std::string functionName = p->ident_;

    currentFunctionReturnType = returnType;
    currentFunctionLocalsSize = 0;
    currentFunctionCalls.clear();
    currentFunctionTailCalls.clear();
    hasReturnStatement = false;

    std::vector<Type *> argumentTypes;
    std::set<std::string> argumentNames;
    std::map<std::string, Type *> savedSymbolTable = symbolTable;
    if (p->listarg_)
    {
        for (Arg *arg : *p->listarg_)
        {
            if (Ar *argument = dynamic_cast<Ar *>(arg))
            {
                if (argumentNames.find(argument->ident_) != argumentNames.end())
                {
                    throw std::runtime_error("Naming conflict: Duplicate argument name '" + argument->ident_ + "' in function '" + functionName + "' at line " + std::to_string(argument->line_number) + ", column " + std::to_string(argument->char_number) + ".");
                }
                argumentNames.insert(argument->ident_);
                argumentTypes.push_back(argument->type_);
                symbolTable[argument->ident_] = argument->type_;
                currentFunctionLocalsSize += 1;
            }
        }
    }

    if (p->block_)
    {
        p->block_->accept(this);
    }

    if (!dynamic_cast<Void *>(returnType) && !hasReturnStatement)
    {
        throw std::runtime_error("Type error: Function '" + functionName + "' must have a return statement at line " + std::to_string(p->line_number) + ", column " + std::to_string(p->char_number) + ".");
    }

    symbolTable = savedSymbolTable;
    p->tailCalledFunctions = currentFunctionTailCalls;
    p->calledFunctions = currentFunctionCalls;

    for (const std::string &tailCall : currentFunctionTailCalls)
    {
        printf("Function '%s' contains a tail call to function '%s'.\n", functionName.c_str(), tailCall.c_str());
    }
    p->localsSize = currentFunctionLocalsSize;
    p->tailArgCount = static_cast<int>(argumentTypes.size());
    currentFunctionReturnType = nullptr;
}

void TypeChecker::visitAr(Ar *p)
{
    if (p->type_)
        p->type_->accept(this);
    visitIdent(p->ident_);
}

void TypeChecker::visitBlk(Blk *p)
{
    std::map<std::string, Type *> savedSymbolTable = symbolTable;
    std::set<std::string> savedBlockDeclarations = currentBlockDeclarations;
    currentBlockDeclarations.clear();

    bool savedHasReturnStatement = hasReturnStatement;

    if (p->liststmt_)
        p->liststmt_->accept(this);

    symbolTable = savedSymbolTable;
    currentBlockDeclarations = savedBlockDeclarations;
    hasReturnStatement = hasReturnStatement || savedHasReturnStatement;
}

void TypeChecker::visitEmpty(Empty *p) {}

void TypeChecker::visitBStmt(BStmt *p)
{
    if (p->block_)
        p->block_->accept(this);
}

void TypeChecker::visitDecl(Decl *p)
{
    if (!p->listitem_)
        return;

    for (ListItem::iterator i = p->listitem_->begin(); i != p->listitem_->end(); ++i)
    {
        (*i)->accept(this);
        NoInit *noInit = dynamic_cast<NoInit *>(*i);
        Init *init = dynamic_cast<Init *>(*i);
        if (noInit)
        {
            std::string varName = noInit->ident_;
            if (currentBlockDeclarations.find(varName) != currentBlockDeclarations.end())
            {
                throw std::runtime_error("Naming conflict at line " + std::to_string(noInit->line_number) + ", column " + std::to_string(noInit->char_number) + ": Variable '" + varName + "' is already declared in this block.");
            }
            currentBlockDeclarations.insert(varName);
            if (dynamic_cast<Void *>(p->type_))
            {
                throw std::runtime_error("Type error at line " + std::to_string(noInit->line_number) + ", column " + std::to_string(noInit->char_number) + ": Variable '" + varName + "' cannot be declared with type void.");
            }
            symbolTable[varName] = p->type_;
            currentFunctionLocalsSize += 1;
        }
        else if (init)
        {
            std::string varName = init->ident_;
            if (currentBlockDeclarations.find(varName) != currentBlockDeclarations.end())
            {
                throw std::runtime_error("Naming conflict at line " + std::to_string(init->line_number) + ", column " + std::to_string(init->char_number) + ": Variable '" + varName + "' is already declared in this block.");
            }
            currentBlockDeclarations.insert(varName);
            symbolTable[varName] = p->type_;
            currentFunctionLocalsSize += 1;

            Type *varType = p->type_;
            if (dynamic_cast<Void *>(p->type_))
            {
                throw std::runtime_error("Type error at line " + std::to_string(noInit->line_number) + ", column " + std::to_string(noInit->char_number) + ": Variable '" + varName + "' cannot be declared with type void.");
            }
            Type *exprType = inferExprType(init->expr_);
            if (!typesEqual(varType, exprType))
            {
                throw std::runtime_error("Type error at line " + std::to_string(init->line_number) + ", column " + std::to_string(init->char_number) + ": Type mismatch in initialization of variable '" + varName + "'. Expected type '" + typeToString(varType) + "', but got type '" + typeToString(exprType) + "'.");
            }
        }
    }
}

void TypeChecker::visitNoInit(NoInit *p)
{
    visitIdent(p->ident_);
}

void TypeChecker::visitInit(Init *p)
{
    visitIdent(p->ident_);
    if (p->expr_)
        p->expr_->accept(this);
}

void TypeChecker::visitAss(Ass *p)
{
    if (symbolTable.find(p->ident_) == symbolTable.end())
    {
        throw std::runtime_error("Type error at line " + std::to_string(p->line_number) + ", column " + std::to_string(p->char_number) + ": Variable '" + p->ident_ + "' is not declared.");
    }

    Type *varType = symbolTable[p->ident_];
    Type *exprType = inferExprType(p->expr_);

    if (!typesEqual(varType, exprType))
    {
        throw std::runtime_error("Type error at line " + std::to_string(p->line_number) + ", column " + std::to_string(p->char_number) + ": Type mismatch in assignment to variable '" + p->ident_ +
                                 "'. Expected type '" + typeToString(varType) +
                                 "', but got type '" + typeToString(exprType) + "'.");
    }
}

void TypeChecker::visitIncr(Incr *p)
{
    auto it = symbolTable.find(p->ident_);
    if (it == symbolTable.end())
    {
        throw std::runtime_error("Type error at line " + std::to_string(p->line_number) + ", column " + std::to_string(p->char_number) + ": Variable '" + p->ident_ + "' is not declared.");
    }

    if (!dynamic_cast<Int *>(it->second))
    {
        throw std::runtime_error("Type error at line " + std::to_string(p->line_number) + ", column " + std::to_string(p->char_number) + ": Increment can only be performed on variables of type int.");
    }
}

void TypeChecker::visitDecr(Decr *p)
{
    auto it = symbolTable.find(p->ident_);
    if (it == symbolTable.end())
    {
        throw std::runtime_error("Type error at line " + std::to_string(p->line_number) + ", column " + std::to_string(p->char_number) + ": Variable '" + p->ident_ + "' is not declared.");
    }

    if (!dynamic_cast<Int *>(it->second))
    {
        throw std::runtime_error("Type error at line " + std::to_string(p->line_number) + ", column " + std::to_string(p->char_number) + ": Decrement can only be performed on variables of type int.");
    }
}

void TypeChecker::visitRet(Ret *p)
{
    Type *exprType = inferExprType(p->expr_);
    if (!typesEqual(currentFunctionReturnType, exprType))
    {
        throw std::runtime_error("Type error at line " + std::to_string(p->line_number) + ", column " + std::to_string(p->char_number) + ": Return type does not match function return type.");
    }
    p->expr_->accept(this);
    if(dynamic_cast<EApp *>(p->expr_))
    {
        EApp *eApp = dynamic_cast<EApp *>(p->expr_);
        currentFunctionTailCalls.insert(eApp->ident_);
    }
    hasReturnStatement = true;
}

void TypeChecker::visitVRet(VRet *p)
{
    if (!dynamic_cast<Void *>(currentFunctionReturnType))
    {
        throw std::runtime_error("Type error at line " + std::to_string(p->line_number) + ", column " + std::to_string(p->char_number) + ": Void return statement in a non-void function.");
    }
    hasReturnStatement = true;
}

void TypeChecker::visitCond(Cond *p)
{
    Type *condType = inferExprType(p->expr_);
    if (!dynamic_cast<Bool *>(condType))
    {
        throw std::runtime_error("Type error at line " + std::to_string(p->line_number) + ", column " + std::to_string(p->char_number) + ": Condition in 'if' statement must be a boolean.");
    }
    std::map<std::string, Type *> savedSymbolTable = symbolTable;
    std::set<std::string> savedBlockDeclarations = currentBlockDeclarations;
    currentBlockDeclarations.clear();

    bool initialHasReturnStatement = hasReturnStatement;
    if (isAlwaysTrue(p->expr_))
    {
        p->stmt_->accept(this);
        hasReturnStatement = initialHasReturnStatement || hasReturnStatement;
    }
    else if (!isAlwaysFalse(p->expr_))
    {
        p->stmt_->accept(this);
        hasReturnStatement = initialHasReturnStatement;
    }
    symbolTable = savedSymbolTable;
    currentBlockDeclarations = savedBlockDeclarations;
}

void TypeChecker::visitCondElse(CondElse *p)
{
    Type *condType = inferExprType(p->expr_);
    if (!dynamic_cast<Bool *>(condType))
    {
        throw std::runtime_error("Type error at line " + std::to_string(p->line_number) + ", column " + std::to_string(p->char_number) + ": Condition in 'if-else' statement must be a boolean.");
    }
    bool thenHasReturn = false;
    bool elseHasReturn = false;

    // If the condition is always true or always false only the respective branch needs to return
    if (isAlwaysTrue(p->expr_))
    {
        std::map<std::string, Type *> savedSymbolTable = symbolTable;
        std::set<std::string> savedBlockDeclarations = currentBlockDeclarations;
        currentBlockDeclarations.clear();

        symbolTable = savedSymbolTable;
        currentBlockDeclarations = savedBlockDeclarations;

        p->stmt_1->accept(this);
    }
    else if (isAlwaysFalse(p->expr_))
    {
        std::map<std::string, Type *> savedSymbolTable = symbolTable;
        std::set<std::string> savedBlockDeclarations = currentBlockDeclarations;
        currentBlockDeclarations.clear();
        p->stmt_2->accept(this);
        symbolTable = savedSymbolTable;
        currentBlockDeclarations = savedBlockDeclarations;
    }
    else
    {
        std::map<std::string, Type *> savedSymbolTable = symbolTable;
        std::set<std::string> savedBlockDeclarations = currentBlockDeclarations;
        currentBlockDeclarations.clear();
        p->stmt_1->accept(this);
        symbolTable = savedSymbolTable;
        currentBlockDeclarations = savedBlockDeclarations;
        thenHasReturn = hasReturnStatement;
        hasReturnStatement = false;

        savedSymbolTable = symbolTable;
        savedBlockDeclarations = currentBlockDeclarations;
        currentBlockDeclarations.clear();
        p->stmt_2->accept(this);
        symbolTable = savedSymbolTable;
        currentBlockDeclarations = savedBlockDeclarations;
        elseHasReturn = hasReturnStatement;
        hasReturnStatement = thenHasReturn && elseHasReturn;
    }
}

void TypeChecker::visitWhile(While *p)
{
    Type *condType = inferExprType(p->expr_);
    if (!dynamic_cast<Bool *>(condType))
    {
        throw std::runtime_error("Type error at line " + std::to_string(p->line_number) + ", column " + std::to_string(p->char_number) + ": Condition in 'while' statement must be a boolean.");
    }
    bool initialHasReturnStatement = hasReturnStatement;

    std::map<std::string, Type *> savedSymbolTable = symbolTable;
    std::set<std::string> savedBlockDeclarations = currentBlockDeclarations;
    currentBlockDeclarations.clear();

    if (isAlwaysTrue(p->expr_))
    {
        p->stmt_->accept(this);
        hasReturnStatement = initialHasReturnStatement || hasReturnStatement;
    }
    else if (!isAlwaysFalse(p->expr_))
    {
        p->stmt_->accept(this);
        hasReturnStatement = initialHasReturnStatement;
    }

    symbolTable = savedSymbolTable;
    currentBlockDeclarations = savedBlockDeclarations;
}

bool TypeChecker::isAlwaysTrue(Expr *expr)
{
    if (ELitTrue *litTrue = dynamic_cast<ELitTrue *>(expr))
    {
        return true;
    }
    return false;
}

bool TypeChecker::isAlwaysFalse(Expr *expr)
{
    if (ELitFalse *litFalse = dynamic_cast<ELitFalse *>(expr))
    {
        return true;
    }
    return false;
}

void TypeChecker::visitSExp(SExp *p)
{
    if (p->expr_)
        p->expr_->accept(this);
}

void TypeChecker::visitInt(Int *p) {}
void TypeChecker::visitStr(Str *p) {}
void TypeChecker::visitBool(Bool *p) {}
void TypeChecker::visitVoid(Void *p) {}
void TypeChecker::visitFun(Fun *p)
{
    if (p->type_)
        p->type_->accept(this);
    if (p->listtype_)
        p->listtype_->accept(this);
}

void TypeChecker::visitEVar(EVar *p)
{
    if (symbolTable.find(p->ident_) == symbolTable.end())
    {
        throw std::runtime_error("Type error: Variable '" + std::string(p->ident_) + "' not declared at line " + std::to_string(p->line_number) + ", column " + std::to_string(p->char_number) + ".");
    }
    visitIdent(p->ident_);
}

void TypeChecker::visitELitInt(ELitInt *p) {}
void TypeChecker::visitELitTrue(ELitTrue *p) {}
void TypeChecker::visitELitFalse(ELitFalse *p) {}
void TypeChecker::visitEApp(EApp *p)
{
    currentFunctionCalls.insert(p->ident_);
    auto it = functionTable.find(p->ident_);
    if (it == functionTable.end())
    {
        throw std::runtime_error("Type error: Function '" + std::string(p->ident_) + "' not declared at line " + std::to_string(p->line_number) + ", column " + std::to_string(p->char_number) + ".");
    }

    FunctionSignature signature = it->second;
    std::vector<Type *> argumentTypes = signature.argumentTypes;

    size_t provided = p->listexpr_ ? p->listexpr_->size() : 0;
    if (provided != argumentTypes.size())
    {
        throw std::runtime_error("Type error: Incorrect number of arguments for function '" + std::string(p->ident_) + "' at line " + std::to_string(p->line_number) + ", column " + std::to_string(p->char_number) + ".");
    }

    int index = 0;
    if (p->listexpr_)
    {
        for (Expr *expr : *p->listexpr_)
        {
            expr->accept(this);
            Type *exprType = inferExprType(expr);
            if (!typesEqual(exprType, argumentTypes[index]))
            {
            throw std::runtime_error("Type error: Argument " + std::to_string(index + 1) + " of function '" + std::string(p->ident_) + "' is not of correct type at line " + std::to_string(expr->line_number) + ", column " + std::to_string(expr->char_number) + ".");
            }
            ++index;
        }
    }
}

void TypeChecker::visitEString(EString *p) {}
void TypeChecker::visitNeg(Neg *p)
{
    if (p->expr_)
    {
        Type *type = inferExprType(p->expr_);
        if (!dynamic_cast<Int *>(type))
            throw std::runtime_error("Type error at line " + std::to_string(p->line_number) + ", column " + std::to_string(p->char_number) + ": Negation is only allowed on integers.");
        p->expr_->accept(this);
    }
}

void TypeChecker::visitNot(Not *p)
{
    if (p->expr_)
    {
        Type *type = inferExprType(p->expr_);
        if (!dynamic_cast<Bool *>(type))
            throw std::runtime_error("Type error at line " + std::to_string(p->line_number) + ", column " + std::to_string(p->char_number) + ": Logical Not is only allowed on booleans.");
        p->expr_->accept(this);
    }
}

void TypeChecker::visitEMul(EMul *p)
{
    p->expr_1->accept(this);
    p->expr_2->accept(this);
    Type *leftType = inferExprType(p->expr_1);
    Type *rightType = inferExprType(p->expr_2);

    if (!dynamic_cast<Int *>(leftType) || !dynamic_cast<Int *>(rightType))
    {
        throw std::runtime_error("Type error at line " + std::to_string(p->line_number) + ", column " + std::to_string(p->char_number) + ": Multiplication is only allowed on integers.");
    }
}

void TypeChecker::visitEAdd(EAdd *p)
{
    p->expr_1->accept(this);
    p->expr_2->accept(this);
    Type *leftType = inferExprType(p->expr_1);
    Type *rightType = inferExprType(p->expr_2);

    Plus *plusOp = dynamic_cast<Plus *>(p->addop_);
    Minus *minusOp = dynamic_cast<Minus *>(p->addop_);

    if (plusOp)
    {
        if (!((dynamic_cast<Int *>(leftType) && dynamic_cast<Int *>(rightType)) ||
              (dynamic_cast<Str *>(leftType) && dynamic_cast<Str *>(rightType))))
        {
            throw std::runtime_error("Type error at line " + std::to_string(p->line_number) + ", column " + std::to_string(p->char_number) + ": Addition is only allowed on pairs of integers or strings.");
        }
    }
    else if (minusOp)
    {
        if (!dynamic_cast<Int *>(leftType) || !dynamic_cast<Int *>(rightType))
        {
            throw std::runtime_error("Type error at line " + std::to_string(p->line_number) + ", column " + std::to_string(p->char_number) + ": Subtraction is only allowed on integers.");
        }
    }
    else
    {
        throw std::runtime_error("Type error at line " + std::to_string(p->line_number) + ", column " + std::to_string(p->char_number) + ": Unknown add operation.");
    }
}

void TypeChecker::visitERel(ERel *p)
{
    p->expr_1->accept(this);
    p->expr_2->accept(this);
    Type *leftType = inferExprType(p->expr_1);
    Type *rightType = inferExprType(p->expr_2);
    RelOp *relop = p->relop_;

    std::string loc = " at line " + std::to_string(p->line_number) + ", column " + std::to_string(p->char_number);

    if (dynamic_cast<LTH *>(relop) || dynamic_cast<LE *>(relop) || dynamic_cast<GTH *>(relop) || dynamic_cast<GE *>(relop))
    {
        if (!dynamic_cast<Int *>(leftType) || !dynamic_cast<Int *>(rightType))
        {
            throw std::runtime_error("Type error" + loc + ": Relational operators <, <=, >, >= are only allowed on integers.");
        }
    }
    else if (dynamic_cast<EQU *>(relop) || dynamic_cast<NE *>(relop))
    {
        if (!(dynamic_cast<Int *>(leftType) && dynamic_cast<Int *>(rightType)) &&
            !(dynamic_cast<Bool *>(leftType) && dynamic_cast<Bool *>(rightType)) &&
            !(dynamic_cast<Str *>(leftType) && dynamic_cast<Str *>(rightType)))
        {
            throw std::runtime_error("Type error" + loc + ": Equality operators ==, != are only allowed on pairs of integers, booleans, or strings.");
        }
    }
    else
    {
        throw std::runtime_error("Type error" + loc + ": Unknown relational operation.");
    }
}

void TypeChecker::visitEAnd(EAnd *p)
{
    if (p->expr_1)
        p->expr_1->accept(this);
    if (p->expr_2)
        p->expr_2->accept(this);
}

void TypeChecker::visitEOr(EOr *p)
{
    if (p->expr_1)
        p->expr_1->accept(this);
    if (p->expr_2)
        p->expr_2->accept(this);
}

void TypeChecker::visitPlus(Plus *p) {}
void TypeChecker::visitMinus(Minus *p) {}
void TypeChecker::visitTimes(Times *p) {}
void TypeChecker::visitDiv(Div *p) {}
void TypeChecker::visitMod(Mod *p) {}
void TypeChecker::visitLTH(LTH *p) {}
void TypeChecker::visitLE(LE *p) {}
void TypeChecker::visitGTH(GTH *p) {}
void TypeChecker::visitGE(GE *p) {}
void TypeChecker::visitEQU(EQU *p) {}
void TypeChecker::visitNE(NE *p) {}

void TypeChecker::visitListTopDef(ListTopDef *list_top_def)
{
    for (ListTopDef::iterator i = list_top_def->begin(); i != list_top_def->end(); ++i)
    {
        (*i)->accept(this);
    }
}

void TypeChecker::visitListArg(ListArg *list_arg)
{
    for (ListArg::iterator i = list_arg->begin(); i != list_arg->end(); ++i)
    {
        (*i)->accept(this);
    }
}

void TypeChecker::visitListStmt(ListStmt *list_stmt)
{
    for (ListStmt::iterator i = list_stmt->begin(); i != list_stmt->end(); ++i)
    {
        (*i)->accept(this);
    }
}

void TypeChecker::visitListItem(ListItem *list_item)
{
    for (ListItem::iterator i = list_item->begin(); i != list_item->end(); ++i)
    {
        (*i)->accept(this);
    }
}

void TypeChecker::visitListType(ListType *list_type)
{
    for (ListType::iterator i = list_type->begin(); i != list_type->end(); ++i)
    {
        (*i)->accept(this);
    }
}

void TypeChecker::visitListExpr(ListExpr *list_expr)
{
    for (ListExpr::iterator i = list_expr->begin(); i != list_expr->end(); ++i)
    {
        (*i)->accept(this);
    }
}

void TypeChecker::visitInteger(Integer x) {}
void TypeChecker::visitChar(Char x) {}
void TypeChecker::visitDouble(Double x) {}
void TypeChecker::visitString(String x) {}
void TypeChecker::visitIdent(Ident x) {}
Type *TypeChecker::inferExprType(Expr *expr)
{
    if (expr->inferredType)
        return expr->inferredType;
    if (dynamic_cast<ELitInt *>(expr))
    {
        expr->inferredType = new Int();
        return expr->inferredType;
    }
    else if (dynamic_cast<EString *>(expr))
    {
        expr->inferredType = new Str();
        return expr->inferredType;
    }
    else if (EVar *var = dynamic_cast<EVar *>(expr))
    {
        auto it = symbolTable.find(var->ident_);
        if (it != symbolTable.end())
        {
            expr->inferredType = it->second;
            return expr->inferredType;
        }
        else
        {
            throw std::runtime_error("Type error: Variable '" + std::string(var->ident_) + "' not declared at line " + std::to_string(expr->line_number) + ", column " + std::to_string(expr->char_number) + ".");
        }
    }
    else if (dynamic_cast<ELitTrue *>(expr) || dynamic_cast<ELitFalse *>(expr))
    {
        expr->inferredType = new Bool();
        return expr->inferredType;
    }
    else if (dynamic_cast<EApp *>(expr))
    {
        EApp *app = dynamic_cast<EApp *>(expr);
        auto it = functionTable.find(app->ident_);
        if (it != functionTable.end())
        {
            expr->inferredType = it->second.returnType;
            return expr->inferredType;
        }
        else
        {
            throw std::runtime_error("Type error: Function '" + std::string(app->ident_) + "' not declared at line " + std::to_string(app->line_number) + ", column " + std::to_string(app->char_number) + ".");
        }
    }
    else if (dynamic_cast<Neg *>(expr))
    {
        Neg *negExpr = dynamic_cast<Neg *>(expr);
        Type *innerType = inferExprType(negExpr->expr_);
        if (dynamic_cast<Int *>(innerType))
        {
            expr->inferredType = new Int();
            return expr->inferredType;
        }
        else
        {
            throw std::runtime_error("Type error at line " + std::to_string(expr->line_number) + ", column " + std::to_string(expr->char_number) + ": Unary negation can only be applied to integers.");
        }
    }
    else if (dynamic_cast<Not *>(expr))
    {
        Not *notExpr = dynamic_cast<Not *>(expr);
        Type *innerType = inferExprType(notExpr->expr_);
        if (dynamic_cast<Bool *>(innerType))
        {
            expr->inferredType = new Bool();
            return expr->inferredType;
        }
        else
        {
            throw std::runtime_error("Type error at line " + std::to_string(expr->line_number) + ", column " + std::to_string(expr->char_number) + ": Logical 'not' requires a boolean operand.");
        }
    }
    else if (dynamic_cast<EAdd *>(expr))
    {
        EAdd *addExpr = dynamic_cast<EAdd *>(expr);
        Type *leftType = inferExprType(addExpr->expr_1);
        Type *rightType = inferExprType(addExpr->expr_2);
        Plus *plusOp = dynamic_cast<Plus *>(addExpr->addop_);
        Minus *minusOp = dynamic_cast<Minus *>(addExpr->addop_);

        if (plusOp)
        {
            if (dynamic_cast<Int *>(leftType) && dynamic_cast<Int *>(rightType))
            {
                expr->inferredType = new Int();
                return expr->inferredType;
            }
            else if (dynamic_cast<Str *>(leftType) && dynamic_cast<Str *>(rightType))
            {
                expr->inferredType = new Str();
                return expr->inferredType;
            }
            else
            {
                throw std::runtime_error("Type error at line " + std::to_string(expr->line_number) + ", column " + std::to_string(expr->char_number) + ": Addition is only allowed on integers or strings.");
            }
        }
        else if (minusOp)
        {
            if (dynamic_cast<Int *>(leftType) && dynamic_cast<Int *>(rightType))
            {
                expr->inferredType = new Int();
                return expr->inferredType;
            }
            else
            {
                throw std::runtime_error("Type error at line " + std::to_string(expr->line_number) + ", column " + std::to_string(expr->char_number) + ": Subtraction is only allowed on integers.");
            }
        }
    }
    else if (dynamic_cast<EMul *>(expr))
    {
        EMul *mulExpr = dynamic_cast<EMul *>(expr);
        Type *leftType = inferExprType(mulExpr->expr_1);
        Type *rightType = inferExprType(mulExpr->expr_2);
        if (dynamic_cast<Int *>(leftType) && dynamic_cast<Int *>(rightType))
        {
            expr->inferredType = new Int();
            return expr->inferredType;
        }
        else
        {
            throw std::runtime_error("Type error at line " + std::to_string(expr->line_number) + ", column " + std::to_string(expr->char_number) + ": Multiplication is only allowed on integers.");
        }
    }
    else if (dynamic_cast<ERel *>(expr))
    {
        ERel *relExpr = dynamic_cast<ERel *>(expr);
        Type *leftType = inferExprType(relExpr->expr_1);
        Type *rightType = inferExprType(relExpr->expr_2);
        RelOp *relop = relExpr->relop_;

        if (dynamic_cast<LTH *>(relop) || dynamic_cast<LE *>(relop) || dynamic_cast<GTH *>(relop) || dynamic_cast<GE *>(relop))
        {
            if (!dynamic_cast<Int *>(leftType) || !dynamic_cast<Int *>(rightType))
            {
                throw std::runtime_error("Type error at line " + std::to_string(expr->line_number) + ", column " + std::to_string(expr->char_number) + ": Relational operators <, <=, >, >= are only allowed on integers.");
            }
            expr->inferredType = new Bool();
            return expr->inferredType;
        }
        else if (dynamic_cast<EQU *>(relop) || dynamic_cast<NE *>(relop))
        {
            if ((dynamic_cast<Int *>(leftType) && dynamic_cast<Int *>(rightType)) ||
                (dynamic_cast<Bool *>(leftType) && dynamic_cast<Bool *>(rightType)) ||
                (dynamic_cast<Str *>(leftType) && dynamic_cast<Str *>(rightType)))
            {
                expr->inferredType = new Bool();
                return expr->inferredType;
            }
            else
            {
                throw std::runtime_error("Type error at line " + std::to_string(expr->line_number) + ", column " + std::to_string(expr->char_number) + ": Equality operators ==, != are only allowed on pairs of integers, booleans or strings.");
            }
        }
        else
        {
            throw std::runtime_error("Type error at line " + std::to_string(expr->line_number) + ", column " + std::to_string(expr->char_number) + ": Unknown relational operation.");
        }
    }
    else if (dynamic_cast<EAnd *>(expr))
    {
        EAnd *andExpr = dynamic_cast<EAnd *>(expr);
        Type *leftType = inferExprType(andExpr->expr_1);
        Type *rightType = inferExprType(andExpr->expr_2);
        if (dynamic_cast<Bool *>(leftType) && dynamic_cast<Bool *>(rightType))
        {
            expr->inferredType = new Bool();
            return expr->inferredType;
        }
        else
        {
            throw std::runtime_error("Type error at line " + std::to_string(expr->line_number) + ", column " + std::to_string(expr->char_number) + ": Logical 'and' requires boolean operands.");
        }
    }
    else if (dynamic_cast<EOr *>(expr))
    {
        EOr *orExpr = dynamic_cast<EOr *>(expr);
        Type *leftType = inferExprType(orExpr->expr_1);
        Type *rightType = inferExprType(orExpr->expr_2);
        if (dynamic_cast<Bool *>(leftType) && dynamic_cast<Bool *>(rightType))
        {
            expr->inferredType = new Bool();
            return expr->inferredType;
        }
        else
        {
            throw std::runtime_error("Type error at line " + std::to_string(expr->line_number) + ", column " + std::to_string(expr->char_number) + ": Logical 'or' requires boolean operands.");
        }
    }
    throw std::runtime_error("Type error: Unable to infer type for the given expression.");
}

bool TypeChecker::typesEqual(Type *a, Type *b)
{
    return typeid(*a) == typeid(*b);
}

std::string TypeChecker::typeToString(Type *type)
{
    if (dynamic_cast<Int *>(type))
    {
        return "int";
    }
    else if (dynamic_cast<Bool *>(type))
    {
        return "boolean";
    }
    else if (dynamic_cast<Str *>(type))
    {
        return "string";
    }
    else if (dynamic_cast<Void *>(type))
    {
        return "void";
    }
    return "unknown";
}