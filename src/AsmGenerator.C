#include "AsmGenerator.H"
#include <iostream>

AsmGenerator::AsmGenerator(std::ostream &outStream)
  : out(outStream), stackOffset(0), labelCounter(0) {}

void AsmGenerator::emit(const std::string &s) {
    out << s << "\n";
}

void AsmGenerator::emitStringLiterals() {
    if (stringLiterals.empty())
        return;

    emit("");
    emit(".section .rodata");

    for (const auto& s : stringLiterals)
        emit(s);
}

std::string AsmGenerator::newLabel(const std::string &prefix) {
    return prefix + std::to_string(labelCounter++);
}

static bool isInt(Type* t)  { return dynamic_cast<Int*>(t); }
static bool isStr(Type* t)  { return dynamic_cast<Str*>(t); }
static bool isBool(Type* t) { return dynamic_cast<Bool*>(t); }

static std::string escape(const std::string &s) {
    std::string out;
    for (char c : s) {
        switch (c) {
            case '\n': out += "\\n"; break;
            case '\t': out += "\\t"; break;
            case '\r': out += "\\r"; break;
            case '\\': out += "\\\\"; break;
            case '\"': out += "\\\""; break;
            case '\0': out += "\\0"; break;
            default:
                if (isprint(static_cast<unsigned char>(c))) {
                    out += c;
                } else {
                    char buf[5];
                    snprintf(buf, sizeof(buf), "\\%03o",
                             static_cast<unsigned char>(c));
                    out += buf;
                }
        }
    }
    return out;
}

void AsmGenerator::resetVStack() {
    vstack.clear();
    freeEvalRegs.clear();

    freeEvalRegs.push_back("%rcx");
    freeEvalRegs.push_back("%r8");
    freeEvalRegs.push_back("%r9");
}


void AsmGenerator::vpushRax() {
    if (freeEvalRegs.empty()) {
        if (oldestRegSlotIndex() != -1) {
            evictOldestRegToStack();
        }
    }

    if (!freeEvalRegs.empty()) {
        std::string r = freeEvalRegs.back();
        freeEvalRegs.pop_back();

        emit("  movq %rax, " + r);
        vstack.push_back({SlotKind::REG, r});
    } else {
        emit("  pushq %rax");
        vstack.push_back({SlotKind::STACK, ""});
    }
}


void AsmGenerator::vpopTo(const std::string& dst) {
    if (vstack.empty())
        throw std::runtime_error("Internal compiler error: vpop from empty vstack");

    Slot s = vstack.back();
    vstack.pop_back();

    if (s.kind == SlotKind::REG) {
        emit("  movq " + s.reg + ", " + dst);
        freeEvalRegs.push_back(s.reg);
    } else {
        emit("  popq " + dst);
    }
}


void AsmGenerator::spillVStack() {
    for (auto &s : vstack) {
        if (s.kind == SlotKind::REG) {
            emit("  pushq " + s.reg);
            freeEvalRegs.push_back(s.reg);
            s.kind = SlotKind::STACK;
            s.reg.clear();
        }
    }
}

int AsmGenerator::oldestRegSlotIndex() const {
    for (int i = 0; i < (int)vstack.size(); ++i) {
        if (vstack[i].kind == SlotKind::REG)
            return i;
    }
    return -1;
}

void AsmGenerator::evictOldestRegToStack() {
    int idx = oldestRegSlotIndex();
    if (idx == -1)
        throw std::runtime_error("Internal compiler error: no REG slot to evict");

    std::string reg = vstack[idx].reg;

    emit("  pushq " + reg);

    freeEvalRegs.push_back(reg);

    vstack[idx].kind = SlotKind::STACK;
    vstack[idx].reg.clear();
}

void AsmGenerator::genCond(Expr* e, const std::string& Ltrue, const std::string& Lfalse) {
    if (!e) {
        emit("  jmp " + Lfalse);
        return;
    }
    if (dynamic_cast<ELitTrue*>(e)) {
        emit("  jmp " + Ltrue);
        return;
    }
    if (dynamic_cast<ELitFalse*>(e)) {
        emit("  jmp " + Lfalse);
        return;
    }
    if (auto n = dynamic_cast<Not*>(e)) {
        genCond(n->expr_, Lfalse, Ltrue);
        return;
    }
    if (auto a = dynamic_cast<EAnd*>(e)) {
        std::string Lmid = newLabel(".Land");
        genCond(a->expr_1, Lmid, Lfalse);
        emit(Lmid + ":");
        genCond(a->expr_2, Ltrue, Lfalse);
        return;
    }
    if (auto o = dynamic_cast<EOr*>(e)) {
        std::string Lmid = newLabel(".Lor");
        genCond(o->expr_1, Ltrue, Lmid);
        emit(Lmid + ":");
        genCond(o->expr_2, Ltrue, Lfalse);
        return;
    }

    if (auto r = dynamic_cast<ERel*>(e)) {
        r->expr_1->accept(this);
        vpushRax();
        r->expr_2->accept(this);
        vpopTo("%r10");

        Type* lt = r->expr_1 ? r->expr_1->inferredType : nullptr;

        if ((dynamic_cast<EQU*>(r->relop_) || dynamic_cast<NE*>(r->relop_)) && isStr(lt)) {
            spillVStack();

            emit("  movq %r10, %rdi");
            emit("  movq %rax, %rsi");
            emit("  call string_eq");

            emit("  cmpq $0, %rax");

            if (dynamic_cast<EQU*>(r->relop_)) {
                emit("  jne " + Ltrue);
                emit("  jmp " + Lfalse);
            } else {
                emit("  jne " + Lfalse);
                emit("  jmp " + Ltrue);
            }
            return;
        }

        emit("  cmpq %rax, %r10");

        if (dynamic_cast<LTH*>(r->relop_)) {
            emit("  jl " + Ltrue);
        } else if (dynamic_cast<LE*>(r->relop_)) {
            emit("  jle " + Ltrue);
        } else if (dynamic_cast<GTH*>(r->relop_)) {
            emit("  jg " + Ltrue);
        } else if (dynamic_cast<GE*>(r->relop_)) {
            emit("  jge " + Ltrue);
        } else if (dynamic_cast<EQU*>(r->relop_)) {
            emit("  je " + Ltrue);
        } else if (dynamic_cast<NE*>(r->relop_)) {
            emit("  jne " + Ltrue);
        } else {
            throw std::runtime_error("Internal compiler error: unknown RelOp in genCond");
        }

        emit("  jmp " + Lfalse);
        return;
    }

    e->accept(this);
    emit("  cmpq $0, %rax");
    emit("  jne " + Ltrue);
    emit("  jmp " + Lfalse);
}

void AsmGenerator::genBool(Expr* e) {
    std::string Lt = newLabel(".Ltrue");
    std::string Lf = newLabel(".Lfalse");
    std::string Lend = newLabel(".Lend");

    genCond(e, Lt, Lf);

    emit(Lt + ":");
    emit("  movq $1, %rax");
    emit("  jmp " + Lend);

    emit(Lf + ":");
    emit("  movq $0, %rax");

    emit(Lend + ":");
}

void AsmGenerator::visitProgram(Program *) {}
void AsmGenerator::visitTopDef(TopDef *) {}
void AsmGenerator::visitArg(Arg *) {}
void AsmGenerator::visitBlock(Block *) {}
void AsmGenerator::visitStmt(Stmt *) {}
void AsmGenerator::visitItem(Item *) {}
void AsmGenerator::visitType(Type *) {}
void AsmGenerator::visitExpr(Expr *) {}
void AsmGenerator::visitAddOp(AddOp *) {}
void AsmGenerator::visitMulOp(MulOp *) {}
void AsmGenerator::visitRelOp(RelOp *) {}

void AsmGenerator::visitProg(Prog *p) {
    emit(".text");
    if (p->listtopdef_)
        p->listtopdef_->accept(this);
}

void AsmGenerator::visitFnDef(FnDef *p) {
    locals.clear();
    stackOffset = 0;
    currentReturnLabel = newLabel(".Lreturn");
    currentFunction = p;
    resetVStack();

    emit("");
    emit(".globl " + std::string(p->ident_));
    emit(p->ident_ + ":");
    emit("  pushq %rbp");
    emit("  movq %rsp, %rbp");

    int frameSize = p->localsSize * 8;

    /* Ensure 16-byte alignment */
    if ((frameSize + 8) % 16 != 0)
        frameSize += 8;

    if (frameSize > 0)
        emit("  subq $" + std::to_string(frameSize) + ", %rsp");

    static const char* argRegs[] = {
        "%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"
    };

    int argIndex = 0;

    if (p->listarg_) {
        for (auto it = p->listarg_->begin(); it != p->listarg_->end(); ++it) {
            Arg* a = *it;
            Ar* arg = dynamic_cast<Ar*>(a);
            if (!arg)
                throw std::runtime_error("Internal compiler error: expected Ar");

            stackOffset -= 8;
            locals[arg->ident_] = stackOffset;

            if (argIndex < 6) {
                emit("  movq " + std::string(argRegs[argIndex]) +
                     ", " + std::to_string(stackOffset) + "(%rbp)");
            } else {
                int callerOffset = 16 + (argIndex - 6) * 8;
                emit("  movq " + std::to_string(callerOffset) + "(%rbp), %rax");
                emit("  movq %rax, " + std::to_string(stackOffset) + "(%rbp)");
            }

            argIndex++;
        }
    }

    if (p->block_)
        p->block_->accept(this);

    emit(currentReturnLabel + ":");
    emit("  leave");
    emit("  ret");
}

void AsmGenerator::visitAr(Ar *) {}

void AsmGenerator::visitBlk(Blk *p) {
    auto savedLocals = locals;
    if (p->liststmt_)
        p->liststmt_->accept(this);
    locals = savedLocals;
}

void AsmGenerator::visitEmpty(Empty *) {}

void AsmGenerator::visitBStmt(BStmt *p) {
    if (p->block_)
        p->block_->accept(this);
}

void AsmGenerator::visitDecl(Decl *p) {
    currentDeclType = p->type_;

    for (ListItem::iterator it = p->listitem_->begin();
         it != p->listitem_->end();
         ++it)
    {
        (*it)->accept(this);
    }

    currentDeclType = nullptr;
}


void AsmGenerator::visitNoInit(NoInit *p) {
    stackOffset -= 8;
    locals[p->ident_] = stackOffset;

    emit("  movq $0, " + std::to_string(stackOffset) + "(%rbp)");
}


void AsmGenerator::visitInit(Init *p) {
    p->expr_->accept(this);

    stackOffset -= 8;
    locals[p->ident_] = stackOffset;

    emit("  movq %rax, " + std::to_string(stackOffset) + "(%rbp)");
}


void AsmGenerator::visitAss(Ass *p) {
    p->expr_->accept(this);
    int offset = locals[p->ident_];
    emit("  movq %rax, " + std::to_string(offset) + "(%rbp)");
}

void AsmGenerator::visitIncr(Incr *p) {
    int offset = locals[p->ident_];
    emit("  addq $1, " + std::to_string(offset) + "(%rbp)");
}

void AsmGenerator::visitDecr(Decr *p) {
    int offset = locals[p->ident_];
    emit("  subq $1, " + std::to_string(offset) + "(%rbp)");
}

void AsmGenerator::visitRet(Ret *p) {
    bool oldTail = inTailCallPosition;
    inTailCallPosition = (dynamic_cast<EApp*>(p->expr_) != nullptr);

    p->expr_->accept(this);

    inTailCallPosition = oldTail;

    emit("  jmp " + currentReturnLabel);
}

void AsmGenerator::visitVRet(VRet *) {
    emit("  movq $0, %rax");
    emit("  jmp " + currentReturnLabel);
}


void AsmGenerator::visitCond(Cond *p) {
    std::string Lthen = newLabel(".Lthen");
    std::string Lend  = newLabel(".Lend");

    genCond(p->expr_, Lthen, Lend);

    emit(Lthen + ":");
    p->stmt_->accept(this);

    emit(Lend + ":");
}

void AsmGenerator::visitCondElse(CondElse *p) {
    std::string Lthen = newLabel(".Lthen");
    std::string Lelse = newLabel(".Lelse");
    std::string Lend  = newLabel(".Lend");

    genCond(p->expr_, Lthen, Lelse);

    emit(Lthen + ":");
    p->stmt_1->accept(this);
    emit("  jmp " + Lend);

    emit(Lelse + ":");
    p->stmt_2->accept(this);

    emit(Lend + ":");
}

void AsmGenerator::visitWhile(While *p) {
    std::string Lcond = newLabel(".Lcond");
    std::string Lloop = newLabel(".Lloop");
    std::string Lend  = newLabel(".Lend");

    emit("  jmp " + Lcond);

    emit(Lloop + ":");
    p->stmt_->accept(this);

    emit(Lcond + ":");
    genCond(p->expr_, Lloop, Lend);

    emit(Lend + ":");
}

void AsmGenerator::visitSExp(SExp *p) {
    p->expr_->accept(this);
}

void AsmGenerator::visitInt(Int *) {}
void AsmGenerator::visitStr(Str *) {}
void AsmGenerator::visitBool(Bool *) {}
void AsmGenerator::visitVoid(Void *) {}
void AsmGenerator::visitFun(Fun *) {}

void AsmGenerator::visitEVar(EVar *p) {
    int offset = locals[p->ident_];
    emit("  movq " + std::to_string(offset) + "(%rbp), %rax");
}

void AsmGenerator::visitELitInt(ELitInt *p) {
    emit("  movq $" + std::to_string(p->integer_) + ", %rax");
}

void AsmGenerator::visitELitTrue(ELitTrue *) {
    emit("  movq $1, %rax");
}

void AsmGenerator::visitELitFalse(ELitFalse *) {
    emit("  movq $0, %rax");
}

void AsmGenerator::visitEString(EString *p) {
    spillVStack();

    std::string lbl = newLabel(".str");
    stringLiterals.push_back(lbl + ": .string \"" + escape(p->string_) + "\"");

    emit("  leaq " + lbl + "(%rip), %rdi");
    emit("  call string_from_cstr");
}

void AsmGenerator::visitNeg(Neg *p) {
    p->expr_->accept(this);
    emit("  negq %rax");
}

void AsmGenerator::visitNot(Not *p) {
    p->expr_->accept(this);
    emit("  cmpq $0, %rax");
    emit("  sete %al");
    emit("  movzbq %al, %rax");
}

void AsmGenerator::visitEMul(EMul *p) {
    p->expr_1->accept(this);
    vpushRax();

    p->expr_2->accept(this);
    vpopTo("%r10");

    if (dynamic_cast<Times *>(p->mulop_)) {
        emit("  imulq %r10, %rax");
        return;
    }

    if (dynamic_cast<Div *>(p->mulop_)) {
        emit("  movq %rax, %r11");
        emit("  movq %r10, %rax");
        emit("  cqto");
        emit("  idivq %r11");
        return;
    }

    if (dynamic_cast<Mod *>(p->mulop_)) {
        emit("  movq %rax, %r11");
        emit("  movq %r10, %rax");
        emit("  cqto");
        emit("  idivq %r11");
        emit("  movq %rdx, %rax");
        return;
    }
}

void AsmGenerator::visitEAdd(EAdd *p) {
    Type* t = p->inferredType;

    p->expr_1->accept(this);
    vpushRax();

    p->expr_2->accept(this);

    if (dynamic_cast<Plus*>(p->addop_)) {
        if (isInt(t)) {
            vpopTo("%r10");
            emit("  addq %r10, %rax");
        } else if (isStr(t)) {
            vpopTo("%rdi");
            emit("  movq %rax, %rsi");
            spillVStack();
            emit("  call string_concat");
        }
    } else {
        vpopTo("%r10");
        emit("  subq %rax, %r10");
        emit("  movq %r10, %rax");
    }
}

void AsmGenerator::visitERel(ERel *p) {
    genBool(p);
}

void AsmGenerator::visitEAnd(EAnd *p) {
    genBool(p);
}

void AsmGenerator::visitEOr(EOr *p) {
    genBool(p);
}

void AsmGenerator::visitEApp(EApp *p) {
    spillVStack();

    static const char* argRegs[] = {
        "%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"
    };

    int argCount    = p->listexpr_ ? (int)p->listexpr_->size() : 0;
    int regArgs     = std::min(6, argCount);
    int stackArgs   = (argCount > 6) ? (argCount - 6) : 0;

    int requiredArgs = argCount;
    if (currentFunction != nullptr) {
        auto it = currentFunction->tailRequiredArgCounts.find(p->ident_);
        if (it != currentFunction->tailRequiredArgCounts.end()) {
            requiredArgs = std::max(requiredArgs, it->second);
        }
    }

    int requiredStackArgs = (requiredArgs > 6) ? (requiredArgs - 6) : 0;
    int paddingStackArgs  = std::max(0, requiredStackArgs - stackArgs);

    bool canTailCall =
        inTailCallPosition &&
        currentFunction != nullptr &&
        (std::string(currentFunction->ident_) != "main" || requiredStackArgs == 0);

    if (canTailCall) {
        for (int i = 0; i < argCount; ++i) {
            (*p->listexpr_)[i]->accept(this);
            emit("  pushq %rax");
        }

        for (int i = argCount; i >= 7; --i) {
            int slotOffset = 16 + (i - 7) * 8;
            emit("  popq %rax");
            emit("  movq %rax, " + std::to_string(slotOffset) + "(%rbp)");
        }

        for (int k = stackArgs + 1; k <= requiredStackArgs; ++k) {
            int slotOffset = 16 + (k - 1) * 8;
            emit("  movq $0, " + std::to_string(slotOffset) + "(%rbp)");
        }

        for (int i = regArgs; i >= 1; --i) {
            emit("  popq %rax");
            emit("  movq %rax, " + std::string(argRegs[i - 1]));
        }

        emit("  leave");
        emit("  jmp " + std::string(p->ident_));
        return;
    }

    for (int i = 0; i < paddingStackArgs; ++i)
        emit("  pushq $0");

    for (int i = argCount - 1; i >= 6; --i) {
        (*p->listexpr_)[i]->accept(this);
        emit("  pushq %rax");
    }

    for (int i = 0; i < regArgs; ++i) {
        (*p->listexpr_)[i]->accept(this);
        emit("  pushq %rax");
    }

    for (int i = regArgs - 1; i >= 0; --i)
        emit("  popq " + std::string(argRegs[i]));

    emit("  call " + std::string(p->ident_));

    int totalStackPushed = stackArgs + paddingStackArgs;
    if (totalStackPushed > 0)
        emit("  addq $" + std::to_string(totalStackPushed * 8) + ", %rsp");
}

void AsmGenerator::visitPlus(Plus *) {}
void AsmGenerator::visitMinus(Minus *) {}
void AsmGenerator::visitTimes(Times *) {}
void AsmGenerator::visitDiv(Div *) {}
void AsmGenerator::visitMod(Mod *) {}
void AsmGenerator::visitLTH(LTH *) {}
void AsmGenerator::visitLE(LE *) {}
void AsmGenerator::visitGTH(GTH *) {}
void AsmGenerator::visitGE(GE *) {}
void AsmGenerator::visitEQU(EQU *) {}
void AsmGenerator::visitNE(NE *) {}

void AsmGenerator::visitListTopDef(ListTopDef *p) {
    for (auto it = p->begin(); it != p->end(); ++it)
        (*it)->accept(this);
}

void AsmGenerator::visitListArg(ListArg *p) {
    for (auto it = p->begin(); it != p->end(); ++it)
        (*it)->accept(this);
}

void AsmGenerator::visitListStmt(ListStmt *p) {
    for (auto it = p->begin(); it != p->end(); ++it)
        (*it)->accept(this);
}

void AsmGenerator::visitListItem(ListItem *p) {
    for (auto it = p->begin(); it != p->end(); ++it)
        (*it)->accept(this);
}

void AsmGenerator::visitListType(ListType *p) {
    for (auto it = p->begin(); it != p->end(); ++it)
        (*it)->accept(this);
}

void AsmGenerator::visitListExpr(ListExpr *p) {
    for (auto it = p->begin(); it != p->end(); ++it)
        (*it)->accept(this);
}

void AsmGenerator::visitInteger(Integer) {}
void AsmGenerator::visitChar(Char) {}
void AsmGenerator::visitDouble(Double) {}
void AsmGenerator::visitString(String) {}
void AsmGenerator::visitIdent(Ident) {}
