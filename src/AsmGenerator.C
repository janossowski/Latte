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

    // caller-saved, safe scratch regs (not used for arg passing)
    // choose as many as you like
    freeEvalRegs.push_back("%rcx");
    freeEvalRegs.push_back("%r8");
    freeEvalRegs.push_back("%r9");
    // freeEvalRegs.push_back("%r10");
    // freeEvalRegs.push_back("%r11");
    // freeEvalRegs.push_back("%rdx"); // careful with idiv
}


void AsmGenerator::vpushRax() {
    // If no free regs, evict oldest reg-backed value into real stack
    if (freeEvalRegs.empty()) {
        // Only possible if there exists some REG slot already
        // (because otherwise we were storing everything in memory already)
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
        // no regs at all -> spill directly
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
    // Turn all REG slots into STACK slots, preserving order
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

    // push it to real stack (becomes older part of stack)
    emit("  pushq " + reg);

    // free that register
    freeEvalRegs.push_back(reg);

    // convert that slot to STACK
    vstack[idx].kind = SlotKind::STACK;
    vstack[idx].reg.clear();

    // IMPORTANT: this keeps all remaining REG slots at the end,
    // because we removed the oldest one.
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
    std::string end = newLabel(".Lend");

    p->expr_->accept(this);
    emit("  cmpq $0, %rax");
    emit("  je " + end);
    auto savedLocals = locals;
    p->stmt_->accept(this);
    locals = savedLocals;
    emit(end + ":");
}

void AsmGenerator::visitCondElse(CondElse *p) {
    std::string els = newLabel(".Lelse");
    std::string end = newLabel(".Lend");

    p->expr_->accept(this);
    emit("  cmpq $0, %rax");
    emit("  je " + els);

    auto savedLocals = locals;
    p->stmt_1->accept(this);
    locals = savedLocals;
    emit("  jmp " + end);

    emit(els + ":");
    savedLocals = locals;
    p->stmt_2->accept(this);
    locals = savedLocals;

    emit(end + ":");
}

void AsmGenerator::visitWhile(While *p) {
    std::string start = newLabel(".Lwhile");
    std::string end = newLabel(".Lend");

    emit(start + ":");
    p->expr_->accept(this);
    emit("  cmpq $0, %rax");
    emit("  je " + end);
    auto savedLocals = locals;
    p->stmt_->accept(this);
    locals = savedLocals;
    emit("  jmp " + start);
    emit(end + ":");
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
    spillVStack(); // IMPORTANT: call may clobber %r10/%r11

    std::string lbl = newLabel(".str");
    stringLiterals.push_back(lbl + ": .string \"" + escape(p->string_) + "\"");

    emit("  leaq " + lbl + "(%rip), %rdi");
    emit("  call string_from_cstr"); // returns String* in %rax
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
    vpopTo("%r10");   // r10 = left, rax = right  (SAFE: r10 not in pool)

    if (dynamic_cast<Times *>(p->mulop_)) {
        emit("  imulq %r10, %rax");
        return;
    }

    // division/mod clobbers rax/rdx, so flush any eval regs still alive
    // spillVStack();

    if (dynamic_cast<Div *>(p->mulop_)) {
        emit("  movq %rax, %r11");  // divisor = right
        emit("  movq %r10, %rax");  // dividend = left
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

    // lhs -> %rax
    p->expr_1->accept(this);
    vpushRax();

    // rhs -> %rax
    p->expr_2->accept(this);

    if (dynamic_cast<Plus*>(p->addop_)) {
        if (isInt(t)) {
            // lhs -> %r10, rhs already in %rax
            vpopTo("%r10");
            emit("  addq %r10, %rax");   // rax = rhs + lhs
        } else if (isStr(t)) {
            // string_concat(lhs, rhs)
            // lhs -> %rdi, rhs -> %rsi
            vpopTo("%rdi");
            emit("  movq %rax, %rsi");
            spillVStack();
            emit("  call string_concat");
        }
    } else {
        // minus (only ints by typechecker)
        // compute lhs - rhs
        vpopTo("%r10");                 // r10 = lhs
        emit("  subq %rax, %r10");       // r10 = lhs - rhs
        emit("  movq %r10, %rax");       // result -> rax
    }
}

void AsmGenerator::visitERel(ERel *p) {
    Type* t = p->expr_1->inferredType;

    p->expr_1->accept(this);
    vpushRax();
    p->expr_2->accept(this);

    // If strings, handle via runtime calls
    if (isStr(t)) {
        if (dynamic_cast<EQU*>(p->relop_)) {
            vpopTo("%rdi");          // lhs
            emit("  movq %rax, %rsi"); // rhs
            spillVStack();
            emit("  call string_eq");  // result 0/1 in rax
            return;
        }
        if (dynamic_cast<NE*>(p->relop_)) {
            vpopTo("%rdi");
            emit("  movq %rax, %rsi");
            spillVStack();
            emit("  call string_eq");
            emit("  xorq $1, %rax");
            return;
        }

        // (<, <=, >, >=) not allowed on strings by typechecker
        throw std::runtime_error("Internal compiler error: invalid string relational op");
    }

    // Non-string: compare lhs vs rhs
    vpopTo("%r10");         // r10 = lhs, rax = rhs
    emit("  cmpq %rax, %r10");

    std::string trueLbl = newLabel(".Ltrue");
    std::string endLbl  = newLabel(".Lend");

    if (dynamic_cast<LTH*>(p->relop_)) emit("  jl " + trueLbl);
    else if (dynamic_cast<LE*>(p->relop_)) emit("  jle " + trueLbl);
    else if (dynamic_cast<GTH*>(p->relop_)) emit("  jg " + trueLbl);
    else if (dynamic_cast<GE*>(p->relop_)) emit("  jge " + trueLbl);
    else if (dynamic_cast<EQU*>(p->relop_)) emit("  je " + trueLbl);
    else if (dynamic_cast<NE*>(p->relop_)) emit("  jne " + trueLbl);

    emit("  movq $0, %rax");
    emit("  jmp " + endLbl);

    emit(trueLbl + ":");
    emit("  movq $1, %rax");
    emit(endLbl + ":");
}

void AsmGenerator::visitEAnd(EAnd *p) {
    std::string falseLbl = newLabel(".Lfalse");
    std::string endLbl   = newLabel(".Lend");

    p->expr_1->accept(this);
    emit("  cmpq $0, %rax");
    emit("  je " + falseLbl);

    p->expr_2->accept(this);
    emit("  cmpq $0, %rax");
    emit("  je " + falseLbl);

    emit("  movq $1, %rax");
    emit("  jmp " + endLbl);

    emit(falseLbl + ":");
    emit("  movq $0, %rax");
    emit(endLbl + ":");
}

void AsmGenerator::visitEOr(EOr *p) {
    std::string trueLbl = newLabel(".Ltrue");
    std::string endLbl  = newLabel(".Lend");

    p->expr_1->accept(this);
    emit("  cmpq $0, %rax");
    emit("  jne " + trueLbl);

    p->expr_2->accept(this);
    emit("  cmpq $0, %rax");
    emit("  jne " + trueLbl);

    emit("  movq $0, %rax");
    emit("  jmp " + endLbl);

    emit(trueLbl + ":");
    emit("  movq $1, %rax");
    emit(endLbl + ":");
}

void AsmGenerator::visitEApp(EApp *p) {
    spillVStack(); // IMPORTANT: protect any pending virtual stack values

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
        std::string(currentFunction->ident_) != "main";

    if (canTailCall) {
        // evaluate args and push results
        for (int i = 0; i < argCount; ++i) {
            (*p->listexpr_)[i]->accept(this);
            emit("  pushq %rax");
        }

        // write stack args to 16(%rbp)+
        for (int i = argCount; i >= 7; --i) {
            int slotOffset = 16 + (i - 7) * 8;
            emit("  popq %rax");
            emit("  movq %rax, " + std::to_string(slotOffset) + "(%rbp)");
        }

        // padding slots
        for (int k = stackArgs + 1; k <= requiredStackArgs; ++k) {
            int slotOffset = 16 + (k - 1) * 8;
            emit("  movq $0, " + std::to_string(slotOffset) + "(%rbp)");
        }

        // pop reg args
        for (int i = regArgs; i >= 1; --i) {
            emit("  popq %rax");
            emit("  movq %rax, " + std::string(argRegs[i - 1]));
        }

        emit("  leave");
        emit("  jmp " + std::string(p->ident_));
        return;
    }

    // padding first
    for (int i = 0; i < paddingStackArgs; ++i)
        emit("  pushq $0");

    // real stack args right-to-left
    for (int i = argCount - 1; i >= 6; --i) {
        (*p->listexpr_)[i]->accept(this);
        emit("  pushq %rax");
    }

    // reg args left-to-right, pushed temporarily
    for (int i = 0; i < regArgs; ++i) {
        (*p->listexpr_)[i]->accept(this);
        emit("  pushq %rax");
    }

    // pop into regs
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
