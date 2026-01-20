#ifndef ASMGENERATOR_HEADER
#define ASMGENERATOR_HEADER

#include "Absyn.H"
#include <ostream>
#include <map>
#include <string>
#include "TypeChecker.H"

class AsmGenerator : public Visitor {
private:
    std::ostream &out;

        /* =========================
       Hybrid evaluation stack
       ========================= */

    enum class SlotKind { REG, STACK };

    struct Slot {
        SlotKind kind;
        std::string reg; // only valid if kind == REG
    };

    std::vector<std::string> freeEvalRegs;
    std::vector<Slot> vstack;

    // push current %rax onto hybrid stack (reg if possible else real stack)
    void vpushRax();

    // pop the top hybrid slot into the given register
    void vpopTo(const std::string& dst);

    // spill all register-backed virtual stack slots to real stack
    // (required before calls)
    void spillVStack();

    int oldestRegSlotIndex() const;

    void evictOldestRegToStack();

    // reset hybrid stack for a new function
    void resetVStack();

    std::map<std::string, int> locals;
    int stackOffset = 0;
    int labelCounter = 0;
    std::string currentReturnLabel;
    std::vector<std::string> stringLiterals;
    Type* currentDeclType = nullptr;

    // for tail call optimization only
    FnDef* currentFunction = nullptr;
    bool inTailCallPosition = false;

    // jumping code
    void genCond(Expr* e, const std::string& Ltrue, const std::string& Lfalse);
    void genBool(Expr* e);

    void emit(const std::string &s);
    std::string newLabel(const std::string &prefix);

public:
    void emitStringLiterals();

    explicit AsmGenerator(std::ostream &outStream);
    virtual ~AsmGenerator() {}

    void visitProgram(Program *p) override;
    void visitTopDef(TopDef *p) override;
    void visitArg(Arg *p) override;
    void visitBlock(Block *p) override;
    void visitStmt(Stmt *p) override;
    void visitItem(Item *p) override;
    void visitType(Type *p) override;
    void visitExpr(Expr *p) override;
    void visitAddOp(AddOp *p) override;
    void visitMulOp(MulOp *p) override;
    void visitRelOp(RelOp *p) override;

    void visitProg(Prog *p) override;
    void visitFnDef(FnDef *p) override;
    void visitAr(Ar *p) override;
    void visitBlk(Blk *p) override;

    void visitEmpty(Empty *p) override;
    void visitBStmt(BStmt *p) override;
    void visitDecl(Decl *p) override;
    void visitNoInit(NoInit *p) override;
    void visitInit(Init *p) override;
    void visitAss(Ass *p) override;
    void visitIncr(Incr *p) override;
    void visitDecr(Decr *p) override;
    void visitRet(Ret *p) override;
    void visitVRet(VRet *p) override;
    void visitCond(Cond *p) override;
    void visitCondElse(CondElse *p) override;
    void visitWhile(While *p) override;
    void visitSExp(SExp *p) override;

    void visitInt(Int *p) override;
    void visitStr(Str *p) override;
    void visitBool(Bool *p) override;
    void visitVoid(Void *p) override;
    void visitFun(Fun *p) override;

    void visitEVar(EVar *p) override;
    void visitELitInt(ELitInt *p) override;
    void visitELitTrue(ELitTrue *p) override;
    void visitELitFalse(ELitFalse *p) override;
    void visitEApp(EApp *p) override;
    void visitEString(EString *p) override;
    void visitNeg(Neg *p) override;
    void visitNot(Not *p) override;
    void visitEMul(EMul *p) override;
    void visitEAdd(EAdd *p) override;
    void visitERel(ERel *p) override;
    void visitEAnd(EAnd *p) override;
    void visitEOr(EOr *p) override;

    void visitPlus(Plus *p) override;
    void visitMinus(Minus *p) override;
    void visitTimes(Times *p) override;
    void visitDiv(Div *p) override;
    void visitMod(Mod *p) override;
    void visitLTH(LTH *p) override;
    void visitLE(LE *p) override;
    void visitGTH(GTH *p) override;
    void visitGE(GE *p) override;
    void visitEQU(EQU *p) override;
    void visitNE(NE *p) override;

    void visitListTopDef(ListTopDef *p) override;
    void visitListArg(ListArg *p) override;
    void visitListStmt(ListStmt *p) override;
    void visitListItem(ListItem *p) override;
    void visitListType(ListType *p) override;
    void visitListExpr(ListExpr *p) override;

    void visitInteger(Integer x) override;
    void visitChar(Char x) override;
    void visitDouble(Double x) override;
    void visitString(String x) override;
    void visitIdent(Ident x) override;
};

#endif
