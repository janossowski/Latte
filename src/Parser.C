/* A Bison parser, made by GNU Bison 3.8.2.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2021 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output, and Bison version.  */
#define YYBISON 30802

/* Bison version string.  */
#define YYBISON_VERSION "3.8.2"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 1

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1


/* Substitute the variable and function names.  */
#define yyparse         latte_parse
#define yylex           latte_lex
#define yyerror         latte_error
#define yydebug         latte_debug
#define yynerrs         latte_nerrs

/* First part of user prologue.  */
#line 20 "Latte.y"

/* Begin C preamble code */

#include <algorithm> /* for std::reverse */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "Absyn.H"

#define YYMAXDEPTH 10000000

/* The type yyscan_t is defined by flex, but we need it in the parser already. */
#ifndef YY_TYPEDEF_YY_SCANNER_T
#define YY_TYPEDEF_YY_SCANNER_T
typedef void* yyscan_t;
#endif

typedef struct yy_buffer_state *YY_BUFFER_STATE;
extern YY_BUFFER_STATE latte__scan_string(const char *str, yyscan_t scanner);
extern void latte__delete_buffer(YY_BUFFER_STATE buf, yyscan_t scanner);

extern void latte_lex_destroy(yyscan_t scanner);
extern char* latte_get_text(yyscan_t scanner);

extern yyscan_t latte__initialize_lexer(FILE * inp);

/* End C preamble code */

#line 105 "Parser.C"

# ifndef YY_CAST
#  ifdef __cplusplus
#   define YY_CAST(Type, Val) static_cast<Type> (Val)
#   define YY_REINTERPRET_CAST(Type, Val) reinterpret_cast<Type> (Val)
#  else
#   define YY_CAST(Type, Val) ((Type) (Val))
#   define YY_REINTERPRET_CAST(Type, Val) ((Type) (Val))
#  endif
# endif
# ifndef YY_NULLPTR
#  if defined __cplusplus
#   if 201103L <= __cplusplus
#    define YY_NULLPTR nullptr
#   else
#    define YY_NULLPTR 0
#   endif
#  else
#   define YY_NULLPTR ((void*)0)
#  endif
# endif

#include "Bison.H"
/* Symbol kind.  */
enum yysymbol_kind_t
{
  YYSYMBOL_YYEMPTY = -2,
  YYSYMBOL_YYEOF = 0,                      /* "end of file"  */
  YYSYMBOL_YYerror = 1,                    /* error  */
  YYSYMBOL_YYUNDEF = 2,                    /* "invalid token"  */
  YYSYMBOL__ERROR_ = 3,                    /* _ERROR_  */
  YYSYMBOL__BANG = 4,                      /* _BANG  */
  YYSYMBOL__BANGEQ = 5,                    /* _BANGEQ  */
  YYSYMBOL__PERCENT = 6,                   /* _PERCENT  */
  YYSYMBOL__DAMP = 7,                      /* _DAMP  */
  YYSYMBOL__LPAREN = 8,                    /* _LPAREN  */
  YYSYMBOL__RPAREN = 9,                    /* _RPAREN  */
  YYSYMBOL__STAR = 10,                     /* _STAR  */
  YYSYMBOL__PLUS = 11,                     /* _PLUS  */
  YYSYMBOL__DPLUS = 12,                    /* _DPLUS  */
  YYSYMBOL__COMMA = 13,                    /* _COMMA  */
  YYSYMBOL__MINUS = 14,                    /* _MINUS  */
  YYSYMBOL__DMINUS = 15,                   /* _DMINUS  */
  YYSYMBOL__SLASH = 16,                    /* _SLASH  */
  YYSYMBOL__SEMI = 17,                     /* _SEMI  */
  YYSYMBOL__LT = 18,                       /* _LT  */
  YYSYMBOL__LDARROW = 19,                  /* _LDARROW  */
  YYSYMBOL__EQ = 20,                       /* _EQ  */
  YYSYMBOL__DEQ = 21,                      /* _DEQ  */
  YYSYMBOL__GT = 22,                       /* _GT  */
  YYSYMBOL__GTEQ = 23,                     /* _GTEQ  */
  YYSYMBOL__KW_boolean = 24,               /* _KW_boolean  */
  YYSYMBOL__KW_else = 25,                  /* _KW_else  */
  YYSYMBOL__KW_false = 26,                 /* _KW_false  */
  YYSYMBOL__KW_if = 27,                    /* _KW_if  */
  YYSYMBOL__KW_int = 28,                   /* _KW_int  */
  YYSYMBOL__KW_return = 29,                /* _KW_return  */
  YYSYMBOL__KW_string = 30,                /* _KW_string  */
  YYSYMBOL__KW_true = 31,                  /* _KW_true  */
  YYSYMBOL__KW_void = 32,                  /* _KW_void  */
  YYSYMBOL__KW_while = 33,                 /* _KW_while  */
  YYSYMBOL__LBRACE = 34,                   /* _LBRACE  */
  YYSYMBOL__DBAR = 35,                     /* _DBAR  */
  YYSYMBOL__RBRACE = 36,                   /* _RBRACE  */
  YYSYMBOL__STRING_ = 37,                  /* _STRING_  */
  YYSYMBOL__INTEGER_ = 38,                 /* _INTEGER_  */
  YYSYMBOL__IDENT_ = 39,                   /* _IDENT_  */
  YYSYMBOL_YYACCEPT = 40,                  /* $accept  */
  YYSYMBOL_Program = 41,                   /* Program  */
  YYSYMBOL_TopDef = 42,                    /* TopDef  */
  YYSYMBOL_ListTopDef = 43,                /* ListTopDef  */
  YYSYMBOL_Arg = 44,                       /* Arg  */
  YYSYMBOL_ListArg = 45,                   /* ListArg  */
  YYSYMBOL_Block = 46,                     /* Block  */
  YYSYMBOL_ListStmt = 47,                  /* ListStmt  */
  YYSYMBOL_Stmt = 48,                      /* Stmt  */
  YYSYMBOL_Item = 49,                      /* Item  */
  YYSYMBOL_ListItem = 50,                  /* ListItem  */
  YYSYMBOL_Type = 51,                      /* Type  */
  YYSYMBOL_Expr6 = 52,                     /* Expr6  */
  YYSYMBOL_Expr5 = 53,                     /* Expr5  */
  YYSYMBOL_Expr4 = 54,                     /* Expr4  */
  YYSYMBOL_Expr3 = 55,                     /* Expr3  */
  YYSYMBOL_Expr2 = 56,                     /* Expr2  */
  YYSYMBOL_Expr1 = 57,                     /* Expr1  */
  YYSYMBOL_Expr = 58,                      /* Expr  */
  YYSYMBOL_ListExpr = 59,                  /* ListExpr  */
  YYSYMBOL_AddOp = 60,                     /* AddOp  */
  YYSYMBOL_MulOp = 61,                     /* MulOp  */
  YYSYMBOL_RelOp = 62                      /* RelOp  */
};
typedef enum yysymbol_kind_t yysymbol_kind_t;


/* Second part of user prologue.  */
#line 74 "Latte.y"

void yyerror(YYLTYPE *loc, yyscan_t scanner, YYSTYPE *result, const char *msg)
{
  fprintf(stderr, "ERROR\n%d,%d: %s at %s\n",
    loc->first_line, loc->first_column, msg, latte_get_text(scanner));
}

int yyparse(yyscan_t scanner, YYSTYPE *result);

extern int yylex(YYSTYPE *lvalp, YYLTYPE *llocp, yyscan_t scanner);

#line 213 "Parser.C"


#ifdef short
# undef short
#endif

/* On compilers that do not define __PTRDIFF_MAX__ etc., make sure
   <limits.h> and (if available) <stdint.h> are included
   so that the code can choose integer types of a good width.  */

#ifndef __PTRDIFF_MAX__
# include <limits.h> /* INFRINGES ON USER NAME SPACE */
# if defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stdint.h> /* INFRINGES ON USER NAME SPACE */
#  define YY_STDINT_H
# endif
#endif

/* Narrow types that promote to a signed type and that can represent a
   signed or unsigned integer of at least N bits.  In tables they can
   save space and decrease cache pressure.  Promoting to a signed type
   helps avoid bugs in integer arithmetic.  */

#ifdef __INT_LEAST8_MAX__
typedef __INT_LEAST8_TYPE__ yytype_int8;
#elif defined YY_STDINT_H
typedef int_least8_t yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef __INT_LEAST16_MAX__
typedef __INT_LEAST16_TYPE__ yytype_int16;
#elif defined YY_STDINT_H
typedef int_least16_t yytype_int16;
#else
typedef short yytype_int16;
#endif

/* Work around bug in HP-UX 11.23, which defines these macros
   incorrectly for preprocessor constants.  This workaround can likely
   be removed in 2023, as HPE has promised support for HP-UX 11.23
   (aka HP-UX 11i v2) only through the end of 2022; see Table 2 of
   <https://h20195.www2.hpe.com/V2/getpdf.aspx/4AA4-7673ENW.pdf>.  */
#ifdef __hpux
# undef UINT_LEAST8_MAX
# undef UINT_LEAST16_MAX
# define UINT_LEAST8_MAX 255
# define UINT_LEAST16_MAX 65535
#endif

#if defined __UINT_LEAST8_MAX__ && __UINT_LEAST8_MAX__ <= __INT_MAX__
typedef __UINT_LEAST8_TYPE__ yytype_uint8;
#elif (!defined __UINT_LEAST8_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST8_MAX <= INT_MAX)
typedef uint_least8_t yytype_uint8;
#elif !defined __UINT_LEAST8_MAX__ && UCHAR_MAX <= INT_MAX
typedef unsigned char yytype_uint8;
#else
typedef short yytype_uint8;
#endif

#if defined __UINT_LEAST16_MAX__ && __UINT_LEAST16_MAX__ <= __INT_MAX__
typedef __UINT_LEAST16_TYPE__ yytype_uint16;
#elif (!defined __UINT_LEAST16_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST16_MAX <= INT_MAX)
typedef uint_least16_t yytype_uint16;
#elif !defined __UINT_LEAST16_MAX__ && USHRT_MAX <= INT_MAX
typedef unsigned short yytype_uint16;
#else
typedef int yytype_uint16;
#endif

#ifndef YYPTRDIFF_T
# if defined __PTRDIFF_TYPE__ && defined __PTRDIFF_MAX__
#  define YYPTRDIFF_T __PTRDIFF_TYPE__
#  define YYPTRDIFF_MAXIMUM __PTRDIFF_MAX__
# elif defined PTRDIFF_MAX
#  ifndef ptrdiff_t
#   include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  endif
#  define YYPTRDIFF_T ptrdiff_t
#  define YYPTRDIFF_MAXIMUM PTRDIFF_MAX
# else
#  define YYPTRDIFF_T long
#  define YYPTRDIFF_MAXIMUM LONG_MAX
# endif
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned
# endif
#endif

#define YYSIZE_MAXIMUM                                  \
  YY_CAST (YYPTRDIFF_T,                                 \
           (YYPTRDIFF_MAXIMUM < YY_CAST (YYSIZE_T, -1)  \
            ? YYPTRDIFF_MAXIMUM                         \
            : YY_CAST (YYSIZE_T, -1)))

#define YYSIZEOF(X) YY_CAST (YYPTRDIFF_T, sizeof (X))


/* Stored state numbers (used for stacks). */
typedef yytype_int8 yy_state_t;

/* State numbers in computations.  */
typedef int yy_state_fast_t;

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif


#ifndef YY_ATTRIBUTE_PURE
# if defined __GNUC__ && 2 < __GNUC__ + (96 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_PURE __attribute__ ((__pure__))
# else
#  define YY_ATTRIBUTE_PURE
# endif
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# if defined __GNUC__ && 2 < __GNUC__ + (7 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_UNUSED __attribute__ ((__unused__))
# else
#  define YY_ATTRIBUTE_UNUSED
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YY_USE(E) ((void) (E))
#else
# define YY_USE(E) /* empty */
#endif

/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
#if defined __GNUC__ && ! defined __ICC && 406 <= __GNUC__ * 100 + __GNUC_MINOR__
# if __GNUC__ * 100 + __GNUC_MINOR__ < 407
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")
# else
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")              \
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# endif
# define YY_IGNORE_MAYBE_UNINITIALIZED_END      \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif

#if defined __cplusplus && defined __GNUC__ && ! defined __ICC && 6 <= __GNUC__
# define YY_IGNORE_USELESS_CAST_BEGIN                          \
    _Pragma ("GCC diagnostic push")                            \
    _Pragma ("GCC diagnostic ignored \"-Wuseless-cast\"")
# define YY_IGNORE_USELESS_CAST_END            \
    _Pragma ("GCC diagnostic pop")
#endif
#ifndef YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_END
#endif


#define YY_ASSERT(E) ((void) (0 && (E)))

#if !defined yyoverflow

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* !defined yyoverflow */

#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL \
             && defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yy_state_t yyss_alloc;
  YYSTYPE yyvs_alloc;
  YYLTYPE yyls_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (YYSIZEOF (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (YYSIZEOF (yy_state_t) + YYSIZEOF (YYSTYPE) \
             + YYSIZEOF (YYLTYPE)) \
      + 2 * YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYPTRDIFF_T yynewbytes;                                         \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * YYSIZEOF (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / YYSIZEOF (*yyptr);                        \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, YY_CAST (YYSIZE_T, (Count)) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYPTRDIFF_T yyi;                      \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  9
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   135

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  40
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  23
/* YYNRULES -- Number of rules.  */
#define YYNRULES  66
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  107

/* YYMAXUTOK -- Last valid token kind.  */
#define YYMAXUTOK   294


/* YYTRANSLATE(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, with out-of-bounds checking.  */
#define YYTRANSLATE(YYX)                                \
  (0 <= (YYX) && (YYX) <= YYMAXUTOK                     \
   ? YY_CAST (yysymbol_kind_t, yytranslate[YYX])        \
   : YYSYMBOL_YYUNDEF)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex.  */
static const yytype_int8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39
};

#if YYDEBUG
/* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint8 yyrline[] =
{
       0,   152,   152,   154,   156,   157,   159,   161,   162,   163,
     165,   167,   168,   170,   171,   172,   173,   174,   175,   176,
     177,   178,   179,   180,   181,   183,   184,   186,   187,   189,
     190,   191,   192,   198,   199,   200,   201,   202,   203,   204,
     206,   207,   208,   210,   211,   213,   214,   216,   217,   219,
     220,   222,   223,   225,   226,   227,   229,   230,   232,   233,
     234,   236,   237,   238,   239,   240,   241
};
#endif

/** Accessing symbol of state STATE.  */
#define YY_ACCESSING_SYMBOL(State) YY_CAST (yysymbol_kind_t, yystos[State])

#if YYDEBUG || 0
/* The user-facing name of the symbol whose (internal) number is
   YYSYMBOL.  No bounds checking.  */
static const char *yysymbol_name (yysymbol_kind_t yysymbol) YY_ATTRIBUTE_UNUSED;

/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "\"end of file\"", "error", "\"invalid token\"", "_ERROR_", "_BANG",
  "_BANGEQ", "_PERCENT", "_DAMP", "_LPAREN", "_RPAREN", "_STAR", "_PLUS",
  "_DPLUS", "_COMMA", "_MINUS", "_DMINUS", "_SLASH", "_SEMI", "_LT",
  "_LDARROW", "_EQ", "_DEQ", "_GT", "_GTEQ", "_KW_boolean", "_KW_else",
  "_KW_false", "_KW_if", "_KW_int", "_KW_return", "_KW_string", "_KW_true",
  "_KW_void", "_KW_while", "_LBRACE", "_DBAR", "_RBRACE", "_STRING_",
  "_INTEGER_", "_IDENT_", "$accept", "Program", "TopDef", "ListTopDef",
  "Arg", "ListArg", "Block", "ListStmt", "Stmt", "Item", "ListItem",
  "Type", "Expr6", "Expr5", "Expr4", "Expr3", "Expr2", "Expr1", "Expr",
  "ListExpr", "AddOp", "MulOp", "RelOp", YY_NULLPTR
};

static const char *
yysymbol_name (yysymbol_kind_t yysymbol)
{
  return yytname[yysymbol];
}
#endif

#define YYPACT_NINF (-86)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-1)

#define yytable_value_is_error(Yyn) \
  0

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int8 yypact[] =
{
      73,   -86,   -86,   -86,   -86,     2,    73,   -86,   -30,   -86,
     -86,     5,    73,    10,    12,   -15,    73,    -6,   -86,   -86,
     -86,   -86,     8,    86,    96,    86,   -86,   -86,    19,    82,
     -86,    23,   -86,   -86,   -86,    45,   -86,   -86,     9,   -86,
     -86,    56,    -7,    -4,    15,    34,    48,   -86,    52,   -86,
      96,   -86,    51,    96,    96,    53,    54,    96,    49,    62,
      68,   -86,   -86,   -86,    96,   -86,   -86,    96,   -86,    96,
     -86,   -86,   -86,   -86,   -86,    96,    96,   -86,   -86,    83,
     -86,    84,    78,    89,   -86,   -86,    85,    96,     9,   -86,
     -86,    56,   -86,    -7,   -86,    50,    50,    96,   -86,   -86,
     -86,   -86,    70,   -86,   -86,    50,   -86
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_int8 yydefact[] =
{
       0,    31,    29,    30,    32,     0,     4,     2,     0,     1,
       5,     0,     7,     8,     0,     0,     7,     0,     6,     9,
      11,     3,     0,     0,     0,     0,    13,    36,     0,     0,
      35,     0,    10,    38,    34,    33,    14,    12,     0,    42,
      44,    46,    48,    50,    52,     0,    33,    41,     0,    40,
       0,    20,     0,     0,    53,     0,     0,     0,    25,    27,
       0,    60,    58,    59,     0,    56,    57,     0,    66,     0,
      61,    62,    65,    63,    64,     0,     0,    24,    39,     0,
      19,     0,    54,     0,    17,    18,     0,     0,     0,    15,
      43,    45,    49,    47,    51,     0,     0,    53,    37,    16,
      26,    28,    21,    23,    55,     0,    22
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
     -86,   -86,   -86,   100,   -86,    91,    92,   -86,   -85,   -86,
      26,    43,   -17,    47,    59,    40,   -86,    60,   -24,    21,
     -86,   -86,   -86
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int8 yydefgoto[] =
{
       0,     5,     6,     7,    13,    14,    36,    22,    37,    59,
      60,    38,    39,    40,    41,    42,    43,    44,    45,    83,
      67,    64,    75
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int8 yytable[] =
{
      48,    68,     9,    69,    65,    52,    47,    66,    49,    11,
     102,   103,    23,    12,    70,    71,    24,    72,    73,    74,
     106,    17,    25,    16,    18,    26,    79,    50,    20,    81,
      82,    53,     1,    86,    27,    28,     2,    29,     3,    30,
       4,    31,    20,     8,    32,    33,    34,    35,    58,     8,
      76,    77,    94,    54,    23,    15,    54,    55,    24,    15,
      56,    78,    61,   100,    25,    57,    62,    26,    80,    87,
      84,    85,    63,    82,     1,    88,    27,    28,     2,    29,
       3,    30,     4,    31,    20,    89,    23,    33,    34,    35,
      24,    97,    95,    96,    24,   105,    25,     1,    98,    51,
      23,     2,    99,     3,    24,     4,    10,    19,    27,    21,
      25,    90,    27,    30,   101,    93,     0,    30,   104,    33,
      34,    46,    27,    33,    34,    46,    91,    30,     0,    92,
       0,     0,     0,    33,    34,    46
};

static const yytype_int8 yycheck[] =
{
      24,     5,     0,     7,    11,    29,    23,    14,    25,    39,
      95,    96,     4,     8,    18,    19,     8,    21,    22,    23,
     105,     9,    14,    13,    39,    17,    50,     8,    34,    53,
      54,     8,    24,    57,    26,    27,    28,    29,    30,    31,
      32,    33,    34,     0,    36,    37,    38,    39,    39,     6,
      35,    17,    76,     8,     4,    12,     8,    12,     8,    16,
      15,     9,     6,    87,    14,    20,    10,    17,    17,    20,
      17,    17,    16,    97,    24,    13,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    17,     4,    37,    38,    39,
       8,    13,     9,     9,     8,    25,    14,    24,     9,    17,
       4,    28,    17,    30,     8,    32,     6,    16,    26,    17,
      14,    64,    26,    31,    88,    75,    -1,    31,    97,    37,
      38,    39,    26,    37,    38,    39,    67,    31,    -1,    69,
      -1,    -1,    -1,    37,    38,    39
};

/* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
   state STATE-NUM.  */
static const yytype_int8 yystos[] =
{
       0,    24,    28,    30,    32,    41,    42,    43,    51,     0,
      43,    39,     8,    44,    45,    51,    13,     9,    39,    45,
      34,    46,    47,     4,     8,    14,    17,    26,    27,    29,
      31,    33,    36,    37,    38,    39,    46,    48,    51,    52,
      53,    54,    55,    56,    57,    58,    39,    52,    58,    52,
       8,    17,    58,     8,     8,    12,    15,    20,    39,    49,
      50,     6,    10,    16,    61,    11,    14,    60,     5,     7,
      18,    19,    21,    22,    23,    62,    35,    17,     9,    58,
      17,    58,    58,    59,    17,    17,    58,    20,    13,    17,
      53,    54,    57,    55,    58,     9,     9,    13,     9,    17,
      58,    50,    48,    48,    59,    25,    48
};

/* YYR1[RULE-NUM] -- Symbol kind of the left-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr1[] =
{
       0,    40,    41,    42,    43,    43,    44,    45,    45,    45,
      46,    47,    47,    48,    48,    48,    48,    48,    48,    48,
      48,    48,    48,    48,    48,    49,    49,    50,    50,    51,
      51,    51,    51,    52,    52,    52,    52,    52,    52,    52,
      53,    53,    53,    54,    54,    55,    55,    56,    56,    57,
      57,    58,    58,    59,    59,    59,    60,    60,    61,    61,
      61,    62,    62,    62,    62,    62,    62
};

/* YYR2[RULE-NUM] -- Number of symbols on the right-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     1,     6,     1,     2,     2,     0,     1,     3,
       3,     0,     2,     1,     1,     3,     4,     3,     3,     3,
       2,     5,     7,     5,     2,     1,     3,     1,     3,     1,
       1,     1,     1,     1,     1,     1,     1,     4,     1,     3,
       2,     2,     1,     3,     1,     3,     1,     3,     1,     3,
       1,     3,     1,     0,     1,     3,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1
};


enum { YYENOMEM = -2 };

#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab
#define YYNOMEM         goto yyexhaustedlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                    \
  do                                                              \
    if (yychar == YYEMPTY)                                        \
      {                                                           \
        yychar = (Token);                                         \
        yylval = (Value);                                         \
        YYPOPSTACK (yylen);                                       \
        yystate = *yyssp;                                         \
        goto yybackup;                                            \
      }                                                           \
    else                                                          \
      {                                                           \
        yyerror (&yylloc, scanner, result, YY_("syntax error: cannot back up")); \
        YYERROR;                                                  \
      }                                                           \
  while (0)

/* Backward compatibility with an undocumented macro.
   Use YYerror or YYUNDEF. */
#define YYERRCODE YYUNDEF

/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)                                \
    do                                                                  \
      if (N)                                                            \
        {                                                               \
          (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;        \
          (Current).first_column = YYRHSLOC (Rhs, 1).first_column;      \
          (Current).last_line    = YYRHSLOC (Rhs, N).last_line;         \
          (Current).last_column  = YYRHSLOC (Rhs, N).last_column;       \
        }                                                               \
      else                                                              \
        {                                                               \
          (Current).first_line   = (Current).last_line   =              \
            YYRHSLOC (Rhs, 0).last_line;                                \
          (Current).first_column = (Current).last_column =              \
            YYRHSLOC (Rhs, 0).last_column;                              \
        }                                                               \
    while (0)
#endif

#define YYRHSLOC(Rhs, K) ((Rhs)[K])


/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)


/* YYLOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

# ifndef YYLOCATION_PRINT

#  if defined YY_LOCATION_PRINT

   /* Temporary convenience wrapper in case some people defined the
      undocumented and private YY_LOCATION_PRINT macros.  */
#   define YYLOCATION_PRINT(File, Loc)  YY_LOCATION_PRINT(File, *(Loc))

#  elif defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL

/* Print *YYLOCP on YYO.  Private, do not rely on its existence. */

YY_ATTRIBUTE_UNUSED
static int
yy_location_print_ (FILE *yyo, YYLTYPE const * const yylocp)
{
  int res = 0;
  int end_col = 0 != yylocp->last_column ? yylocp->last_column - 1 : 0;
  if (0 <= yylocp->first_line)
    {
      res += YYFPRINTF (yyo, "%d", yylocp->first_line);
      if (0 <= yylocp->first_column)
        res += YYFPRINTF (yyo, ".%d", yylocp->first_column);
    }
  if (0 <= yylocp->last_line)
    {
      if (yylocp->first_line < yylocp->last_line)
        {
          res += YYFPRINTF (yyo, "-%d", yylocp->last_line);
          if (0 <= end_col)
            res += YYFPRINTF (yyo, ".%d", end_col);
        }
      else if (0 <= end_col && yylocp->first_column < end_col)
        res += YYFPRINTF (yyo, "-%d", end_col);
    }
  return res;
}

#   define YYLOCATION_PRINT  yy_location_print_

    /* Temporary convenience wrapper in case some people defined the
       undocumented and private YY_LOCATION_PRINT macros.  */
#   define YY_LOCATION_PRINT(File, Loc)  YYLOCATION_PRINT(File, &(Loc))

#  else

#   define YYLOCATION_PRINT(File, Loc) ((void) 0)
    /* Temporary convenience wrapper in case some people defined the
       undocumented and private YY_LOCATION_PRINT macros.  */
#   define YY_LOCATION_PRINT  YYLOCATION_PRINT

#  endif
# endif /* !defined YYLOCATION_PRINT */


# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Kind, Value, Location, scanner, result); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*-----------------------------------.
| Print this symbol's value on YYO.  |
`-----------------------------------*/

static void
yy_symbol_value_print (FILE *yyo,
                       yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp, yyscan_t scanner, YYSTYPE *result)
{
  FILE *yyoutput = yyo;
  YY_USE (yyoutput);
  YY_USE (yylocationp);
  YY_USE (scanner);
  YY_USE (result);
  if (!yyvaluep)
    return;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/*---------------------------.
| Print this symbol on YYO.  |
`---------------------------*/

static void
yy_symbol_print (FILE *yyo,
                 yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp, yyscan_t scanner, YYSTYPE *result)
{
  YYFPRINTF (yyo, "%s %s (",
             yykind < YYNTOKENS ? "token" : "nterm", yysymbol_name (yykind));

  YYLOCATION_PRINT (yyo, yylocationp);
  YYFPRINTF (yyo, ": ");
  yy_symbol_value_print (yyo, yykind, yyvaluep, yylocationp, scanner, result);
  YYFPRINTF (yyo, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yy_state_t *yybottom, yy_state_t *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yy_state_t *yyssp, YYSTYPE *yyvsp, YYLTYPE *yylsp,
                 int yyrule, yyscan_t scanner, YYSTYPE *result)
{
  int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %d):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       YY_ACCESSING_SYMBOL (+yyssp[yyi + 1 - yynrhs]),
                       &yyvsp[(yyi + 1) - (yynrhs)],
                       &(yylsp[(yyi + 1) - (yynrhs)]), scanner, result);
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, yylsp, Rule, scanner, result); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args) ((void) 0)
# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif






/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg,
            yysymbol_kind_t yykind, YYSTYPE *yyvaluep, YYLTYPE *yylocationp, yyscan_t scanner, YYSTYPE *result)
{
  YY_USE (yyvaluep);
  YY_USE (yylocationp);
  YY_USE (scanner);
  YY_USE (result);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yykind, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}






/*----------.
| yyparse.  |
`----------*/

int
yyparse (yyscan_t scanner, YYSTYPE *result)
{
/* Lookahead token kind.  */
int yychar;


/* The semantic value of the lookahead symbol.  */
/* Default value used for initialization, for pacifying older GCCs
   or non-GCC compilers.  */
YY_INITIAL_VALUE (static YYSTYPE yyval_default;)
YYSTYPE yylval YY_INITIAL_VALUE (= yyval_default);

/* Location data for the lookahead symbol.  */
static YYLTYPE yyloc_default
# if defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL
  = { 1, 1, 1, 1 }
# endif
;
YYLTYPE yylloc = yyloc_default;

    /* Number of syntax errors so far.  */
    int yynerrs = 0;

    yy_state_fast_t yystate = 0;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus = 0;

    /* Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* Their size.  */
    YYPTRDIFF_T yystacksize = YYINITDEPTH;

    /* The state stack: array, bottom, top.  */
    yy_state_t yyssa[YYINITDEPTH];
    yy_state_t *yyss = yyssa;
    yy_state_t *yyssp = yyss;

    /* The semantic value stack: array, bottom, top.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs = yyvsa;
    YYSTYPE *yyvsp = yyvs;

    /* The location stack: array, bottom, top.  */
    YYLTYPE yylsa[YYINITDEPTH];
    YYLTYPE *yyls = yylsa;
    YYLTYPE *yylsp = yyls;

  int yyn;
  /* The return value of yyparse.  */
  int yyresult;
  /* Lookahead symbol kind.  */
  yysymbol_kind_t yytoken = YYSYMBOL_YYEMPTY;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;
  YYLTYPE yyloc;

  /* The locations where the error started and ended.  */
  YYLTYPE yyerror_range[3];



#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N), yylsp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yychar = YYEMPTY; /* Cause a token to be read.  */

  yylsp[0] = yylloc;
  goto yysetstate;


/*------------------------------------------------------------.
| yynewstate -- push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;


/*--------------------------------------------------------------------.
| yysetstate -- set current state (the top of the stack) to yystate.  |
`--------------------------------------------------------------------*/
yysetstate:
  YYDPRINTF ((stderr, "Entering state %d\n", yystate));
  YY_ASSERT (0 <= yystate && yystate < YYNSTATES);
  YY_IGNORE_USELESS_CAST_BEGIN
  *yyssp = YY_CAST (yy_state_t, yystate);
  YY_IGNORE_USELESS_CAST_END
  YY_STACK_PRINT (yyss, yyssp);

  if (yyss + yystacksize - 1 <= yyssp)
#if !defined yyoverflow && !defined YYSTACK_RELOCATE
    YYNOMEM;
#else
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYPTRDIFF_T yysize = yyssp - yyss + 1;

# if defined yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        yy_state_t *yyss1 = yyss;
        YYSTYPE *yyvs1 = yyvs;
        YYLTYPE *yyls1 = yyls;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * YYSIZEOF (*yyssp),
                    &yyvs1, yysize * YYSIZEOF (*yyvsp),
                    &yyls1, yysize * YYSIZEOF (*yylsp),
                    &yystacksize);
        yyss = yyss1;
        yyvs = yyvs1;
        yyls = yyls1;
      }
# else /* defined YYSTACK_RELOCATE */
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        YYNOMEM;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yy_state_t *yyss1 = yyss;
        union yyalloc *yyptr =
          YY_CAST (union yyalloc *,
                   YYSTACK_ALLOC (YY_CAST (YYSIZE_T, YYSTACK_BYTES (yystacksize))));
        if (! yyptr)
          YYNOMEM;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
        YYSTACK_RELOCATE (yyls_alloc, yyls);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;
      yylsp = yyls + yysize - 1;

      YY_IGNORE_USELESS_CAST_BEGIN
      YYDPRINTF ((stderr, "Stack size increased to %ld\n",
                  YY_CAST (long, yystacksize)));
      YY_IGNORE_USELESS_CAST_END

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }
#endif /* !defined yyoverflow && !defined YYSTACK_RELOCATE */


  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;


/*-----------.
| yybackup.  |
`-----------*/
yybackup:
  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either empty, or end-of-input, or a valid lookahead.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token\n"));
      yychar = yylex (&yylval, &yylloc, scanner);
    }

  if (yychar <= YYEOF)
    {
      yychar = YYEOF;
      yytoken = YYSYMBOL_YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else if (yychar == YYerror)
    {
      /* The scanner already issued an error message, process directly
         to error recovery.  But do not keep the error token as
         lookahead, it is too special and may lead us to an endless
         loop in error recovery. */
      yychar = YYUNDEF;
      yytoken = YYSYMBOL_YYerror;
      yyerror_range[1] = yylloc;
      goto yyerrlab1;
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);
  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END
  *++yylsp = yylloc;

  /* Discard the shifted token.  */
  yychar = YYEMPTY;
  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];

  /* Default location. */
  YYLLOC_DEFAULT (yyloc, (yylsp - yylen), yylen);
  yyerror_range[1] = yyloc;
  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
  case 2: /* Program: ListTopDef  */
#line 152 "Latte.y"
                     { std::reverse((yyvsp[0].listtopdef_)->begin(),(yyvsp[0].listtopdef_)->end()) ;(yyval.program_) = new Prog((yyvsp[0].listtopdef_)); (yyval.program_)->line_number = (yyloc).first_line; (yyval.program_)->char_number = (yyloc).first_column; result->program_ = (yyval.program_); }
#line 1372 "Parser.C"
    break;

  case 3: /* TopDef: Type _IDENT_ _LPAREN ListArg _RPAREN Block  */
#line 154 "Latte.y"
                                                    { std::reverse((yyvsp[-2].listarg_)->begin(),(yyvsp[-2].listarg_)->end()) ;(yyval.topdef_) = new FnDef((yyvsp[-5].type_), (yyvsp[-4]._string), (yyvsp[-2].listarg_), (yyvsp[0].block_)); (yyval.topdef_)->line_number = (yyloc).first_line; (yyval.topdef_)->char_number = (yyloc).first_column; }
#line 1378 "Parser.C"
    break;

  case 4: /* ListTopDef: TopDef  */
#line 156 "Latte.y"
                    { (yyval.listtopdef_) = new ListTopDef(); (yyval.listtopdef_)->push_back((yyvsp[0].topdef_)); }
#line 1384 "Parser.C"
    break;

  case 5: /* ListTopDef: TopDef ListTopDef  */
#line 157 "Latte.y"
                      { (yyvsp[0].listtopdef_)->push_back((yyvsp[-1].topdef_)); (yyval.listtopdef_) = (yyvsp[0].listtopdef_); }
#line 1390 "Parser.C"
    break;

  case 6: /* Arg: Type _IDENT_  */
#line 159 "Latte.y"
                   { (yyval.arg_) = new Ar((yyvsp[-1].type_), (yyvsp[0]._string)); (yyval.arg_)->line_number = (yyloc).first_line; (yyval.arg_)->char_number = (yyloc).first_column; }
#line 1396 "Parser.C"
    break;

  case 7: /* ListArg: %empty  */
#line 161 "Latte.y"
                      { (yyval.listarg_) = new ListArg(); }
#line 1402 "Parser.C"
    break;

  case 8: /* ListArg: Arg  */
#line 162 "Latte.y"
        { (yyval.listarg_) = new ListArg(); (yyval.listarg_)->push_back((yyvsp[0].arg_)); }
#line 1408 "Parser.C"
    break;

  case 9: /* ListArg: Arg _COMMA ListArg  */
#line 163 "Latte.y"
                       { (yyvsp[0].listarg_)->push_back((yyvsp[-2].arg_)); (yyval.listarg_) = (yyvsp[0].listarg_); }
#line 1414 "Parser.C"
    break;

  case 10: /* Block: _LBRACE ListStmt _RBRACE  */
#line 165 "Latte.y"
                                 { (yyval.block_) = new Blk((yyvsp[-1].liststmt_)); (yyval.block_)->line_number = (yyloc).first_line; (yyval.block_)->char_number = (yyloc).first_column; }
#line 1420 "Parser.C"
    break;

  case 11: /* ListStmt: %empty  */
#line 167 "Latte.y"
                       { (yyval.liststmt_) = new ListStmt(); }
#line 1426 "Parser.C"
    break;

  case 12: /* ListStmt: ListStmt Stmt  */
#line 168 "Latte.y"
                  { (yyvsp[-1].liststmt_)->push_back((yyvsp[0].stmt_)); (yyval.liststmt_) = (yyvsp[-1].liststmt_); }
#line 1432 "Parser.C"
    break;

  case 13: /* Stmt: _SEMI  */
#line 170 "Latte.y"
             { (yyval.stmt_) = new Empty(); (yyval.stmt_)->line_number = (yyloc).first_line; (yyval.stmt_)->char_number = (yyloc).first_column; }
#line 1438 "Parser.C"
    break;

  case 14: /* Stmt: Block  */
#line 171 "Latte.y"
          { (yyval.stmt_) = new BStmt((yyvsp[0].block_)); (yyval.stmt_)->line_number = (yyloc).first_line; (yyval.stmt_)->char_number = (yyloc).first_column; }
#line 1444 "Parser.C"
    break;

  case 15: /* Stmt: Type ListItem _SEMI  */
#line 172 "Latte.y"
                        { std::reverse((yyvsp[-1].listitem_)->begin(),(yyvsp[-1].listitem_)->end()) ;(yyval.stmt_) = new Decl((yyvsp[-2].type_), (yyvsp[-1].listitem_)); (yyval.stmt_)->line_number = (yyloc).first_line; (yyval.stmt_)->char_number = (yyloc).first_column; }
#line 1450 "Parser.C"
    break;

  case 16: /* Stmt: _IDENT_ _EQ Expr _SEMI  */
#line 173 "Latte.y"
                           { (yyval.stmt_) = new Ass((yyvsp[-3]._string), (yyvsp[-1].expr_)); (yyval.stmt_)->line_number = (yyloc).first_line; (yyval.stmt_)->char_number = (yyloc).first_column; }
#line 1456 "Parser.C"
    break;

  case 17: /* Stmt: _IDENT_ _DPLUS _SEMI  */
#line 174 "Latte.y"
                         { (yyval.stmt_) = new Incr((yyvsp[-2]._string)); (yyval.stmt_)->line_number = (yyloc).first_line; (yyval.stmt_)->char_number = (yyloc).first_column; }
#line 1462 "Parser.C"
    break;

  case 18: /* Stmt: _IDENT_ _DMINUS _SEMI  */
#line 175 "Latte.y"
                          { (yyval.stmt_) = new Decr((yyvsp[-2]._string)); (yyval.stmt_)->line_number = (yyloc).first_line; (yyval.stmt_)->char_number = (yyloc).first_column; }
#line 1468 "Parser.C"
    break;

  case 19: /* Stmt: _KW_return Expr _SEMI  */
#line 176 "Latte.y"
                          { (yyval.stmt_) = new Ret((yyvsp[-1].expr_)); (yyval.stmt_)->line_number = (yyloc).first_line; (yyval.stmt_)->char_number = (yyloc).first_column; }
#line 1474 "Parser.C"
    break;

  case 20: /* Stmt: _KW_return _SEMI  */
#line 177 "Latte.y"
                     { (yyval.stmt_) = new VRet(); (yyval.stmt_)->line_number = (yyloc).first_line; (yyval.stmt_)->char_number = (yyloc).first_column; }
#line 1480 "Parser.C"
    break;

  case 21: /* Stmt: _KW_if _LPAREN Expr _RPAREN Stmt  */
#line 178 "Latte.y"
                                     { (yyval.stmt_) = new Cond((yyvsp[-2].expr_), (yyvsp[0].stmt_)); (yyval.stmt_)->line_number = (yyloc).first_line; (yyval.stmt_)->char_number = (yyloc).first_column; }
#line 1486 "Parser.C"
    break;

  case 22: /* Stmt: _KW_if _LPAREN Expr _RPAREN Stmt _KW_else Stmt  */
#line 179 "Latte.y"
                                                   { (yyval.stmt_) = new CondElse((yyvsp[-4].expr_), (yyvsp[-2].stmt_), (yyvsp[0].stmt_)); (yyval.stmt_)->line_number = (yyloc).first_line; (yyval.stmt_)->char_number = (yyloc).first_column; }
#line 1492 "Parser.C"
    break;

  case 23: /* Stmt: _KW_while _LPAREN Expr _RPAREN Stmt  */
#line 180 "Latte.y"
                                        { (yyval.stmt_) = new While((yyvsp[-2].expr_), (yyvsp[0].stmt_)); (yyval.stmt_)->line_number = (yyloc).first_line; (yyval.stmt_)->char_number = (yyloc).first_column; }
#line 1498 "Parser.C"
    break;

  case 24: /* Stmt: Expr _SEMI  */
#line 181 "Latte.y"
               { (yyval.stmt_) = new SExp((yyvsp[-1].expr_)); (yyval.stmt_)->line_number = (yyloc).first_line; (yyval.stmt_)->char_number = (yyloc).first_column; }
#line 1504 "Parser.C"
    break;

  case 25: /* Item: _IDENT_  */
#line 183 "Latte.y"
               { (yyval.item_) = new NoInit((yyvsp[0]._string)); (yyval.item_)->line_number = (yyloc).first_line; (yyval.item_)->char_number = (yyloc).first_column; }
#line 1510 "Parser.C"
    break;

  case 26: /* Item: _IDENT_ _EQ Expr  */
#line 184 "Latte.y"
                     { (yyval.item_) = new Init((yyvsp[-2]._string), (yyvsp[0].expr_)); (yyval.item_)->line_number = (yyloc).first_line; (yyval.item_)->char_number = (yyloc).first_column; }
#line 1516 "Parser.C"
    break;

  case 27: /* ListItem: Item  */
#line 186 "Latte.y"
                { (yyval.listitem_) = new ListItem(); (yyval.listitem_)->push_back((yyvsp[0].item_)); }
#line 1522 "Parser.C"
    break;

  case 28: /* ListItem: Item _COMMA ListItem  */
#line 187 "Latte.y"
                         { (yyvsp[0].listitem_)->push_back((yyvsp[-2].item_)); (yyval.listitem_) = (yyvsp[0].listitem_); }
#line 1528 "Parser.C"
    break;

  case 29: /* Type: _KW_int  */
#line 189 "Latte.y"
               { (yyval.type_) = new Int(); (yyval.type_)->line_number = (yyloc).first_line; (yyval.type_)->char_number = (yyloc).first_column; }
#line 1534 "Parser.C"
    break;

  case 30: /* Type: _KW_string  */
#line 190 "Latte.y"
               { (yyval.type_) = new Str(); (yyval.type_)->line_number = (yyloc).first_line; (yyval.type_)->char_number = (yyloc).first_column; }
#line 1540 "Parser.C"
    break;

  case 31: /* Type: _KW_boolean  */
#line 191 "Latte.y"
                { (yyval.type_) = new Bool(); (yyval.type_)->line_number = (yyloc).first_line; (yyval.type_)->char_number = (yyloc).first_column; }
#line 1546 "Parser.C"
    break;

  case 32: /* Type: _KW_void  */
#line 192 "Latte.y"
             { (yyval.type_) = new Void(); (yyval.type_)->line_number = (yyloc).first_line; (yyval.type_)->char_number = (yyloc).first_column; }
#line 1552 "Parser.C"
    break;

  case 33: /* Expr6: _IDENT_  */
#line 198 "Latte.y"
                { (yyval.expr_) = new EVar((yyvsp[0]._string)); (yyval.expr_)->line_number = (yyloc).first_line; (yyval.expr_)->char_number = (yyloc).first_column; }
#line 1558 "Parser.C"
    break;

  case 34: /* Expr6: _INTEGER_  */
#line 199 "Latte.y"
              { (yyval.expr_) = new ELitInt((yyvsp[0]._int)); (yyval.expr_)->line_number = (yyloc).first_line; (yyval.expr_)->char_number = (yyloc).first_column; }
#line 1564 "Parser.C"
    break;

  case 35: /* Expr6: _KW_true  */
#line 200 "Latte.y"
             { (yyval.expr_) = new ELitTrue(); (yyval.expr_)->line_number = (yyloc).first_line; (yyval.expr_)->char_number = (yyloc).first_column; }
#line 1570 "Parser.C"
    break;

  case 36: /* Expr6: _KW_false  */
#line 201 "Latte.y"
              { (yyval.expr_) = new ELitFalse(); (yyval.expr_)->line_number = (yyloc).first_line; (yyval.expr_)->char_number = (yyloc).first_column; }
#line 1576 "Parser.C"
    break;

  case 37: /* Expr6: _IDENT_ _LPAREN ListExpr _RPAREN  */
#line 202 "Latte.y"
                                     { std::reverse((yyvsp[-1].listexpr_)->begin(),(yyvsp[-1].listexpr_)->end()) ;(yyval.expr_) = new EApp((yyvsp[-3]._string), (yyvsp[-1].listexpr_)); (yyval.expr_)->line_number = (yyloc).first_line; (yyval.expr_)->char_number = (yyloc).first_column; }
#line 1582 "Parser.C"
    break;

  case 38: /* Expr6: _STRING_  */
#line 203 "Latte.y"
             { (yyval.expr_) = new EString((yyvsp[0]._string)); (yyval.expr_)->line_number = (yyloc).first_line; (yyval.expr_)->char_number = (yyloc).first_column; }
#line 1588 "Parser.C"
    break;

  case 39: /* Expr6: _LPAREN Expr _RPAREN  */
#line 204 "Latte.y"
                         { (yyval.expr_) = (yyvsp[-1].expr_); (yyval.expr_)->line_number = (yyloc).first_line; (yyval.expr_)->char_number = (yyloc).first_column; }
#line 1594 "Parser.C"
    break;

  case 40: /* Expr5: _MINUS Expr6  */
#line 206 "Latte.y"
                     { (yyval.expr_) = new Neg((yyvsp[0].expr_)); (yyval.expr_)->line_number = (yyloc).first_line; (yyval.expr_)->char_number = (yyloc).first_column; }
#line 1600 "Parser.C"
    break;

  case 41: /* Expr5: _BANG Expr6  */
#line 207 "Latte.y"
                { (yyval.expr_) = new Not((yyvsp[0].expr_)); (yyval.expr_)->line_number = (yyloc).first_line; (yyval.expr_)->char_number = (yyloc).first_column; }
#line 1606 "Parser.C"
    break;

  case 42: /* Expr5: Expr6  */
#line 208 "Latte.y"
          { (yyval.expr_) = (yyvsp[0].expr_); (yyval.expr_)->line_number = (yyloc).first_line; (yyval.expr_)->char_number = (yyloc).first_column; }
#line 1612 "Parser.C"
    break;

  case 43: /* Expr4: Expr4 MulOp Expr5  */
#line 210 "Latte.y"
                          { (yyval.expr_) = new EMul((yyvsp[-2].expr_), (yyvsp[-1].mulop_), (yyvsp[0].expr_)); (yyval.expr_)->line_number = (yyloc).first_line; (yyval.expr_)->char_number = (yyloc).first_column; }
#line 1618 "Parser.C"
    break;

  case 44: /* Expr4: Expr5  */
#line 211 "Latte.y"
          { (yyval.expr_) = (yyvsp[0].expr_); (yyval.expr_)->line_number = (yyloc).first_line; (yyval.expr_)->char_number = (yyloc).first_column; }
#line 1624 "Parser.C"
    break;

  case 45: /* Expr3: Expr3 AddOp Expr4  */
#line 213 "Latte.y"
                          { (yyval.expr_) = new EAdd((yyvsp[-2].expr_), (yyvsp[-1].addop_), (yyvsp[0].expr_)); (yyval.expr_)->line_number = (yyloc).first_line; (yyval.expr_)->char_number = (yyloc).first_column; }
#line 1630 "Parser.C"
    break;

  case 46: /* Expr3: Expr4  */
#line 214 "Latte.y"
          { (yyval.expr_) = (yyvsp[0].expr_); (yyval.expr_)->line_number = (yyloc).first_line; (yyval.expr_)->char_number = (yyloc).first_column; }
#line 1636 "Parser.C"
    break;

  case 47: /* Expr2: Expr2 RelOp Expr3  */
#line 216 "Latte.y"
                          { (yyval.expr_) = new ERel((yyvsp[-2].expr_), (yyvsp[-1].relop_), (yyvsp[0].expr_)); (yyval.expr_)->line_number = (yyloc).first_line; (yyval.expr_)->char_number = (yyloc).first_column; }
#line 1642 "Parser.C"
    break;

  case 48: /* Expr2: Expr3  */
#line 217 "Latte.y"
          { (yyval.expr_) = (yyvsp[0].expr_); (yyval.expr_)->line_number = (yyloc).first_line; (yyval.expr_)->char_number = (yyloc).first_column; }
#line 1648 "Parser.C"
    break;

  case 49: /* Expr1: Expr2 _DAMP Expr1  */
#line 219 "Latte.y"
                          { (yyval.expr_) = new EAnd((yyvsp[-2].expr_), (yyvsp[0].expr_)); (yyval.expr_)->line_number = (yyloc).first_line; (yyval.expr_)->char_number = (yyloc).first_column; }
#line 1654 "Parser.C"
    break;

  case 50: /* Expr1: Expr2  */
#line 220 "Latte.y"
          { (yyval.expr_) = (yyvsp[0].expr_); (yyval.expr_)->line_number = (yyloc).first_line; (yyval.expr_)->char_number = (yyloc).first_column; }
#line 1660 "Parser.C"
    break;

  case 51: /* Expr: Expr1 _DBAR Expr  */
#line 222 "Latte.y"
                        { (yyval.expr_) = new EOr((yyvsp[-2].expr_), (yyvsp[0].expr_)); (yyval.expr_)->line_number = (yyloc).first_line; (yyval.expr_)->char_number = (yyloc).first_column; }
#line 1666 "Parser.C"
    break;

  case 52: /* Expr: Expr1  */
#line 223 "Latte.y"
          { (yyval.expr_) = (yyvsp[0].expr_); (yyval.expr_)->line_number = (yyloc).first_line; (yyval.expr_)->char_number = (yyloc).first_column; }
#line 1672 "Parser.C"
    break;

  case 53: /* ListExpr: %empty  */
#line 225 "Latte.y"
                       { (yyval.listexpr_) = new ListExpr(); }
#line 1678 "Parser.C"
    break;

  case 54: /* ListExpr: Expr  */
#line 226 "Latte.y"
         { (yyval.listexpr_) = new ListExpr(); (yyval.listexpr_)->push_back((yyvsp[0].expr_)); }
#line 1684 "Parser.C"
    break;

  case 55: /* ListExpr: Expr _COMMA ListExpr  */
#line 227 "Latte.y"
                         { (yyvsp[0].listexpr_)->push_back((yyvsp[-2].expr_)); (yyval.listexpr_) = (yyvsp[0].listexpr_); }
#line 1690 "Parser.C"
    break;

  case 56: /* AddOp: _PLUS  */
#line 229 "Latte.y"
              { (yyval.addop_) = new Plus(); (yyval.addop_)->line_number = (yyloc).first_line; (yyval.addop_)->char_number = (yyloc).first_column; }
#line 1696 "Parser.C"
    break;

  case 57: /* AddOp: _MINUS  */
#line 230 "Latte.y"
           { (yyval.addop_) = new Minus(); (yyval.addop_)->line_number = (yyloc).first_line; (yyval.addop_)->char_number = (yyloc).first_column; }
#line 1702 "Parser.C"
    break;

  case 58: /* MulOp: _STAR  */
#line 232 "Latte.y"
              { (yyval.mulop_) = new Times(); (yyval.mulop_)->line_number = (yyloc).first_line; (yyval.mulop_)->char_number = (yyloc).first_column; }
#line 1708 "Parser.C"
    break;

  case 59: /* MulOp: _SLASH  */
#line 233 "Latte.y"
           { (yyval.mulop_) = new Div(); (yyval.mulop_)->line_number = (yyloc).first_line; (yyval.mulop_)->char_number = (yyloc).first_column; }
#line 1714 "Parser.C"
    break;

  case 60: /* MulOp: _PERCENT  */
#line 234 "Latte.y"
             { (yyval.mulop_) = new Mod(); (yyval.mulop_)->line_number = (yyloc).first_line; (yyval.mulop_)->char_number = (yyloc).first_column; }
#line 1720 "Parser.C"
    break;

  case 61: /* RelOp: _LT  */
#line 236 "Latte.y"
            { (yyval.relop_) = new LTH(); (yyval.relop_)->line_number = (yyloc).first_line; (yyval.relop_)->char_number = (yyloc).first_column; }
#line 1726 "Parser.C"
    break;

  case 62: /* RelOp: _LDARROW  */
#line 237 "Latte.y"
             { (yyval.relop_) = new LE(); (yyval.relop_)->line_number = (yyloc).first_line; (yyval.relop_)->char_number = (yyloc).first_column; }
#line 1732 "Parser.C"
    break;

  case 63: /* RelOp: _GT  */
#line 238 "Latte.y"
        { (yyval.relop_) = new GTH(); (yyval.relop_)->line_number = (yyloc).first_line; (yyval.relop_)->char_number = (yyloc).first_column; }
#line 1738 "Parser.C"
    break;

  case 64: /* RelOp: _GTEQ  */
#line 239 "Latte.y"
          { (yyval.relop_) = new GE(); (yyval.relop_)->line_number = (yyloc).first_line; (yyval.relop_)->char_number = (yyloc).first_column; }
#line 1744 "Parser.C"
    break;

  case 65: /* RelOp: _DEQ  */
#line 240 "Latte.y"
         { (yyval.relop_) = new EQU(); (yyval.relop_)->line_number = (yyloc).first_line; (yyval.relop_)->char_number = (yyloc).first_column; }
#line 1750 "Parser.C"
    break;

  case 66: /* RelOp: _BANGEQ  */
#line 241 "Latte.y"
            { (yyval.relop_) = new NE(); (yyval.relop_)->line_number = (yyloc).first_line; (yyval.relop_)->char_number = (yyloc).first_column; }
#line 1756 "Parser.C"
    break;


#line 1760 "Parser.C"

      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", YY_CAST (yysymbol_kind_t, yyr1[yyn]), &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;

  *++yyvsp = yyval;
  *++yylsp = yyloc;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */
  {
    const int yylhs = yyr1[yyn] - YYNTOKENS;
    const int yyi = yypgoto[yylhs] + *yyssp;
    yystate = (0 <= yyi && yyi <= YYLAST && yycheck[yyi] == *yyssp
               ? yytable[yyi]
               : yydefgoto[yylhs]);
  }

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYSYMBOL_YYEMPTY : YYTRANSLATE (yychar);
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
      yyerror (&yylloc, scanner, result, YY_("syntax error"));
    }

  yyerror_range[1] = yylloc;
  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval, &yylloc, scanner, result);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:
  /* Pacify compilers when the user code never invokes YYERROR and the
     label yyerrorlab therefore never appears in user code.  */
  if (0)
    YYERROR;
  ++yynerrs;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  /* Pop stack until we find a state that shifts the error token.  */
  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYSYMBOL_YYerror;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYSYMBOL_YYerror)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;

      yyerror_range[1] = *yylsp;
      yydestruct ("Error: popping",
                  YY_ACCESSING_SYMBOL (yystate), yyvsp, yylsp, scanner, result);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  yyerror_range[2] = yylloc;
  ++yylsp;
  YYLLOC_DEFAULT (*yylsp, yyerror_range, 2);

  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", YY_ACCESSING_SYMBOL (yyn), yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturnlab;


/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturnlab;


/*-----------------------------------------------------------.
| yyexhaustedlab -- YYNOMEM (memory exhaustion) comes here.  |
`-----------------------------------------------------------*/
yyexhaustedlab:
  yyerror (&yylloc, scanner, result, YY_("memory exhausted"));
  yyresult = 2;
  goto yyreturnlab;


/*----------------------------------------------------------.
| yyreturnlab -- parsing is finished, clean up and return.  |
`----------------------------------------------------------*/
yyreturnlab:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval, &yylloc, scanner, result);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  YY_ACCESSING_SYMBOL (+*yyssp), yyvsp, yylsp, scanner, result);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif

  return yyresult;
}

#line 244 "Latte.y"



/* Entrypoint: parse Program* from file. */
Program* pProgram(FILE *inp)
{
  YYSTYPE result;
  yyscan_t scanner = latte__initialize_lexer(inp);
  if (!scanner) {
    fprintf(stderr, "Failed to initialize lexer.\n");
    return 0;
  }
  int error = yyparse(scanner, &result);
  latte_lex_destroy(scanner);
  if (error)
  { /* Failure */
    return 0;
  }
  else
  { /* Success */
    return result.program_;
  }
}

/* Entrypoint: parse Program* from string. */
Program* psProgram(const char *str)
{
  YYSTYPE result;
  yyscan_t scanner = latte__initialize_lexer(0);
  if (!scanner) {
    fprintf(stderr, "Failed to initialize lexer.\n");
    return 0;
  }
  YY_BUFFER_STATE buf = latte__scan_string(str, scanner);
  int error = yyparse(scanner, &result);
  latte__delete_buffer(buf, scanner);
  latte_lex_destroy(scanner);
  if (error)
  { /* Failure */
    return 0;
  }
  else
  { /* Success */
    return result.program_;
  }
}



