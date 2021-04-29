/****************************************************/
/* File: parse.c                                    */
/* The parser implementation for the TINY compiler  */
/* Compiler Construction: Principles and Practice   */
/* Kenneth C. Louden                                */
/****************************************************/

#include "globals.h"
#include "util.h"
#include "scan.h"
#include "parse.h"
#include "GLOBALS.H"

static TokenType token; /* holds current token */

/* function prototypes for recursive calls */
static TreeNode * stmt_sequence(void);
static TreeNode * statement(void);
static TreeNode * declaration(void);
static TreeNode * if_stmt(void);
static TreeNode * repeat_stmt(void);
static TreeNode * assign_stmt(void);
static TreeNode * read_stmt(void);
static TreeNode * write_stmt(void);
static TreeNode * express(void);
static TreeNode * simple_exp(void);
static TreeNode * term(void);
static TreeNode * factor(void);

static void syntaxError(const char * message)
{ fprintf(listing,"\n>>> ");
    fprintf(listing,"Syntax error at line %d: %s",lineno,message);
    Error = TRUE;
}

static void match(TokenType expected)
{ if (token == expected) token = getToken();
    else {
        syntaxError("unexpected token -> ");
        printToken(token,tokenString);
        fprintf(listing,"      ");
    }
}

TreeNode * stmt_sequence(void)
{ TreeNode * t = statement();
    TreeNode * p = t;
    while ((token!=ENDFILE) && (token!=END) &&
           (token!=ELSE) && (token!=UNTIL)&&(token!=RCURLY))
    { TreeNode * q;
        match(SEMI);
        q = statement();
        if (q!=NULL) {
            if (t==NULL) t = p = q;
            else /* now p cannot be NULL either */
            { p->sibling = q;
                p = q;
            }
        }
    }
    return t;
}

TreeNode * statement(void)
{ TreeNode * t = NULL;
    switch (token) {
        case IF : t = if_stmt(); break;
        case REPEAT : t = repeat_stmt(); break;
        case ID : t = assign_stmt(); break;
        case READ : t = read_stmt(); break;
        case WRITE : t = write_stmt(); break;
        case INT: t = declaration();break;
        case FLOAT: t = declaration();break;
        case VOID: t = declaration();break;
        default : syntaxError("unexpected token -> ");
            printToken(token,tokenString);
            token = getToken();
            break;
    } /* end case */
    return t;
}

TreeNode * declaration(void){
    TreeNode* t = NULL;
    TokenType type = token;
    if(token==INT){
        match(INT);
    }else if(token == FLOAT){
        match(FLOAT);
    }else if(token == VOID){
        match(VOID);
    }
    char* id =  copyString(tokenString);
    match(ID);
    if(token == COMMA||token == SEMI|| token == ASSIGN||token == LBRACKET){
        t = newDeclareNode(VarK);
        t ->attr.op = type;
        TreeNode* p;
        if(token == LBRACKET){
            p = newDeclareNode(ArrayK);
            p->attr.name = id;
            t->child[0] = p;
            match(LBRACKET);
            TreeNode* r =  newExpNode(ConstK);
            r->attr.val =  atoi(tokenString);
            match(NUM);
            p->child[0]=r;
            match(RBRACKET);
            TreeNode* s;
            while(token == LBRACKET){
                match(LBRACKET);
                s =  newExpNode(ConstK);
                s->attr.val =  atoi(tokenString);
                match(NUM);
                r->sibling = s;
                r = s;
                match(RBRACKET);
            }
            if(token == ASSIGN){
                match(ASSIGN);
                match(LCURLY);
                if(token != RCURLY){
                    r = express();
                    p->child[1]=r;
                }
                while(token != RCURLY){
                    match(COMMA);
                    s = express();
                    r->sibling = s;
                    r = s;
                }
                match(RCURLY);
            }
        }else{
            p = newExpNode(IdK);
            p->attr.name = id;
            t ->child[0]=p;
            if(token == ASSIGN){
                match(ASSIGN);
                p->child[0] = express();
            }
        }
        TreeNode* q;
        while(token == COMMA){
            match(COMMA);
            id =  copyString(tokenString);
            match(ID);
            if(token == LBRACKET){
                q = newDeclareNode(ArrayK);
                q->attr.name = id;
                match(LBRACKET);
                TreeNode* r =  newExpNode(ConstK);
                r->attr.val =  atoi(tokenString);
                match(NUM);
                q->child[0]=r;
                match(RBRACKET);
                TreeNode* s;
                while(token == LBRACKET){
                    match(LBRACKET);
                    s =  newExpNode(ConstK);
                    s->attr.val =  atoi(tokenString);
                    match(NUM);
                    r->sibling = s;
                    r = s;
                    match(RBRACKET);
                }
                if(token == ASSIGN){
                    match(ASSIGN);
                    match(LCURLY);
                    if(token != RCURLY){
                        r = express();
                        q->child[1]=r;
                    }
                    while(token != RCURLY){
                        match(COMMA);
                        s = express();
                        r->sibling = s;
                        r = s;
                    }
                    match(RCURLY);
                }
            }else{
                q = newExpNode(IdK);
                q->attr.name = id;
                if(token == ASSIGN){
                    match(ASSIGN);
                    q->child[0] = express();
                }
            }
            p ->sibling = q;
            p = q;
        }
    }else if(token == LPAREN){
        match(LPAREN);
        t = newDeclareNode(FuncK);
        t ->attr.name = id;
        TreeNode* p = newDeclareNode(VarK);
        p ->attr.op = type;
        t ->child[0] = p;
        TreeNode* q;
        if(token != RPAREN){
            p = newDeclareNode(VarK);
            p ->attr.op = token;
            t->child[1] = p;
            if(token == INT){
                match(INT);
            }else if(token == FLOAT){
                match(FLOAT);
            }else if(token == VOID){
                match(VOID);
            }
            q = newExpNode(IdK);
            q ->attr.name = copyString(tokenString);
            match(ID);
            if(token == ASSIGN){
                match(ASSIGN);
                q->child[0] = express();
            }
            p ->child[0] = q;
        }
        while(token != RPAREN){
            match(COMMA);
            q = newDeclareNode(VarK);
            p ->sibling = q;
            p = q;
            p ->attr.op = token;
            if(token == INT){
                match(INT);
            }else if(token == FLOAT){
                match(FLOAT);
            }else if(token == VOID){
                match(VOID);
            }
            q = newExpNode(IdK);
            q ->attr.name = copyString(tokenString);
            match(ID);
            if(token == ASSIGN){
                match(ASSIGN);
                q->child[0] = express();
            }
            p ->child[0] = q;
        }
        match(RPAREN);
        match(LCURLY);
        t->child[2] = stmt_sequence();
        match(RCURLY);
    }
    return t;
}

TreeNode * if_stmt(void)
{ TreeNode * t = newStmtNode(IfK);
    match(IF);
    if (t!=NULL) t->child[0] = express();
    match(THEN);
    if (t!=NULL) t->child[1] = stmt_sequence();
    if (token==ELSE) {
        match(ELSE);
        if (t!=NULL) t->child[2] = stmt_sequence();
    }
    match(END);
    return t;
}

TreeNode * repeat_stmt(void)
{ TreeNode * t = newStmtNode(RepeatK);
    match(REPEAT);
    if (t!=NULL) t->child[0] = stmt_sequence();
    match(UNTIL);
    if (t!=NULL) t->child[1] = express();
    return t;
}

TreeNode * assign_stmt(void)
{ TreeNode * t = newStmtNode(AssignK);
    if ((t!=NULL) && (token==ID))
        t->attr.name = copyString(tokenString);
    match(ID);
    match(ASSIGN);
    if (t!=NULL) t->child[0] = express();
    return t;
}

TreeNode * read_stmt(void)
{ TreeNode * t = newStmtNode(ReadK);
    match(READ);
    if ((t!=NULL) && (token==ID))
        t->attr.name = copyString(tokenString);
    match(ID);
    return t;
}

TreeNode * write_stmt(void)
{ TreeNode * t = newStmtNode(WriteK);
    match(WRITE);
    if (t!=NULL) t->child[0] = express();
    return t;
}

TreeNode * express(void)
{ TreeNode * t = simple_exp();
    if ((token==LT)||(token==EQ)) {
        TreeNode * p = newExpNode(OpK);
        if (p!=NULL) {
            p->child[0] = t;
            p->attr.op = token;
            t = p;
        }
        match(token);
        if (t!=NULL)
            t->child[1] = simple_exp();
    }
    return t;
}

TreeNode * simple_exp(void)
{ TreeNode * t = term();
    while ((token==PLUS)||(token==MINUS))
    { TreeNode * p = newExpNode(OpK);
        if (p!=NULL) {
            p->child[0] = t;
            p->attr.op = token;
            t = p;
            match(token);
            t->child[1] = term();
        }
    }
    return t;
}

TreeNode * term(void)
{ TreeNode * t = factor();
    while ((token==TIMES)||(token==OVER))
    { TreeNode * p = newExpNode(OpK);
        if (p!=NULL) {
            p->child[0] = t;
            p->attr.op = token;
            t = p;
            match(token);
            p->child[1] = factor();
        }
    }
    return t;
}

TreeNode * factor(void)
{ TreeNode * t = NULL;
    char* id = NULL;
    switch (token) {
        case NUM :
            t = newExpNode(ConstK);
            if ((t!=NULL) && (token==NUM))
                t->attr.val = atoi(tokenString);
            match(NUM);
            break;
        case FLOATNUM :
            t = newExpNode(ConstfK);
            if ((t!=NULL) && (token==FLOATNUM))
                t->attr.valf = atof(tokenString);
            match(FLOATNUM);
            break;

        case ID :
            id =  copyString(tokenString);
            match(ID);
            if(token == LBRACKET){
                t = newExpNode(IdArrayK);
                t ->attr.name = id;
                match(LBRACKET);
                TreeNode* r =  express();
                t->child[0]=r;
                match(RBRACKET);
                TreeNode* s;
                while(token == LBRACKET){
                    match(LBRACKET);
                    s =  express();
                    r->sibling = s;
                    r = s;
                    match(RBRACKET);
                }
            }else if(token == LPAREN){
                t = newExpNode(IdFuncK);
                t->attr.name =id;
                match(LPAREN);
                TreeNode* r;
                TreeNode* s;
                if(token!=RPAREN){
                    r = express();
                    t ->child[0] = r;
                }
                while(token!=RPAREN){
                    match(COMMA);
                    s = express();
                    r ->sibling = s;
                    r = s;
                }
                match(RPAREN);
            }else{
                t = newExpNode(IdK);
                t->attr.name =id;
            }
            break;

        case LPAREN :
            match(LPAREN);
            t = express();
            match(RPAREN);
            break;
        default:
            syntaxError("unexpected token -> ");
            printToken(token,tokenString);
            token = getToken();
            break;
    }
    return t;
}

/****************************************/
/* the primary function of the parser   */
/****************************************/
/* Function parse returns the newly
 * constructed syntax tree
 */
TreeNode * parse(void)
{ TreeNode * t;
    token = getToken();
    t = stmt_sequence();
    if (token!=ENDFILE)
        syntaxError("Code ends before file\n");
    return t;
}