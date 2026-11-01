#include "lisp.h"

LObj* Eval(LObj*);
void add_def_owned(char* key, LObj* val);
void add_def_unowned(const char* key, LObj* val);

LObj* eval(LObj*);
LObj* define(LObj*);
LObj* import(LObj*);
LObj* quit(LObj*);
