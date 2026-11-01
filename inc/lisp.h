#pragma once
#include <stdbool.h>

struct LObj;
typedef struct LObj LObj;

enum LObjVariants { TOKEN, CONS, INT, FUNCTION, ERR, STR, NIL, EXT }; // nil is nullptr

enum ErrorCode { ER_NONE = 0, ER_RUN, ER_INT, ER_SYNTAX, ER_TYPE, ER_CODED = (unsigned)-1 };

#ifndef NULL
#define NULL ((void*)0)
#endif

#ifndef uint
#define uint unsigned int
#endif

#ifndef PURE
#define PURE __attribute__((const))
#endif

// call when returning LObj* not obtained from a function
LObj* clone(LObj*);
// call when a LObj* recieved from a function isn't returned
void drop(LObj*);
void cleanup(LObj**); // for convenience with __attribute__((cleanup))

void dealloc(void); // call on program end, after dropping all active LObj*'s

LObj* nil();
LObj* integ(long);
LObj* err(enum ErrorCode code, const char* blame, const char* msg);
LObj* err_owned(enum ErrorCode code, const char* blame, char* msg);
LObj* err_fmt(enum ErrorCode code, const char* blame, const char* msg, ...);

LObj* tok(const char*);
LObj* tok_owned(char*);
LObj* str(const char*);
LObj* str_owned(char*);
LObj* str_sized(char*, unsigned int);
LObj* func(LObj* (*f)(LObj*));
LObj* func_owned(LObj* (*f)(LObj*), unsigned int size);
LObj* _cons(LObj*, LObj*);
LObj* unowned_cons(LObj*, LObj*);
LObj* ext(int type, void*, void (*cleanup)(void*), uint (*print)(void*, char*, uint));
LObj* ext_static(int type, void*, uint (*print)(void*, char*, uint));

PURE enum LObjVariants variant(LObj*);
PURE bool is_nil(LObj*);
PURE bool is_err(LObj*);
PURE bool is_cons(LObj*);
PURE bool is_func(LObj*);
PURE bool is_callable(LObj*);
PURE bool is_int(LObj*);
PURE bool is_str(LObj*);
PURE bool is_owned(LObj*);
PURE bool is_ext(LObj*);

PURE const char* name(LObj*);
PURE int str_len(LObj*);
PURE long int_val(LObj*);
PURE int ext_type(LObj*);
PURE void* ext_ptr(LObj*);
PURE enum ErrorCode code(LObj*);

LObj* _car(LObj*);
LObj* _cdr(LObj*);
PURE LObj* car_unowned(LObj*);
PURE LObj* cdr_unowned(LObj*);
LObj* If(LObj*);
LObj* _apply(LObj*, LObj*);

unsigned int tostring(LObj*, char*, unsigned int cap);
PURE bool eq(LObj*, LObj*);

#define try(expr) ({ \
	LObj* __try_internal = expr; \
	if (is_err(__try_internal)) return __try_internal; \
	__try_internal; \
})

#define foreach(name, obj, body) ({ LObj* __i; for (__i = obj; is_cons(__i); __i = cdr_unowned(__i)) { LObj* name = car_unowned(__i); body } __i; })

#ifndef CLEANUP
#define CLEANUP __attribute__((cleanup(cleanup)))
#endif
