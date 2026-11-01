#include <sys/mman.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <pthread.h>
#include <stdarg.h>
#include "../inc/lisp.h"

#ifndef uint
#define uint unsigned int
#endif

#define POOL_SLAB_BLOCKS (64 * 1024 - 1)
 // 2^n-1, to ensure sizeof(struct pool_slab)
 // is just under a clean number of pages

#define RET(v) { return (v); }

struct LObjCons {
	LObj* car;
	LObj* cdr;
};

struct Ptr { void* addr; bool owned; uint size; }; // 16 B
struct Unknown { void* value; void (*cleanup)(void*); uint (*print)(void*, char*, uint); int type; }; // 28 B
struct Error { const char* blame; char* msg; enum ErrorCode code; bool owned; }; // 21 B

union LObjUnion {
	long as_int;
	struct Unknown as_ptr;
	LObj* (*as_func)(LObj*);
	char* as_token;
	struct LObjCons as_cons;
	struct Ptr as_flagged;
	struct Error as_err;
}; // 28 B - align: 32 B

struct LObj {
	_Atomic int refs;
	enum LObjVariants variant;
	union LObjUnion value; // 36 B - align: 40 B
};

union pool_block {
	LObj data;
	void* next_free;
}; // 48 B

struct pool_slab {
	struct pool_slab* next;
	union pool_block blocks[POOL_SLAB_BLOCKS]; // ~ 4 MB
};

struct pool {
	pthread_mutex_t lock;
	struct pool_slab* slabs;
	void* free_list;
};

static void pool_grow(struct pool* p) {
	struct pool_slab* s = mmap(NULL, sizeof(struct pool_slab), PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, -1, 0);
	if (s == MAP_FAILED) { perror("\nFATAL: out of memory"); exit(0); }
	for (int i=POOL_SLAB_BLOCKS-1; i>0; i--) s->blocks[i-1].next_free = &s->blocks[i];
	s->blocks[POOL_SLAB_BLOCKS-1].next_free = p->free_list;
	s->next = p->slabs;
	p->slabs = s;
	p->free_list = s->blocks;
}

static inline void* pool_alloc(struct pool* p) {
	pthread_mutex_lock(&p->lock);
	if (p->free_list == NULL) pool_grow(p);
	void* ret = p->free_list;
	p->free_list = *(void**)ret;
	pthread_mutex_unlock(&p->lock);
	return ret;
}

static inline void pool_free(struct pool* p, void* ptr) {
	pthread_mutex_lock(&p->lock);
	*(void**)ptr = p->free_list;
	p->free_list = ptr;
	pthread_mutex_unlock(&p->lock);
}

static void pool_drop(struct pool* p) {
	for (struct pool_slab* s = p->slabs; s!=NULL;) {
		struct pool_slab* n = s->next;
		munmap(s, sizeof(struct pool_slab));
		s = n;
	}
}

struct pool ALLOC = {0};

void dealloc(void) { pool_drop(&ALLOC); }

static LObj* new(enum LObjVariants variant, union LObjUnion value) {
	LObj* obj = pool_alloc(&ALLOC);
	*obj = (LObj){1, variant, value};
	return obj;
}
// call when a LObj* recieved from a function isn't in return
void drop(LObj* obj) {
	if (obj!=NULL && __atomic_fetch_sub(&obj->refs, 1, __ATOMIC_ACQ_REL)==1) {
		switch (variant(obj)) {
		case CONS:
			drop(obj->value.as_cons.car);
			drop(obj->value.as_cons.cdr);
			break;
		case EXT:
			obj->value.as_ptr.cleanup(obj->value.as_ptr.value);
			break;
		case TOKEN: case STR:
			if (obj->value.as_flagged.owned) free(obj->value.as_flagged.addr);
			break;
		case ERR:
			if (obj->value.as_err.owned) free(obj->value.as_err.msg);
			break;
		case FUNCTION:
			if (obj->value.as_flagged.owned) {
				struct Ptr* ptr = &obj->value.as_flagged;
				drop(*(LObj**)((char*)ptr->addr + ptr->size - sizeof(LObj*)));
				munmap(ptr->addr, ptr->size);
			} break;
		case NIL: case INT: break;
		}
		pool_free(&ALLOC, obj);
	}
}

void cleanup(LObj** obj) { drop(*obj); }
void no_cleanup(void*) {}

// call when returning a LObj* not obtained from a function
LObj* clone(LObj* obj) { if (obj!=NULL) __atomic_fetch_add(&obj->refs, 1, __ATOMIC_RELAXED); return obj; }

LObj* nil() RET(NULL)
LObj* integ(long i) RET(new(INT, (union LObjUnion){.as_int = i}))
LObj* err(enum ErrorCode code, const char* blame, const char* msg) RET(new(ERR, (union LObjUnion){ .as_err = (struct Error){ .blame = blame, .msg = (char*)msg, .code = code } }))
LObj* err_owned(enum ErrorCode code, const char* blame, char* msg) RET(new(ERR, (union LObjUnion){ .as_err = (struct Error){ .blame = blame, .msg = msg, .code = code, .owned = true } }))
LObj* _cons(LObj* car, LObj* cdr) RET(new(CONS, (union LObjUnion){ .as_cons = (struct LObjCons){ .car = clone(car), .cdr = clone(cdr) }}))
LObj* unowned_cons(LObj* car, LObj* cdr) RET(new(CONS, (union LObjUnion){ .as_cons = (struct LObjCons){ .car = car, .cdr = cdr }}))
LObj* tok(const char* name) RET(new(TOKEN, (union LObjUnion){ .as_flagged = (struct Ptr){(char*)name} }))
LObj* tok_owned(char* name) RET(new(TOKEN, (union LObjUnion){ .as_flagged = (struct Ptr){name, true} }))
LObj* str(const char* name) RET(new(STR, (union LObjUnion){ .as_flagged = (struct Ptr){ (char*)name, false, strlen(name) } }))
LObj* str_owned(char* name) RET(new(STR, (union LObjUnion){ .as_flagged = (struct Ptr){ name, true,  strlen(name) } }))
LObj* str_sized(char* name, uint size) RET(new(STR, (union LObjUnion){ .as_flagged = (struct Ptr){ name, true,  size } }))
LObj* func(LObj* (*f)(LObj*)) RET(new(FUNCTION, (union LObjUnion){ .as_flagged = (struct Ptr){f} }))
LObj* func_owned(LObj* (*f)(LObj*), uint size) RET(new(FUNCTION, (union LObjUnion){ .as_flagged = (struct Ptr){f, true, size} }))
LObj* ext(int type, void* value, void (*cleanup)(void*), uint (*print)(void*, char*, uint)) RET(new(EXT, (union LObjUnion){ .as_ptr = (struct Unknown){ .value = value, .cleanup = cleanup, .print = print, .type = type }}))
LObj* ext_static(int type, void* value, uint (*print)(void*, char*, uint)) RET(new(EXT, (union LObjUnion){ .as_ptr = (struct Unknown){ .value = value, .cleanup = no_cleanup, .print = print, .type = type }}))

LObj* err_fmt(enum ErrorCode code, const char* blame, const char* fmt, ...) {
	va_list args;
	va_start(args, fmt);
	int len = vsnprintf(NULL, 0, fmt, args);
	char* out = malloc(len+1);
	va_start(args, fmt);
	vsnprintf(out, len+1, fmt, args);
	return err_owned(code, blame, out);
}

enum LObjVariants variant(LObj* obj) RET(is_nil(obj) ? NIL : obj->variant)
bool is_nil(LObj* obj) RET(obj == NULL)
bool is_err(LObj* obj) RET(!is_nil(obj) && obj->variant == ERR)
bool is_cons(LObj* obj) RET(!is_nil(obj) && obj->variant == CONS)
bool is_func(LObj* obj) RET(!is_nil(obj) && obj->variant == FUNCTION)
bool is_callable(LObj* obj) RET(!is_nil(obj) && obj->variant == FUNCTION)
bool is_int(LObj* obj) RET(!is_nil(obj) && obj->variant == INT)
bool is_str(LObj* obj) RET(!is_nil(obj) && obj->variant == STR)
bool is_owned(LObj* obj) {
	const enum LObjVariants v = variant(obj);
	return (v==TOKEN || v==ERR || v==FUNCTION || v==STR) && obj->value.as_flagged.owned;
}
bool is_ext(LObj* obj) RET(!is_nil(obj) && obj->variant == EXT)

const char* name(LObj* obj) {
	const enum LObjVariants v = variant(obj);
 	return (v==TOKEN || v==STR)
		? obj->value.as_token
		: NULL;
}
int str_len(LObj* obj) RET(variant(obj) == STR ? obj->value.as_flagged.size : (unsigned)-1)
long int_val(LObj* obj) RET(obj->value.as_int)
int ext_type(LObj* obj) RET(variant(obj) == EXT ? obj->value.as_ptr.type : 0)
void* ext_ptr(LObj* obj) RET(variant(obj) == EXT ? obj->value.as_ptr.value : NULL)
enum ErrorCode code(LObj* obj) RET(variant(obj) == ERR ? obj->value.as_err.code : ER_NONE)

LObj* _car(LObj* obj) RET(is_cons(obj) ? clone(obj->value.as_cons.car) : err(ER_TYPE, "car", "expected cons"))
LObj* _cdr(LObj* obj) RET(is_cons(obj) ? clone(obj->value.as_cons.cdr) : err(ER_TYPE, "cdr", "expected cons"))

LObj* car_unowned(LObj* obj) RET(is_cons(obj) ? obj->value.as_cons.car : err(ER_TYPE, "car", "expected cons"))
LObj* cdr_unowned(LObj* obj) RET(is_cons(obj) ? obj->value.as_cons.cdr : err(ER_TYPE, "cdr", "expected cons"))

LObj* _apply(LObj* func, LObj* args) RET(is_func(func) ? func->value.as_func(args) : is_err(func) ? clone(func) : unowned_cons(err_fmt(ER_TYPE, "APPLY", "expected callable; got %s", name(func) ? name(func) : "(...)"), clone(func)))

// precond: CONS
static uint constostring(LObj* obj, char* out, uint cap) {
	uint x = tostring(car_unowned(obj), out, cap);
	LObj* tail = cdr_unowned(obj);
	if (is_nil(tail)) return x + snprintf(out+x, x<cap ? cap-x : 0, ")");
	else if (is_cons(tail)) {
		x += snprintf(out+x, x < cap ? cap-x : 0, " ");
		return x + constostring(tail, out+x, x<cap ? cap-x : 0);
	} else {
		x += snprintf(out+x, x<cap ? cap-x : 0, " . ");
		x += tostring(tail, out+x, x<cap ? cap-x : 0);
		return x + snprintf(out+x, x<cap ? cap-x : 0, ")");
	}
}

uint tostring(LObj* obj, char* out, uint cap) {
	switch (variant(obj)) {
		case NIL: return snprintf(out, cap, "()");
		case TOKEN: return snprintf(out, cap, "%s", obj->value.as_token);
		case STR: return snprintf(out, cap, "%.*s", obj->value.as_flagged.size, obj->value.as_token);
		case ERR: return snprintf(out, cap, "ERROR %d: in %s : %s", obj->value.as_err.code, obj->value.as_err.blame, obj->value.as_err.msg);
		case EXT: if (obj->value.as_ptr.print == NULL) return snprintf(out, cap, "ptr:%p", obj->value.as_ptr.value);
			else return obj->value.as_ptr.print(obj->value.as_ptr.value, out, cap);
		case INT: return snprintf(out, cap, "%ld", obj->value.as_int);
		case FUNCTION: return snprintf(out, cap, "func:%p", obj->value.as_func);
		case CONS: {
			uint x = snprintf(out, cap, "(");
			return x + constostring(obj, out+x, x<cap ? cap-x : 0);
		}
	}
	return -1;
}

bool eq(LObj* a, LObj* b) {
	if (variant(a) != variant(b)) return false;
	switch (variant(a)) {
		case NIL: return true;
		case INT: return a->value.as_int == b->value.as_int;
		case FUNCTION: return a->value.as_ptr.value == b->value.as_ptr.value;
		case EXT: return a->value.as_ptr.value == b->value.as_ptr.value && a->value.as_ptr.type == b->value.as_ptr.type;
		case TOKEN: case ERR: return strcmp(a->value.as_token, b->value.as_token)==0;
		case STR: return a->value.as_flagged.size == b->value.as_flagged.size && memcmp(a->value.as_token, b->value.as_token, a->value.as_flagged.size)==0;
		case CONS: return eq(car_unowned(a), car_unowned(b)) && eq(cdr_unowned(a), cdr_unowned(b));
	}
	return false;
}
