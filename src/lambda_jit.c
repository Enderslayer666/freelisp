// note : THIS FILE WAS WRITTEN BY ARTIFICIAL INTELLIGENCE
// lambda_jit.c -- x86-64 SysV only.
//
// Drop this into prelude.c (replacing the lambda stub), *after* eval_args.
// It also needs the lisp.c change to drop() shown in the notes (munmap + release body).
//
// (lambda (x y) body)  ==>  a FUNCTION whose machine code:
//   0. parameters may end in a rest parameter: (lambda (a b . rest) body), or
//      (lambda args body) for "everything".  `rest` is bound to the list of the
//      remaining arguments (nil if there are none)
//   1. evals its (unevaluated) argument list, checks arity
//      (lambda-macro skips this step and substitutes the raw argument forms)
//   2. builds a copy of `body` with x / y replaced by the argument values,
//      sharing every subtree that mentions no parameter
//   3. Evals that result and returns it (LAMBDA_EVAL_RESULT=0 to skip)

#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/mman.h>
#include "../inc/lisp.h"
#include "../inc/interp.h"

// 1 (default): the generated function Evals the substituted tree before returning it
//              (params are wrapped in (quote ...) so their values aren't evaluated twice)
// 0:           return the substituted tree unevaluated (template / macro semantics)
#ifndef LAMBDA_EVAL_RESULT
#define LAMBDA_EVAL_RESULT 1
#endif

typedef LObj* (*LFn)(LObj*);

/* ------------------------------------------------------------------ */
/* Runtime helpers that the generated code calls (ordinary C ABI)      */
/* ------------------------------------------------------------------ */

// _cons clones its arguments, so give up our own references afterwards.
static LObj* cons_take(LObj* a, LObj* b) {
	LObj* c = _cons(a, b);
	drop(a); drop(b);
	return c;
}

static LObj* quote_take(LObj* v) {           // v -> (quote v), consumes v
	return cons_take(tok("quote"), cons_take(v, NULL));
}

static LObj* eval_take(LObj* tree) {         // Eval and release the tree
	LObj* r = Eval(tree);
	drop(tree);
	return r;
}

// Park an unowned pointer to each of the first n list elements in slots[]; NULL on success.
// rest=0: the list must have exactly n elements.
// rest=1: slots[n] gets the remaining tail of the list (nil if empty), so slots[] needs n+1 entries.
static LObj* unpack(LObj* list, long n, LObj** slots, int rest) {
	LObj* p = list;
	for (long i = 0; i < n; i++) {
		if (!is_cons(p)) return err(ER_SYNTAX, "[anon]", "too few arguments");
		slots[i] = car_unowned(p);
		p = cdr_unowned(p);
	}
	if (rest) { slots[n] = p; return NULL; }
	return is_nil(p) ? NULL : err(ER_SYNTAX, "[anon]", "too many arguments");
}

// lambda: evaluate the args first. Returns NULL on success (and *ev_out = the
// evaluated list, which the generated code drops at the end), or an ERR object.

static LObj* eval_args(LObj* args, const char* msg) {
	if (is_nil(args)) return clone(args);
	if (!is_cons(args)) return err(ER_SYNTAX, "lambda", msg);
	LObj* head = car_unowned(args);
	LObj* tail = cdr_unowned(args);
	LObj* h = Eval(head);
	LObj* t = eval_args(tail, msg);
	LObj* ret = _cons(h, t);
	drop(h); drop(t); return ret;
}
static LObj* jit_enter(LObj* args, long n, LObj** slots, LObj** ev_out, int rest) {
	LObj* ev = eval_args(args, "bad args");
	if (is_err(ev)) return ev;
	LObj* e = unpack(ev, n, slots, rest);
	if (e) { drop(ev); return e; }
	*ev_out = ev;
	return NULL;
}

// lambda-macro: same, but the raw (unevaluated) argument forms are used.
// We hold our own reference to the list so the epilogue's drop stays uniform.
static LObj* jit_enter_raw(LObj* args, long n, LObj** slots, LObj** ev_out, int rest) {
	LObj* e = unpack(args, n, slots, rest);
	if (e) return e;
	*ev_out = clone(args);
	return NULL;
}

/* ------------------------------------------------------------------ */
/* Tiny x86-64 emitter                                                 */
/* ------------------------------------------------------------------ */

struct code { uint8_t* buf; size_t len, cap; };

static void put(struct code* c, const void* p, size_t n) {
	if (c->len + n > c->cap) {
		if (!c->cap) c->cap = 256;
		while (c->cap < c->len + n) c->cap *= 2;
		c->buf = realloc(c->buf, c->cap);
	}
	memcpy(c->buf + c->len, p, n);
	c->len += n;
}
#define BYTES(c, ...) do { const uint8_t b_[] = { __VA_ARGS__ }; put((c), b_, sizeof b_); } while (0)
static void imm32(struct code* c, int32_t v)  { put(c, &v, 4); }
static void imm64(struct code* c, uint64_t v) { put(c, &v, 8); }
static void patch32(struct code* c, size_t at, int32_t v) { memcpy(c->buf + at, &v, 4); }

static void call_abs(struct code* c, void* fn) {
	BYTES(c, 0x48, 0xB8); imm64(c, (uintptr_t)fn);   // mov rax, fn
	BYTES(c, 0xFF, 0xD0);                            // call rax
}
static void load_rdi_slot(struct code* c, int slot) { BYTES(c, 0x48, 0x8B, 0xBC, 0x24); imm32(c, slot * 8); } // mov rdi,[rsp+8*slot]
static void store_slot(struct code* c, int slot)    { BYTES(c, 0x48, 0x89, 0x84, 0x24); imm32(c, slot * 8); } // mov [rsp+8*slot],rax
#define MOV_RDI_RAX(c) BYTES(c, 0x48, 0x89, 0xC7)
#define MOV_RSI_RAX(c) BYTES(c, 0x48, 0x89, 0xC6)

/* ------------------------------------------------------------------ */
/* Template compiler                                                   */
/*                                                                     */
/* Frame layout (relative to rsp after the prologue), in 8-byte slots: */
/*   [0]            evaluated arg list (for the final drop)            */
/*   [1 .. N]       unowned pointer to each parameter's value; N is    */
/*                  the fixed count, +1 if there is a rest parameter   */
/*   [N+1 ..]       temporaries, one per nesting depth of the build    */
/* ------------------------------------------------------------------ */

struct jit {
	struct code code;
	const char** params;
	long nparams;
	long max_temp;
	bool quote_params;
};

static long param_index(struct jit* j, LObj* n) {
	if (variant(n) != TOKEN) return -1;
	const char* s = name(n);
	for (long i = 0; i < j->nparams; i++)
		if (strcmp(s, j->params[i]) == 0) return i;
	return -1;
}

static bool uses_param(struct jit* j, LObj* n) {
	if (is_cons(n)) return uses_param(j, car_unowned(n)) || uses_param(j, cdr_unowned(n));
	return param_index(j, n) >= 0;
}

// Emit code that leaves a NEW owned reference to the substituted `n` in rax.
static void emit(struct jit* j, LObj* n, int depth) {
	struct code* c = &j->code;
	long p = param_index(j, n);

	if (p >= 0) {                                    // parameter
		load_rdi_slot(c, 1 + p);
		call_abs(c, clone);
		if (j->quote_params) { MOV_RDI_RAX(c); call_abs(c, quote_take); }
	} else if (!uses_param(j, n)) {                  // constant subtree: share it
		if (is_nil(n)) BYTES(c, 0x31, 0xC0);         // xor eax, eax
		else { BYTES(c, 0x48, 0xBF); imm64(c, (uintptr_t)n); call_abs(c, clone); } // mov rdi, n
	} else {                                         // cons with a parameter somewhere below
		int slot = 1 + (int)j->nparams + depth;
		if (depth + 1 > j->max_temp) j->max_temp = depth + 1;
		emit(j, car_unowned(n), depth);
		store_slot(c, slot);
		emit(j, cdr_unowned(n), depth + 1);
		MOV_RSI_RAX(c);
		load_rdi_slot(c, slot);
		call_abs(c, cons_take);
	}
}

// macro=false: evaluate args, substitute their values (quoted, if the result is Eval'd)
// macro=true:  substitute the raw argument forms; they get evaluated where they land
// params holds n fixed names, followed by the rest-parameter name if `rest` is set.
static LObj* compile_lambda(const char** params, long n, bool rest, LObj* body, bool macro) {
	struct jit j = { .params = params, .nparams = n + (rest ? 1 : 0), .quote_params = LAMBDA_EVAL_RESULT && !macro };
	struct code* c = &j.code;

	// prologue. entry rsp = 8 mod 16; push rbx -> 0 mod 16; frame is a multiple of 16.
	BYTES(c, 0x53);                                        // push rbx
	BYTES(c, 0x48, 0x81, 0xEC);                            // sub rsp, imm32
	size_t frame_at = c->len; imm32(c, 0);                 //   (patched below)
	BYTES(c, 0xBE); imm32(c, (int32_t)n);                  // mov esi, n          fixed params (rdi = args, untouched)
	BYTES(c, 0x48, 0x8D, 0x54, 0x24, 0x08);                // lea rdx, [rsp+8]    slots
	BYTES(c, 0x48, 0x89, 0xE1);                            // mov rcx, rsp        ev_out
	BYTES(c, 0x41, 0xB8); imm32(c, rest ? 1 : 0);          // mov r8d, rest       has rest param
	call_abs(c, macro ? (void*)jit_enter_raw : (void*)jit_enter);
	BYTES(c, 0x48, 0x85, 0xC0);                            // test rax, rax
	BYTES(c, 0x0F, 0x85);                                  // jnz .leave (rax = error)
	size_t fail_at = c->len; imm32(c, 0);

	emit(&j, body, 0);
#if LAMBDA_EVAL_RESULT
	MOV_RDI_RAX(c); call_abs(c, eval_take);
#endif

	BYTES(c, 0x48, 0x89, 0xC3);                            // mov rbx, rax        save result
	BYTES(c, 0x48, 0x8B, 0x3C, 0x24);                      // mov rdi, [rsp]      evaluated arg list
	call_abs(c, drop);
	BYTES(c, 0x48, 0x89, 0xD8);                            // mov rax, rbx

	// .leave:
	patch32(c, fail_at, (int32_t)(c->len - (fail_at + 4)));
	long words = 1 + j.nparams + j.max_temp;
	if (words & 1) words++;
	int32_t frame = (int32_t)(words * 8);
	patch32(c, frame_at, frame);
	BYTES(c, 0x48, 0x81, 0xC4); imm32(c, frame);           // add rsp, frame
	BYTES(c, 0x5B);                                        // pop rbx
	BYTES(c, 0xC3);                                        // ret

	// Copy into its own page(s): write first, then flip to R+X (never RWX).
	// The last word of the mapping owns a reference to `body` so the constant
	// subtrees baked into the code stay alive.
	long pg = sysconf(_SC_PAGESIZE);
	size_t size = (c->len + sizeof(LObj*) + pg - 1) & ~(size_t)(pg - 1);
	uint8_t* mem = mmap(NULL, size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
	if (mem == MAP_FAILED) { free(c->buf); return err(-errno, "lambda", "mmap failed"); }
	memcpy(mem, c->buf, c->len);
	*(LObj**)(mem + size - sizeof(LObj*)) = clone(body);
	size_t codelen = c->len;
	free(c->buf);
	if (mprotect(mem, size, PROT_READ | PROT_EXEC) != 0) {
		drop(body); munmap(mem, size);
		return err(-errno, "lambda", "mprotect failed");
	}
	__builtin___clear_cache((char*)mem, (char*)mem + codelen);   // no-op on x86, needed on ARM
	return func_owned((LFn)mem, (long)size);
}

// The parser has no dotted-pair syntax: `(a b . c)` reads as the proper list (a b "." c).
// So a bare "." token marks the rest parameter. A genuinely improper list, or a bare token
// as the whole parameter list, are accepted too.
static bool is_dot(LObj* o) { return variant(o) == TOKEN && strcmp(name(o), ".") == 0; }

static LObj* make_lambda(LObj* args, bool macro) {
	if (!is_cons(args)) return err(ER_SYNTAX, "lambda", "expected two arguments; got none");
	LObj* plist = car_unowned(args);
	LObj* rest_node = cdr_unowned(args);
	if (!is_cons(rest_node)) return err(ER_SYNTAX, "lambda", "expected two arguments; got one");
	if (!is_nil(cdr_unowned(rest_node))) return err(ER_SYNTAX, "lambda", "expected two arguments; got more");
	LObj* body = car_unowned(rest_node);

	// Pass 1: validate; count fixed params; find the rest parameter, if any.
	long n = 0;
	const char* rest_name = NULL;
	LObj* p = plist;
	for (; is_cons(p); p = cdr_unowned(p)) {
		LObj* pn = car_unowned(p);
		if (is_dot(pn)) {
			LObj* after = cdr_unowned(p);
			if (!is_cons(after)) return err(ER_SYNTAX, "lambda", "expected one tail parameter after `.`; got none");
			if (!is_nil(cdr_unowned(after))) return err(ER_SYNTAX, "lambda", "expected one tail parameter after `.`; got more");
			if (variant(car_unowned(after)) != TOKEN) return err(ER_SYNTAX, "lambda", "expected named tail parameter after `.`; got other type");
			if (is_dot(car_unowned(after))) return err(ER_SYNTAX, "lambda", "expected at most one tail (.); got more");
			rest_name = name(car_unowned(after));
			p = NULL;
			break;
		}
		if (variant(pn) != TOKEN) return err(ER_TYPE, "lambda", "expected token parameter names; got other types");
		n++;
	}
	if (!rest_name && !is_nil(p)) {                  // improper tail: (a b . c) or bare `c`
		if (variant(p) != TOKEN) return err(ER_TYPE, "lambda", "expected token parameter names; got other types");
		rest_name = name(p);
	}

	// Pass 2: collect the names.
	const char** names = malloc((n + 1) * sizeof *names);
	long i = 0;
	for (p = plist; i < n; p = cdr_unowned(p)) names[i++] = name(car_unowned(p));
	if (rest_name) names[n] = rest_name;

	LObj* fn = compile_lambda(names, n, rest_name != NULL, body, macro);
	free(names);
	return fn;
}

LObj* lambda(LObj* args) { return make_lambda(args, false); }

LObj* lambda_macro(LObj*) __asm__("\"lambda-macro\"");
LObj* lambda_macro(LObj* args) { return make_lambda(args, true); }
