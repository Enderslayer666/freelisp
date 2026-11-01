#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <dlfcn.h>
#include <ctype.h>
#include "../inc/lisp.h"
#include "../inc/parse.h"

LObj* eval(LObj*);

struct DefineTree {
	const char* key;
	LObj* val;
	struct DefineTree* left;
	struct DefineTree* right;
	bool owned_key;
};

struct DefineTree root = {
	.key = "nil",
	.owned_key = false,
	.val = NULL,
	.left = NULL,
	.right = NULL
};

struct dls {
	long cap;
	long len;
	void** handles;
};

static struct {
	int argc;
	char** argv;
} Args;

static struct dls dylibs;

static void add_dylib(void* handle) {
	if (dylibs.len == dylibs.cap) {
		dylibs.cap *= 2;
		dylibs.handles = realloc(dylibs.handles, dylibs.cap * sizeof(void*));
	}
	dylibs.handles[dylibs.len++] = handle;
}

LObj* Eval(LObj* obj) {
	switch (variant(obj)) {
	case TOKEN:
		const char* s = name(obj);
		if (*s == '$') {
			int acc = 0;
			for (const char* c = s+1; *c != 0; c++) {
				acc *= 10;
				if (!isdigit(*c)) {
					const char* var = getenv(s+1);
					return var == NULL ? nil() : str(var);
				}
				acc += *c - '0';
			}
			return acc < Args.argc ? str(Args.argv[acc]) : nil();
		}
		struct DefineTree* cur = &root;
		while (cur != NULL) {
			int side = strcmp(s, cur->key);
			if (side == 0) return clone(cur->val);
			cur = side>0 ? cur->right : cur->left;
		}
		return err_fmt(ER_INT, "Eval", "unrecognized identifier: `%s`", s);
	case NIL:
		return nil();
	case CONS:
		LObj* fn = Eval(car_unowned(obj));
		LObj* final = _apply(fn, cdr_unowned(obj));
		drop(fn); return final;
	case ERR:
		fprintf(stderr, "%s", name(obj));
		exit(0);
	default:
		return clone(obj);
	}
}

LObj* eval(LObj* obj) {
	if (!is_cons(obj)) return err(ER_SYNTAX, "eval", "expected argument");
	if (!is_nil(cdr_unowned(obj))) return err(ER_SYNTAX, "eval", "expected one argument, got more");
	LObj* arg = car_unowned(obj);
	LObj* fst = Eval(arg);
	LObj* snd = Eval(fst);
	drop(fst);
	return snd;
}

static void add_def(const char* key, LObj* val, bool owned_key) {
	struct DefineTree* cur = &root;
	struct DefineTree* next = &root;
	int side;
	while (next != NULL) {
		cur = next;
		side = strcmp(key, cur->key);
		if (side == 0) { drop(cur->val); cur->val = val; return; }
		next = side>0 ? cur->right : cur->left;
	}

	struct DefineTree* this = malloc(sizeof(struct DefineTree));
	*this = (struct DefineTree){.key = owned_key ? strdup(key) : key, .owned_key = owned_key, .val = val, .left = NULL, .right = NULL};
	if (side>0) cur->right = this;
	else cur->left = this;
}

void add_def_owned(char* key, LObj* val) { add_def(key, val, true); }
void add_def_unowned(const char* key, LObj* val) { add_def(key, val, false); }

LObj* define(LObj* obj) {
	if (!is_cons(obj)) return err(ER_SYNTAX, "define", "expected name, def");
	LObj* arg1 = car_unowned(obj);

	const char* key = name(arg1);
	if (key == NULL || is_str(arg1)) return err(ER_SYNTAX, "define", "expected name");

	LObj* tail = cdr_unowned(obj);
	if (!is_cons(tail)) return err(ER_SYNTAX, "define", "expected name, def; got name . ???");
	LObj* arg2 = car_unowned(tail);
	if (!is_nil(cdr_unowned(tail))) return err(ER_SYNTAX, "define", "expected name, def; got name, def, ...");
	LObj* val = Eval(arg2);

	if (is_owned(arg1)) add_def_owned(strdup(key), val);
	else add_def_unowned(key, val);
	return nil();
}

LObj* extern_(LObj*) __asm__("extern");
LObj* extern_(LObj* obj) {
	char* error;
	LObj* (*floaded)(LObj*);

	if (!is_cons(obj)) return err(ER_SYNTAX, "extern", "expected two arguments; got none");
	if (!is_cons(cdr_unowned(obj))) return err(ER_SYNTAX, "extern", "expected two arguments; got one");
	if (!is_nil(cdr_unowned(cdr_unowned(obj)))) return err(ER_SYNTAX, "extern", "expected two arguments; got more");

	LObj* arg1 CLEANUP = Eval(car_unowned(obj));
	if (!is_str(arg1)) return is_err(arg1) ? clone(arg1) : err(ER_TYPE, "extern", "expected str path first arg; got other type");
	int path_len = str_len(arg1);
	char path[path_len+1];
	memcpy(path, name(arg1), path_len);
	path[path_len] = '\0';

	LObj* flist CLEANUP = Eval(car_unowned(cdr_unowned(obj)));
	if (!is_cons(flist)) return is_err(flist) ? clone(flist) : err(ER_TYPE, "extern", "expected path, (f1, f2, ...); got path, ???");

	void* handle = dlopen(path, RTLD_LAZY);
	if (handle == NULL) return err(ER_INT, "extern", dlerror());
	add_dylib(handle);

	LObj* end = foreach (fn, flist, {
		const char* sym;
		const char* key;
		bool owned = false;
		if (is_cons(fn)) {
			LObj* fntail = cdr_unowned(fn);
			if (!is_cons(fntail) || !is_nil(cdr_unowned(fntail))) return err(ER_SYNTAX, "extern", "expected path, (f1, (f2sym, f2name), ...); got path, (..., (??\?), ...)");
			sym = name(car_unowned(fn));
			key = name(car_unowned(fntail));
			owned = is_owned(car_unowned(fntail));
		} else { sym = name(fn); key = sym; owned = is_owned(fn); }

		if (sym == NULL || key == NULL) return err(ER_TYPE, "extern", "expected path, (tok f1, (tok f2sym, tok f2name), ...); got other types");
		floaded = dlsym(handle, sym);
		if ((error = dlerror()) != NULL) return err_owned(ER_INT, "extern", strdup(error));
		if (owned) add_def_owned((char*)sym, func(floaded));
		else add_def_unowned(sym, func(floaded));
	});
	if (!is_nil(end)) return err(ER_SYNTAX, "extern", "expected path, (f1, f2, ...); got path, (f1 ... fn . ??\?)");
	return nil();
}

LObj* import(LObj* obj) {
	if (!is_cons(obj)) return err(ER_SYNTAX, "import", "expected one argument; got none");
	LObj* arg CLEANUP = Eval(car_unowned(obj));
	if (!is_str(arg)) return is_err(arg) ? clone(arg) : err(ER_TYPE, "import", "expected str path arg; got other type");
	int path_len = str_len(arg);
	char path[path_len+1];
	memcpy(path, name(arg), path_len);
	path[path_len] = '\0';

	FILE* handle = fopen(path, "r");
	if (handle == NULL) return err(ER_INT, "import", "file unavailable");
	while (!feof(handle)) {
		struct Parsed result = parse_file(handle);
		if (result.result == Err) return result.value;
		else {
			LObj* expr = result.value;
			LObj* final = Eval(expr);
			drop(expr); drop(final);
		}
	}
	fclose(handle);
	return nil();
}

LObj* quote(LObj* obj) {
	return is_cons(obj)
		? is_nil(cdr_unowned(obj))
			? _car(obj)
			: err(ER_SYNTAX, "quote", "expected one argument; got more")
		: is_nil(obj)
			? err(ER_SYNTAX, "quote", "expected one argument; got none")
			: err(ER_SYNTAX, "quote", "bad args");
}

static LObj* _semiquote(LObj* arg) {
	if (!is_cons(arg)) return clone(arg);
	if (variant(car_unowned(arg)) != TOKEN) goto NORMAL;
	const char* str = name(car_unowned(arg));
	if (strcmp(str, "unquote")!=0) goto NORMAL;

	if (is_nil(cdr_unowned(arg))) return err(ER_SYNTAX, ",", "expected one argument; got none");
	if (!is_nil(cdr_unowned(cdr_unowned(arg)))) return err(ER_SYNTAX, ",", "expected one argument; got more");
	return Eval(car_unowned(cdr_unowned(arg)));
NORMAL:
	return unowned_cons(_semiquote(car_unowned(arg)), _semiquote(cdr_unowned(arg)));
}

LObj* semiquote(LObj* obj) { // `(e ,wee ("" (+ 2 3) ,(+ 2 3)))
	if (!is_cons(obj)) return err(ER_SYNTAX, "`", "expected one argument; got none");
	if (!is_nil(cdr_unowned(obj))) return err(ER_SYNTAX, "`", "expected one argument; got more");
	return _semiquote(car_unowned(obj));
}

LObj* unquote(LObj* obj __attribute__((unused))) {
	return err(ER_SYNTAX, ",", "should not be used outside semiquote (`)");
}

void __attribute__((constructor)) setup_definitions(void) {
	add_def("eval", func(eval), false);
	add_def("define", func(define), false);
	add_def("extern", func(extern_), false);
	add_def("import", func(import), false);
	add_def("quote", func(quote), false);
	add_def("semiquote", func(semiquote), false);
	add_def("unquote", func(unquote), false);
	dylibs = (struct dls){ .cap = 16, .len = 0, .handles = malloc(16*sizeof(void*)) };
}

void free_def(struct DefineTree* def) {
	if (!def) return;
	drop(def->val);
	if (def->owned_key) free((char*)def->key);
	free_def(def->left);
	free_def(def->right);
	free(def);
}

void __attribute__((destructor)) free_definitions(void) {
	free_def(root.left); free_def(root.right);
	dealloc();
}

void free_dylibs(void) {
	for (long i = 0; i<dylibs.len; i++) dlclose(dylibs.handles[i]);
	free(dylibs.handles);
}

int main(int argc, char** argv) {
	Args.argc = argc - 1;
	Args.argv = argv + 1;
	FILE* file = NULL;
	if (argc == 1) file = stdin;
	else file = fopen(argv[1], "r");
	if (file == NULL) { perror("failed to open file"); return 1; }

	bool running = true;
	while (running && !feof(file)) {
		struct Parsed result = parse_file(file);
		if (result.result == Err) printf("PARSE ERROR: %s\n", name(result.value));
		else {
			LObj* expr = result.value;
			LObj* final = Eval(expr);
			if (is_err(final)) {
				int len = tostring(final, NULL, 0);
				char buf[len+1];
				tostring(final, buf, len+1);
				printf("%s\n", buf);
			}
			drop(expr); drop(final);
		}
	}

	fclose(file);
	//free_dylibs();
	return 0;
}
