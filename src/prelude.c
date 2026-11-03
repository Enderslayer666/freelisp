#include <sys/syscall.h>
#include <sys/stat.h>
#include <sys/sendfile.h>
#include <linux/limits.h>
#include <stdio.h>
#include <readline/readline.h>
#include <readline/history.h>
#include <time.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <errno.h>
#include <omp.h>
#include <dirent.h>
#include <errno.h>
#include "../inc/lisp.h"
#include "../inc/interp.h"
#include "../inc/parse.h"

#ifndef uint
#define uint unsigned int
#endif

#define CLEANUP __attribute__((cleanup(cleanup)))

static LObj* get_arg(LObj* obj, const char* blame) {
	if (!is_cons(obj)) return err(ER_SYNTAX, blame, "expected one argument; got none");
	if (!is_nil(cdr_unowned(obj))) return err(ER_SYNTAX, blame, "expected one argument; got more");
	return car_unowned(obj);
}

LObj* nilq(LObj*) __asm__("\"nil?\"");
LObj* nilq(LObj* obj) {
	LObj* arg CLEANUP = Eval(try(get_arg(obj, "nil?")));
	return is_nil(arg) ? tok("T") : NULL;
}

LObj* consq(LObj*) __asm__("\"cons?\"");
LObj* consq(LObj* obj) {
	LObj* arg CLEANUP = Eval(try(get_arg(obj, "cons?")));
	return is_cons(arg) ? tok("T") : NULL;
}

LObj* errq(LObj*) __asm__("\"err?\"");
LObj* errq(LObj* obj) {
	LObj* arg CLEANUP = Eval(try(get_arg(obj, "err?")));
	return is_err(arg) ? tok("T") : NULL;
}

LObj* funcq(LObj*) __asm__("\"func?\"");
LObj* funcq(LObj* obj) {
	LObj* arg CLEANUP = Eval(try(get_arg(obj, "func?")));
	return is_callable(arg) ? tok("T") : NULL;
}

LObj* tokq(LObj*) __asm__("\"tok?\"");
LObj* tokq(LObj* obj) {
	LObj* arg CLEANUP = Eval(try(get_arg(obj, "tok?")));
	return variant(arg)==TOKEN ? tok("T") : NULL;
}

LObj* strq(LObj*) __asm__("\"str?\"");
LObj* strq(LObj* obj) {
	LObj* arg CLEANUP = Eval(try(get_arg(obj, "str?")));
	return is_str(arg) ? tok("T") : NULL;
}

LObj* intq(LObj*) __asm__("\"int?\"");
LObj* intq(LObj* obj) {
	LObj* arg CLEANUP = Eval(try(get_arg(obj, "int?")));
	return is_int(arg) ? tok("T") : NULL;
}

LObj* equ(LObj*) __asm__("\"=\"");
LObj* equ(LObj* obj) {
	if (!is_cons(obj)) return err(ER_SYNTAX, "=", "expected at least one argument; got none");
	LObj* key CLEANUP = Eval(car_unowned(obj));
	LObj* rem = foreach(a, obj, {
		LObj* arg CLEANUP = Eval(a);
		if (!eq(arg, key)) return nil();
	});
	if (!is_nil(rem)) return err(ER_SYNTAX, "=", "unexpected dotted tail");
	return tok("T");
}

LObj* error(LObj* obj) {
	if (!is_cons(obj)) return err(ER_SYNTAX, "error", "expected two arguments; got none");
	if (!is_cons(cdr_unowned(obj))) return err(ER_SYNTAX, "error", "expected two arguments; got one");
	if (!is_nil(cdr_unowned(cdr_unowned(obj)))) return err(ER_SYNTAX, "error", "expected two arguments; got more");

	LObj* lblame CLEANUP = Eval(car_unowned(obj));
	if (!is_str(lblame)) return is_err(lblame) ? clone(lblame) : err(ER_TYPE, "error", "expected str args; got other types");

	LObj* lmsg CLEANUP = Eval(car_unowned(cdr_unowned(obj)));
	if (!is_str(lmsg)) return is_err(lmsg) ? clone(lmsg) : err(ER_TYPE, "error", "expected str args; got other types");

	int blame_len = str_len(lblame);
	char* blame = malloc(blame_len+1);
	memcpy(blame, name(lblame), blame_len);
	blame[blame_len] = '\0';

	int msg_len = str_len(lmsg);
	char* msg = malloc(msg_len+1);
	memcpy(msg, name(lmsg), msg_len);
	msg[msg_len] = '\0';

	return err(ER_RUN, blame, msg);
}

LObj* cons(LObj* obj) {
	if (!is_cons(obj)) return err(ER_SYNTAX, "cons", "expected two arguments; got none");
	if (!is_cons(cdr_unowned(obj))) return err(ER_SYNTAX, "cons", "expected two arguments; got one");
	if (!is_nil(cdr_unowned(cdr_unowned(obj)))) return err(ER_SYNTAX, "cons", "expected two arguments; got more");

	LObj* a CLEANUP = Eval(car_unowned(obj));
	LObj* b CLEANUP = Eval(car_unowned(cdr_unowned(obj)));
	return _cons(a, b);
}

LObj* apply(LObj* obj) {
	if (!is_cons(obj)) return err(ER_SYNTAX, "apply", "expected two arguments; got none");
	if (!is_cons(cdr_unowned(obj))) return err(ER_SYNTAX, "apply", "expected two arguments; got one");
	if (!is_nil(cdr_unowned(cdr_unowned(obj)))) return err(ER_SYNTAX, "apply", "expected two arguments; got more");

	LObj* fn CLEANUP = Eval(car_unowned(obj));
	LObj* input CLEANUP = Eval(car_unowned(cdr_unowned(obj)));

	return _apply(fn, input);
}

LObj* and(LObj*) __asm__("\"&&\"");
LObj* and(LObj* o) {
	if (!is_cons(o)) return err(ER_SYNTAX, "&&", "expected arguments; got none");
	LObj* end = foreach (a, o, {
		LObj* arg = Eval(a);
		if (is_nil(arg)) return nil();
		drop(arg);
	});
	return is_nil(end) ? tok("T") : err(ER_SYNTAX, "&&", "unexpected dotted tail");
}

LObj* or(LObj*) __asm__("\"||\"");
LObj* or(LObj* o) {
	if (!is_cons(o)) return err(ER_SYNTAX, "||", "expected arguments; got none");
	LObj* end = foreach (a, o, {
		LObj* arg = Eval(a);
		if (!is_nil(arg)) { drop(arg); return tok("T"); }
	});
	return is_nil(end) ? nil() : err(ER_SYNTAX, "||", "unexpected dotted tail");
}

LObj* xor(LObj*) __asm__("\"^^\"");
LObj* xor(LObj* o) {
	bool acc = false;
	if (!is_cons(o)) return err(ER_SYNTAX, "||", "expected arguments; got none");
	LObj* end = foreach (a, o, {
		LObj* arg CLEANUP = Eval(a);
		acc = acc ^ !is_nil(arg);
	});
	return is_nil(end) ? nil() : err(ER_SYNTAX, "||", "unexpected dotted tail");
}

LObj* bitand(LObj*) __asm__("\"&\"");
LObj* bitand(LObj* o) {
	if (!is_cons(o)) return err(ER_SYNTAX, "&", "expected arguments; got none");
	unsigned long acc = (unsigned)-1;
	LObj* end = foreach (a, o, {
		LObj* arg CLEANUP = Eval(a);
		if (!is_int(arg)) return is_err(arg) ? clone(arg) : err(ER_TYPE, "&", "expected numeric inputs");
		acc &= int_val(arg);
	});
	return is_nil(end) ? integ(acc) : err(ER_SYNTAX, "&", "unexpected dotted tail");
}

LObj* bitor(LObj*) __asm__("\"|\"");
LObj* bitor(LObj* o) {
	if (!is_cons(o)) return err(ER_SYNTAX, "|", "expected arguments; got none");
	unsigned long acc = 0;
	LObj* end = foreach (a, o, {
		LObj* arg CLEANUP = Eval(a);
		if (!is_int(arg)) return is_err(arg) ? clone(arg) : err(ER_TYPE, "|", "expected numeric inputs");
		acc |= int_val(arg);
	});
	return is_nil(end) ? integ(acc) : err(ER_SYNTAX, "|", "unexpected dotted tail");
}

LObj* bitxor(LObj*) __asm__("\"^\"");
LObj* bitxor(LObj* o) {
	if (!is_cons(o)) return err(ER_SYNTAX, "^", "expected arguments; got none");
	unsigned long acc = 0;
	LObj* end = foreach (a, o, {
		LObj* arg CLEANUP = Eval(a);
		if (!is_int(arg)) return is_err(arg) ? clone(arg) : err(ER_TYPE, "^", "expected numeric inputs");
		acc ^= int_val(arg);
	});
	return is_nil(end) ? integ(acc) : err(ER_SYNTAX, "^", "unexpected dotted tail");
}

LObj* sum(LObj*) __asm__("\"+\"");
LObj* sum(LObj* obj) {
	if (!is_cons(obj)) return err(ER_SYNTAX, "+", "expected arguments; got none");
	long acc = 0;
	LObj* end = foreach (a, obj, {
		LObj* arg CLEANUP = Eval(a);
		if (!is_int(arg)) return is_err(arg) ? clone(arg) : err(ER_TYPE, "+", "expected numeric inputs");
		acc += int_val(arg);
	});
	return is_nil(end) ? integ(acc) : err(ER_SYNTAX, "+", "unexpected dotted tail");
}

LObj* prod(LObj*) __asm__("\"*\"");
LObj* prod(LObj* obj) {
	if (!is_cons(obj)) return err(ER_SYNTAX, "*", "expected arguments; got none");
	long acc = 1;
	LObj* end = foreach (a, obj, {
		LObj* arg CLEANUP = Eval(a);
		if (!is_int(arg)) return is_err(arg) ? clone(arg) : err(ER_TYPE, "*", "expected numeric inputs");
		acc *= int_val(arg);
	});
	return is_nil(end) ? integ(acc) : err(ER_SYNTAX, "*", "unexpected dotted tail");
}

LObj* sub(LObj*) __asm__("\"-\"");
LObj* sub(LObj* obj) {
	if (!is_cons(obj)) return err(ER_SYNTAX, "-", "expected one or two arguments; got none");
	if (is_cons(cdr_unowned(obj)) && !is_nil(cdr_unowned(cdr_unowned(obj)))) return err(ER_SYNTAX, "-", "expected one or two arguments; got more");

	LObj* ox CLEANUP = Eval(car_unowned(obj));
	if (!is_int(ox)) return is_err(ox) ? clone(ox) : err(ER_TYPE, "-", "expected numeric inputs");
	if (is_nil(obj)) return integ(-int_val(ox));
	LObj* oy CLEANUP = Eval(car_unowned(cdr_unowned(obj)));
	if (!is_int(oy)) return is_err(oy) ? clone(oy) : err(ER_TYPE, "-", "expected numeric inputs");
	return integ(int_val(ox)-int_val(oy));
}
LObj* divide(LObj*) __asm__("\"/\"");
LObj* divide(LObj* obj) {
	if (!is_cons(obj)) return err(ER_SYNTAX, "/", "expected one or two arguments; got none");
	if (is_cons(cdr_unowned(obj)) && !is_nil(cdr_unowned(cdr_unowned(obj)))) return err(ER_SYNTAX, "/", "expected one or two arguments; got more");

	LObj* oy CLEANUP = Eval(car_unowned(cdr_unowned(obj)));
	if (!is_int(oy)) return is_err(oy) ? clone(oy) : err(ER_TYPE, "/", "expected numeric inputs");
	if (int_val(oy) == 0) return err(ER_INT, "/", "division by zero");
	LObj* ox CLEANUP = Eval(car_unowned(obj));
	if (!is_int(ox)) return is_err(ox) ? clone(ox) : err(ER_TYPE, "/", "expected numeric inputs");
	return integ(int_val(ox)/int_val(oy));
}
LObj* mod(LObj*) __asm__("\"%\"");
LObj* mod(LObj* obj) {
	if (!is_cons(obj)) return err(ER_SYNTAX, "%", "expected one or two arguments; got none");
	if (is_cons(cdr_unowned(obj)) && !is_nil(cdr_unowned(cdr_unowned(obj)))) return err(ER_SYNTAX, "%", "expected one or two arguments; got more");

	LObj* oy CLEANUP = Eval(car_unowned(cdr_unowned(obj)));
	if (!is_int(oy)) return is_err(oy) ? clone(oy) : err(ER_TYPE, "%", "expected numeric inputs");
	if (int_val(oy) == 0) return err(ER_INT, "%", "division by zero");
	LObj* ox CLEANUP = Eval(car_unowned(obj));
	if (!is_int(ox)) return is_err(ox) ? clone(ox) : err(ER_TYPE, "%", "expected numeric inputs");
	return integ(int_val(ox)%int_val(oy));
}

LObj* zeroq(LObj*) __asm__("\"zero?\"");
LObj* zeroq(LObj* obj) {
	LObj* arg CLEANUP = Eval(try(get_arg(obj, "zero?")));
	if (!is_int(arg)) return is_err(arg) ? clone(arg) : err(ER_TYPE, "zero?", "expected numeric argument");
	return int_val(arg) == 0 ? tok("T") : nil();
}
LObj* negq(LObj*) __asm__("\"neg?\"");
LObj* negq(LObj* obj) {
	LObj* arg CLEANUP = Eval(try(get_arg(obj, "neg?")));
	if (!is_int(arg)) return is_err(arg) ? clone(arg) : err(ER_TYPE, "neg?", "expected numeric argument");
	return int_val(arg) < 0 ? tok("T") : nil();
}
LObj* posq(LObj*) __asm__("\"pos?\"");
LObj* posq(LObj* obj) {
	LObj* arg CLEANUP = Eval(try(get_arg(obj, "pos?")));
	if (!is_int(arg)) return is_err(arg) ? clone(arg) : err(ER_TYPE, "pos?", "expected numeric argument");
	return int_val(arg) > 0 ? tok("T") : nil();
}

LObj* evenq(LObj*) __asm__("\"even?\"");
LObj* evenq(LObj* obj) {
	LObj* arg CLEANUP = Eval(try(get_arg(obj, "even?")));
	if (!is_int(arg)) return is_err(arg) ? clone(arg) : err(ER_TYPE, "even?", "expected numeric argument");
	return int_val(arg)%2 == 0 ? tok("T") : nil();
}

LObj* cond(LObj* obj) {
	foreach(pair, obj, {
		if (!is_cons(pair)) return is_err(pair) ? clone(pair) : err(ER_SYNTAX, "cond", "expected (c1 v1) ...; got ??? ...");
		LObj* con = try(car_unowned(pair));
		LObj* b CLEANUP = Eval(con);
		if (is_nil(b)) continue;
		LObj* tail = try(cdr_unowned(pair));
		if (!is_cons(tail)) return is_err(tail) ? clone(tail) : err(ER_SYNTAX, "cond", "expected (c1 v1) ..., got (c1 . ??\?)");
		LObj* val = try(car_unowned(tail));
		if (!is_nil(try(cdr_unowned(tail)))) return err(ER_SYNTAX, "cond", "expected (c1 v1) (c2 v2) ...; got (c1 v1 ??\?) ...");
		return Eval(val);
	});
	return nil();
}

LObj* display(LObj* obj) {
	foreach (a, obj, {
		LObj* arg CLEANUP = Eval(a);
		int l = tostring(arg, NULL, 0);
		char buf[l+1]; tostring(arg, buf, l+1);
		printf("%*s", l, buf);
	});
	return nil();
}

#include "lambda_jit.c"

LObj* type(LObj* o) {
	if (!is_cons(o)) return err(ER_SYNTAX, "type", "expected one argument; got none");
	if (!is_nil(cdr_unowned(o))) return err(ER_SYNTAX, "type", "expected one argument; got more");
	LObj* arg CLEANUP = Eval(car_unowned(o));
	return integ(is_ext(arg) ? ext_type(arg) : (int)variant(arg));
}

LObj* car(LObj* o) {
	if (!is_cons(o)) return err(ER_SYNTAX, "car", "expected one argument; got none");
	if (!is_nil(cdr_unowned(o))) return err(ER_SYNTAX, "car", "expected one argument; got more");
	LObj* arg CLEANUP = Eval(car_unowned(o));
	if (!is_cons(arg)) return is_err(arg) ? clone(arg) : err(ER_TYPE, "car", "expected cons argument; got other type");
	return _car(arg);
}

LObj* cdr(LObj* o) {
	if (!is_cons(o)) return err(ER_SYNTAX, "cdr", "expected one argument; got none");
	if (!is_nil(cdr_unowned(o))) return err(ER_SYNTAX, "cdr", "expected one argument; got more");
	LObj* arg CLEANUP = Eval(car_unowned(o));
	if (!is_cons(arg)) return is_err(arg) ? clone(arg) : err(ER_SYNTAX, "cdr", "expected cons argument; got other type");
	return _cdr(arg);
}

LObj* par(LObj* o) {
	if (!is_cons(o)) return err(ER_SYNTAX, "par", "expected two arguments; got none");
	if (!is_cons(cdr_unowned(o))) return err(ER_SYNTAX, "par", "expected two arguments; got one");
	if (!is_nil(cdr_unowned(cdr_unowned(o)))) return err(ER_SYNTAX, "par", "expected two arguments; got more");
	LObj* raws[2] = { try(car_unowned(o)), try(car_unowned(cdr_unowned(o))) };
	LObj* evals[2];
	#pragma omp parallel sections
	{
		#pragma omp section
		{ evals[0] = Eval(raws[0]); }
		#pragma omp section
		{ evals[1] = Eval(raws[1]); }
	}
	LObj* final = _cons(evals[0], evals[1]);
	drop(evals[0]); drop(evals[1]);
	return final;
}

LObj* seq(LObj* o) { return cons(o); }

LObj* loop(LObj* o) {
	if (!is_cons(o)) return err(ER_SYNTAX, "loop", "expected one argument; got none");
	if (!is_nil(cdr_unowned(o))) return err(ER_SYNTAX, "loop", "expected one argument; got more");

	LObj* target = car_unowned(o);
	if (is_err(target)) return clone(target);

	while (1) {
		LObj* ev CLEANUP = Eval(target);
		if (is_err(ev)) return clone(ev);
		if (variant(ev) == TOKEN && strcmp("break", name(ev))) return nil();
	}
}

LObj* sys(LObj* o) {
	if (!is_cons(o)) return err(ER_SYNTAX, "sys", "expected at least one argument; got none");

	int argc = 0;
	for (LObj* t = o; is_cons(t); t = cdr_unowned(t)) argc++;
	LObj* argv[argc];
	int i = 0;
	int len = 0;
	for (LObj* t = o; is_cons(t); t = cdr_unowned(t)) {
		argv[i] = Eval(car_unowned(t));
		switch (variant(argv[i])) {
			case STR: len += str_len(argv[i]); break;
			case TOKEN: len += strlen(name(argv[i])); break;
			default:
				LObj* ret = is_err(argv[i]) ? clone(argv[i]) : err(ER_TYPE, "sys", "expected named or string args; got other types");
				for (int j = 0; j <= i; j++) drop(argv[j]);
				return ret;
		}
	i++;}
	char cmd[len+argc];

	int off = 0;
	for (int i = 0; i < argc; i++) {
		switch (variant(argv[i])) {
			case STR:
				memcpy(cmd+off, name(argv[i]), str_len(argv[i]));
				off += str_len(argv[i]);
				break;
			case TOKEN:
				strcpy(cmd+off, name(argv[i]));
				off += strlen(name(argv[i]));
				break;
			default: return err(ER_NONE, "sys", "unreachable");
		}
		cmd[off++] = ' ';
	}
	cmd[off-1] = '\0';
	int out = system(cmd);
	return out == 0 ? nil() : err(ER_INT, "sys", "command exited abnormally");
}

LObj* sysdotpid(LObj*) __asm__("\"sys.pid\"");
LObj* sysdotpid(LObj* o) {
	if (!is_nil(o)) return err(ER_SYNTAX, "sys.pid", "expected no arguments; got more");
	return integ(getpid());
}

LObj* sysdotcwd(LObj*) __asm__("\"sys.cwd\"");
LObj* sysdotcwd(LObj* o) {
	if (!is_nil(o)) return err(ER_SYNTAX, "sys.cwd", "expected no arguments; got more");
	char* cwd = malloc(PATH_MAX);
	return getcwd(cwd, PATH_MAX) != NULL ? str_owned(cwd) : err(-errno, "sys.cwd", strerror(errno));
}

LObj* sysdotchdir(LObj*) __asm__("\"sys.chdir\"");
LObj* sysdotchdir(LObj* o) {
	if (!is_cons(o)) return err(ER_SYNTAX, "sys.chdir", "expected one argument; got none");
	if (!is_nil(cdr_unowned(o))) return err(ER_SYNTAX, "sys.chdir", "expected one argument; got more");
	LObj* lpath CLEANUP = Eval(car_unowned(o));
	if (!is_str(lpath)) return is_err(lpath) ? clone(lpath) : err(ER_TYPE, "sys.chdir", "expected str path argument; got other type");
	int path_len = str_len(lpath);
	char path[path_len+1];
	for (int i = 0; i<path_len; i++) path[i] = name(lpath)[i];
	path[path_len] = '\0';

	return chdir(path) == 0 ? nil() : err(-errno, "sys.chdir", strerror(errno));
}

LObj* sysdotcd(LObj*) __asm__("\"sys.cd\"");
LObj* sysdotcd(LObj* o) {
	if (!is_cons(o)) return err(ER_SYNTAX, "sys.cd", "expected two arguments; got none");
	if (!is_cons(cdr_unowned(o))) return err(ER_SYNTAX, "sys.cd", "expected two arguments; got one");
	if (!is_nil(cdr_unowned(cdr_unowned(o)))) return err(ER_SYNTAX, "sys.cd", "expected two arguments; got more");

	LObj* lpath CLEANUP = Eval(car_unowned(o));
	if (!is_str(lpath)) return is_err(lpath) ? clone(lpath) : err(ER_TYPE, "sys.cd", "expected str path first argument; got other type");
	int path_len = str_len(lpath);
	char path[path_len+1];
	for (int i = 0; i<path_len; i++) path[i] = name(lpath)[i];
	path[path_len] = '\0';

	char old[PATH_MAX];
	if (getcwd(old, PATH_MAX) == NULL) return err(-errno, "sys.cd", "getcwd failed to save");
	if (chdir(path) != 0) return err(-errno, "sys.cd", strerror(errno));

	LObj* ret = Eval(car_unowned(cdr_unowned(o)));
	if (chdir(old) != 0) { drop(ret); return err(-errno, "sys.cd", "chdir failed to restore"); }
	return ret;
}

static void clean_file(int* fd) { close(*fd); }

static LObj* move_file(char* src, char* dst) {
	if (rename(src, dst) == 0) return nil();
	switch (errno) {
		case EXDEV: case EMLINK: {
			int fsrc __attribute__((cleanup(clean_file))) = open(src, O_RDONLY);
			if (fsrc < 0) return err(-errno, "sys.mv", "file not accessible");

			struct stat st;
			if (fstat(fsrc, &st) < 0) return err(-errno, "sys.mv", "stat failed");
			ssize_t rem = st.st_size;
			mode_t mode = st.st_mode & 0777;

			int fdst __attribute__((cleanup(clean_file))) = creat(dst, mode);
			if (fdst < 0) return err(-errno, "sys.mv", "destination not accessible");

			while (rem > 0) {
				ssize_t done = sendfile(fdst, fsrc, NULL, rem);
				if (done < 0) return err(-errno, "sys.mv", "sendfile failed; may have left artifacts");
				rem -= done;
			}

			return nil();
		}
		default: return err(-errno, "sys.mv", strerror(errno));
	}
}

LObj* sysdotmv(LObj*) __asm__("\"sys.mv\"");
LObj* sysdotmv(LObj* o) {
	if (!is_cons(o)) return err(ER_SYNTAX, "sys.mv", "expected two arguments; got none");
	if (!is_cons(cdr_unowned(o))) return err(ER_SYNTAX, "sys.mv", "expected two arguments; got one");
	if (!is_nil(cdr_unowned(cdr_unowned(o)))) return err(ER_SYNTAX, "sys.mv", "expected two arguments; got more");

	LObj* lsrc CLEANUP = Eval(car_unowned(o));
	if (!is_str(lsrc)) return err(ER_TYPE, "sys.mv", "expected string path args; got other first type");
	int src_len = str_len(lsrc);
	char src[src_len+1];
	memcpy(src, name(lsrc), src_len);
	src[src_len] = '\0';

	LObj* ldst CLEANUP = Eval(car_unowned(cdr_unowned(o)));
	if (!is_str(ldst)) return err(ER_TYPE, "sys.mv", "expected string path args; got other first type");
	int dst_len = str_len(ldst);
	char dst[dst_len+1];
	memcpy(dst, name(ldst), dst_len);
	dst[dst_len] = '\0';

	return move_file(src, dst);
}

//new
LObj* sysdotsleep(LObj*) __asm__("\"sys.sleep\"");
LObj* sysdotsleep(LObj* o) {
	if (!is_cons(o)) return err(ER_SYNTAX, "sleep", "expected one or two arguments; got none");
	struct timespec buf;
	struct timespec rem;

	if (is_nil(cdr_unowned(o))) {
		LObj* ltime CLEANUP = Eval(car_unowned(o));
		if (is_cons(ltime)) {
			if (!is_cons(cdr_unowned(ltime))) return err(ER_SYNTAX, "sleep", "expected (sec nsec); got (sec . ??\?)");
			if (!is_nil(cdr_unowned(cdr_unowned(ltime)))) return err(ER_SYNTAX, "sleep", "expected (sec nsec); got (sec nsec . ??\?)");
			if (!is_int(car_unowned(ltime))) return is_err(car_unowned(ltime)) ? clone(car_unowned(ltime)) : err(ER_TYPE, "sleep", "expected int sec; got other type");
			if (!is_int(car_unowned(cdr_unowned(ltime)))) return is_err(car_unowned(cdr_unowned(ltime))) ? clone(car_unowned(cdr_unowned(ltime))) : err(ER_TYPE, "sleep", "expected int nsec; got other type");
			buf.tv_sec = int_val(car_unowned(ltime));
			buf.tv_nsec = int_val(car_unowned(cdr_unowned(ltime)));
		} else if (is_int(ltime)) {
			buf.tv_sec = int_val(ltime);
			buf.tv_nsec = 0;
		} else return is_err(ltime) ? clone(ltime) : err(ER_SYNTAX, "sleep", "expected sec, sec nsec, or (sec nsec); got other structure");
	} else if (is_cons(cdr_unowned(o))) {
		if (!is_nil(cdr_unowned(cdr_unowned(o)))) return err(ER_SYNTAX, "sleep", "expected one or two arguments; got more");
		LObj* lsec CLEANUP = Eval(car_unowned(o));
		if (!is_int(lsec)) return is_err(lsec) ? clone(lsec) : err(ER_TYPE, "sleep", "expected int sec; got other type");
		LObj* lnsec CLEANUP = Eval(car_unowned(cdr_unowned(o)));
		if (!is_int(lnsec)) return is_err(lnsec) ? clone(lnsec) : err(ER_TYPE, "sleep", "expected int nsec; got other type");
		buf.tv_sec = int_val(lsec);
		buf.tv_nsec = int_val(lnsec);
	} else return err(ER_SYNTAX, "sleep", "expected sec, sec nsec, or (sec nsec); got other structure");

	return nanosleep(&buf, &rem) == 0 ? nil() : unowned_cons(integ(rem.tv_sec), unowned_cons(integ(rem.tv_nsec), nil()));
}

extern char** environ;
//new
LObj* sysdotexec(LObj*) __asm__("\"sys.exec\"");
LObj* sysdotexec(LObj* o) {
	if (!is_cons(o)) return err(ER_SYNTAX, "sys.exec", "expected at least one argument; got none");

	int res = fork();
	if (res == 0) {
		int argc = 0;
		for (LObj* obj = o; is_cons(obj); obj = cdr_unowned(obj)) argc++;
		char** argv = malloc((argc+1) * sizeof(char*));
		argv[argc] = NULL;

		for (int i = 0; is_cons(o); o = cdr_unowned(o)) {
			LObj* larg CLEANUP = Eval(car_unowned(o));
			if (!is_str(larg)) {
				for (int j = 0; j<i; j++) free(argv[j]);
				free(argv);
				exit(0);
			}
			char* arg = malloc(str_len(larg) + 1);
			for (int j = 0; j < str_len(larg); j++) arg[j] = name(larg)[j];
			arg[str_len(larg)] = '\0';
			argv[i] = arg;
		i++;}

		execve(argv[0], argv, environ);
		exit(127);
	} else return res>0 ? nil() : err(-errno, "sys.exec", strerror(errno));
}

LObj* sysdotexit(LObj*) __asm__("\"sys.exit\"");
LObj* sysdotexit(LObj* o) {
	if (is_nil(o)) exit(0);
	if (!is_cons(o)) return err(ER_SYNTAX, "sys.exit", "bad args");
	if (!is_nil(cdr_unowned(o))) return err(ER_SYNTAX, "sys.exit", "expected at most one argument; got more");
	LObj* code CLEANUP = Eval(car_unowned(o));
	if (!is_int(code)) return is_err(code) ? clone(code) : err(ER_TYPE, "sys.exit", "expected int argument; got other type");
	exit(int_val(code));
}

// str

LObj* strdotlen(LObj*) __asm__("\"str.len\"");
LObj* strdotlen(LObj* o) {
	if (!is_cons(o)) return err(ER_SYNTAX, "str.len", "expected one argument; got none");
	if (!is_nil(cdr_unowned(o))) return err(ER_SYNTAX, "str.len", "expected one argument; got more");
	LObj* arg CLEANUP = Eval(car_unowned(o));
	if (!is_str(arg)) return is_err(arg) ? clone(arg) : err(ER_TYPE, "str.len", "expected string argument; got other type");
	return integ(str_len(arg));
}

LObj* strtocons(LObj*) __asm__("\"str->cons\"");
LObj* strtocons(LObj* o) {
	if (!is_cons(o)) return err(ER_SYNTAX, "str->cons", "expected one argument; got zero");
	if (!is_nil(cdr_unowned(o))) return err(ER_SYNTAX, "str->cons", "expected one argument; got more");
	LObj* arg CLEANUP = Eval(car_unowned(o));
	if (!is_str(arg)) return is_err(arg) ? clone(arg) : err(ER_TYPE, "str->cons", "expected string arg");
	LObj* strtoconsloop(const char* s) {
		if (*s == '\0') return nil();
		return _cons(integ(*s), strtoconsloop(s+1));
	}
	return strtoconsloop(name(arg));
}

LObj* constostr(LObj*) __asm__("\"cons->str\"");
LObj* constostr(LObj* o) {
	if (!is_cons(o)) return err(ER_SYNTAX, "cons->str", "expected one argument; got zero");
	if (!is_nil(cdr_unowned(o))) return err(ER_SYNTAX, "str->cons", "expected one argument; got more");
	LObj* arg CLEANUP = Eval(car_unowned(o));
	int len = 0;
	LObj* arg_pass = arg;
	while (is_cons(arg_pass)) {
		if (!is_int(car_unowned(arg_pass))) return is_err(car_unowned(arg_pass)) ? clone(car_unowned(arg_pass)) : err(ER_TYPE, "cons->str", "expected list of numbers");
		arg_pass = cdr_unowned(arg_pass);
		len++;
	}
	if (!is_nil(arg_pass)) return err(ER_TYPE, "cons->str", "unexpected dotted tail");
	char* s = malloc(len+1);
	s[len] = '\0';
	int i = 0;
	arg_pass = arg;
	while (!is_nil(arg_pass)) {
		s[i++] = int_val(car_unowned(arg_pass));
		arg_pass = cdr_unowned(arg_pass);
	}
	return str_owned(s);
}

LObj* tostr(LObj*) __asm__("\"str<-\"");
LObj* tostr(LObj* o) {
	if (!is_cons(o)) return err(ER_SYNTAX, "str<-", "expected one argument; got none");
	if (!is_nil(cdr_unowned(o))) return err(ER_SYNTAX, "str<-", "expected one argument; got more");
	LObj* l CLEANUP = Eval(car_unowned(o));
	int len = tostring(l, NULL, 0);
	char* buf = malloc(len+1);
	int n = tostring(l, buf, len+1);
	return str_sized(buf, n < len ? n : len);
}

LObj* strdotinput(LObj*) __asm__("\"str.input\"");
LObj* strdotinput(LObj* o) {
	if (!is_nil(o)) return err(ER_SYNTAX, "str.input", "expected no arguments; got more");
	char* line = NULL;
	size_t len = 0;
	if (getline(&line, &len, stdin) == -1) { free(line); return err(-errno, "str.input", strerror(errno)); }
	return str_sized(line, strlen(line)-1);
}

LObj* strdotcat(LObj*) __asm__("\"str.cat\"");
LObj* strdotcat(LObj* o) {
	if (is_nil(o)) return str("\0");
	int argc = 0;
	int argcap = 16;
	LObj** args = malloc(argcap * sizeof(LObj*));


	for (LObj* obj = o; is_cons(obj); obj = cdr_unowned(obj)) {
		if (argc == argcap) {
			argcap = argcap * 2;
			args = realloc(args, argcap * sizeof(void*));
		}
		LObj* val = Eval(car_unowned(obj));
		if (!is_str(val)) {
			for (int i = 0; i < argc; i++) drop(args[i]);
			free(args);
			return is_err(val) ? clone(val) : err(ER_TYPE, "str.cat", "expected str args; got other types");
		}
		args[argc++] = val;
	}

	int len = 0;
	for (int i = 0; i < argc; i++) len += str_len(args[i]);
	char* buf = malloc(len);
	int off = 0;
	for (int i = 0; i < argc; i++) {
		for (int j = 0; j < str_len(args[i]); j++) buf[off++] = name(args[i])[j];
		drop(args[i]);
	}
	free(args);
	return str_sized(buf, off);
}

LObj* strdotrep(LObj*) __asm__("\"str.rep\"");
LObj* strdotrep(LObj* o) {
	if (!is_cons(o)) return err(ER_SYNTAX, "str.rep", "expected two args; got none");
	if (!is_cons(cdr_unowned(o))) return err(ER_SYNTAX, "str.rep", "expected two args; got one");
	if (!is_nil(cdr_unowned(cdr_unowned(o)))) return err(ER_SYNTAX, "str.rep", "expected two args; got more");

	LObj* lnum CLEANUP = Eval(car_unowned(o));
	if (!is_int(lnum)) return err(ER_TYPE, "str.rep", "expected int count first argument; got other type");
	int n = int_val(lnum);
	if (n <= 0) return str("");

	LObj* lstr CLEANUP = Eval(car_unowned(cdr_unowned(o)));
	if (!is_str(lstr)) return err(ER_TYPE, "str.rep", "expected str second argument; got other type");
	int len = str_len(lstr) * n;
	if (len == 0) return str("");
	char* str = malloc(len);
	for (int i = 0; i < len; i++) str[i] = name(lstr)[i % str_len(lstr)];

	return str_sized(str, len);
}

LObj* strdotat(LObj*) __asm__("\"str.at\"");
LObj* strdotat(LObj* o) {
	if (!is_cons(o)) return err(ER_SYNTAX, "str.at", "expected two args; got none");
	if (!is_cons(cdr_unowned(o))) return err(ER_SYNTAX, "str.at", "expected two args; got one");
	if (!is_nil(cdr_unowned(cdr_unowned(o)))) return err(ER_SYNTAX, "str.at", "expected two args; got more");

	LObj* lstr CLEANUP = Eval(car_unowned(o));
	if (!is_str(lstr)) return err(ER_TYPE, "str.at", "expected str first arg; got other type");
	const char* str = name(lstr);

	LObj* lptr CLEANUP = Eval(car_unowned(cdr_unowned(o)));
	if (is_int(lptr)) {
		int ptr = int_val(lptr);
		if (ptr < 0 || ptr > str_len(lstr)) return err(ER_INT, "str.at", "int address outside string");
		return integ(str[ptr]);
	} else if (is_cons(lptr)) {
		if (!is_int(car_unowned(lptr))) return is_err(car_unowned(lptr)) ? clone(car_unowned(lptr)) : err(ER_TYPE, "str.at", "second arg: slice's lower bound expects an int");
		if (!is_int(cdr_unowned(lptr))) return is_err(cdr_unowned(lptr)) ? clone(cdr_unowned(lptr)) : err(ER_TYPE, "str.at", "second arg: slice's upper bound expects an int");
		int lo = int_val(car_unowned(lptr));
		int hi = int_val(car_unowned(lptr));
		int len = str_len(lstr);
		if (lo < 0 || lo >= len || hi < 0 || hi > len) return err(ER_INT, "str.at", "slice bounds outside string");
		char* buf = malloc(hi - lo);
		for (int i = 0; i < hi - lo; i++) buf[i] = str[i + lo];
		return str_sized(buf, hi - lo);
	} else return is_err(lptr) ? clone(lptr) : err(ER_TYPE, "str.at", "second arg: expected int address or slice `(int int)`");
}

LObj* strdotfind(LObj*) __asm__("\"str.find\"");
LObj* strdotfind(LObj* o) {
	if (!is_cons(o)) return err(ER_SYNTAX, "str.find", "expected two args; got none");
	if (!is_cons(cdr_unowned(o))) return err(ER_SYNTAX, "str.find", "expected two args; got one");
	if (!is_nil(cdr_unowned(cdr_unowned(o)))) return err(ER_SYNTAX, "str.find", "expected two args; got more");

	LObj* lstr CLEANUP = Eval(car_unowned(o));
	if (!is_str(lstr)) return is_err(lstr) ? clone(lstr) : err(ER_TYPE, "str.find", "expected string first argument");
	const char* haystack = name(lstr);
	int haystack_len = str_len(lstr);

	LObj* lkey CLEANUP = Eval(car_unowned(cdr_unowned(o)));
	switch (variant(lkey)) {
		case INT: {
			char needle = int_val(lkey);
			for (int i = 0; i<haystack_len; i++) if (haystack[i] == needle) return integ(i);
			return nil(); }
		case STR: {
			const char* needle = name(lkey);
			int needle_len = str_len(lkey);
			for (int i = 0; i<haystack_len - needle_len; i++) {
				for (int j = 0; j<needle_len; j++) if (haystack[i+j] != needle[j]) goto CONT;
				return integ(i);
				CONT:
			}
			return nil(); }
		case ERR: return lkey;
		default: return err(ER_TYPE, "str.find", "expected searchable key");
	}
}

LObj* strdotparse(LObj*) __asm__("\"str.parse\"");
LObj* strdotparse(LObj* o) {
	if (!is_cons(o)) return err(ER_SYNTAX, "str.parse", "expected one argument; got none");
	if (!is_nil(cdr_unowned(o))) return err(ER_SYNTAX, "str.parse", "expected one argument; got more");
	LObj* lstr CLEANUP = Eval(car_unowned(o));
	if (!is_str(lstr)) return is_err(lstr) ? clone(lstr) : err(ER_TYPE, "str.parse", "expected string argument");
	struct Parsed parsed = parse_str(name(lstr));
	return parsed.value;
}

#define T_FILE 873

uint file_print(FILE* file, char* buf, uint len) {
	return snprintf(buf, len, "file:%d", fileno(file));
}

LObj* strdotsh(LObj*) __asm__("\"str.sh\"");
LObj* strdotsh(LObj* o) {
	if (!is_cons(o)) return err(ER_SYNTAX, "str.sh", "expected one argument; got none");
	if (!is_nil(cdr_unowned(o))) return err(ER_SYNTAX, "str.sh", "expected one argument; got more");
	LObj* lstr CLEANUP = Eval(car_unowned(o));
	if (!is_str(lstr)) return is_err(lstr) ? clone(lstr) : err(ER_TYPE, "str.sh", "expected string argument");
	const char* str = name(lstr);
	int len = str_len(lstr);

	int l2b[2];
	int b2l[2];
	if (pipe(l2b) == -1 || pipe(b2l) == -1) return err(-errno, "str.sh", "pipe failed");
	int pid = fork();
	if (pid == -1) return err(-errno, "str.sh", "fork failed");
	if (pid == 0) {
		close(l2b[1]); close(b2l[0]);
		dup2(l2b[0], 0);
		dup2(b2l[1], 1);
		close(l2b[0]); close(b2l[1]);
		execl("/bin/sh", "sh", NULL);
		perror("execl failed");
		exit(1);
	}
	close(l2b[0]); close(b2l[1]);
	dprintf(l2b[1], "%.*s\n", len, str);
	close(l2b[1]);
	FILE* out = fdopen(b2l[0], "r");
	if (out == NULL) { close(b2l[0]); return err(ER_INT, "str.sh", "failed to create file wrapper"); }
	return ext(T_FILE, out, (void*)fclose, (void*)file_print);
}

// file

// new
LObj* filedotaccess(LObj*) __asm__("\"file.access\"");
LObj* filedotaccess(LObj* o) {
	if (!is_cons(o)) return err(ER_SYNTAX, "file.access", "expected two arguments; got none");
	if (!is_cons(cdr_unowned(o))) return err(ER_SYNTAX, "file.access", "expected two arguments; got one");
	if (!is_nil(cdr_unowned(cdr_unowned(o)))) return err(ER_SYNTAX, "file.access", "expected two arguments; got more");

	LObj* lpath CLEANUP = Eval(car_unowned(o));
	if (!is_str(lpath)) return is_err(lpath) ? clone(lpath) : err(ER_TYPE, "file.access", "expected str path first arg; got other type");
	int path_len = str_len(lpath);
	char path[path_len+1];
	for (int i = 0; i<path_len; i++) path[i] = name(lpath)[i];
	path[path_len] = '\0';

	LObj* lmode CLEANUP = Eval(car_unowned(cdr_unowned(o)));
	if (!is_int(lmode)) return is_err(lmode) ? clone(lmode) : err(ER_TYPE, "file.access", "expected int mode second argument; got other type");
	int mode = int_val(lmode);

	return access(path, mode) == 0 ? tok("T") : nil(); // TODO: check whether constants are right
}

LObj* filedotopen(LObj*) __asm__("\"file.open\"");
LObj* filedotopen(LObj* o) {
	if (!is_cons(o)) return err(ER_SYNTAX, "file.open", "expected two arguments; got none");
	if (!is_cons(cdr_unowned(o))) return err(ER_SYNTAX, "file.open", "expected two arguments; got one");
	if (!is_nil(cdr_unowned(cdr_unowned(o)))) return err(ER_SYNTAX, "file.open", "expected two arguments; got more");

	LObj* lpath CLEANUP = Eval(car_unowned(o));
	if (!is_str(lpath)) return is_err(lpath) ? clone(lpath) : err(ER_TYPE, "file.open", "expected str path first arg; got other type");
	int path_len = str_len(lpath);
	char path[path_len+1];
	for (int i = 0; i<path_len; i++) path[i] = name(lpath)[i];
	path[path_len] = '\0';

	LObj* lmode CLEANUP = Eval(car_unowned(cdr_unowned(o)));
	if (!is_str(lmode)) return is_err(lmode) ? clone(lmode) : err(ER_TYPE, "file.open", "expected str mode second arg; got other type");
	int mode_len = str_len(lmode);
	char mode[mode_len+1];
	for (int i = 0; i<mode_len; i++) mode[i] = name(lmode)[i];
	mode[mode_len] = '\0';

	FILE* file = fopen(path, mode);
	if (file == NULL) return err(-errno, "file.open", strerror(errno));
	return ext(T_FILE, file, (void*)fclose, (void*)file_print);
}

LObj* fileq(LObj*) __asm__("\"file?\"");
LObj* fileq(LObj* o) {
	if (!is_cons(o)) return err(ER_SYNTAX, "file?", "expected one argument; got none");
	if (!is_nil(cdr_unowned(o))) return err(ER_SYNTAX, "file?", "expected one argument; got more");
	LObj* inp CLEANUP = Eval(car_unowned(o));
	return is_ext(inp) && ext_type(inp)==T_FILE ? tok("T") : nil();
}

LObj* filedottemp(LObj*) __asm__("\"file.temp\"");
LObj* filedottemp(LObj* o) {
	if (!is_nil(o)) return err(ER_SYNTAX, "file.temp", "expected no arguments; got more");
	FILE* file = tmpfile();
	return ext(T_FILE, file, (void*)fclose, (void*)file_print);
}

LObj* filedotreaddotchar(LObj*) __asm__("\"file.read.char\"");
LObj* filedotreaddotchar(LObj* o) {
	if (!is_cons(o)) return err(ER_SYNTAX, "file.read.char", "expected one argument; got none");
	if (!is_nil(cdr_unowned(o))) return err(ER_SYNTAX, "file.read.char", "expected one argument; got more");
	LObj* lfile CLEANUP = Eval(car_unowned(o));
	if (!is_ext(lfile) || ext_type(lfile) != T_FILE) return is_err(lfile) ? clone(lfile) : err(ER_TYPE, "file.read.char", "expected file; got other type");
	FILE* file = ext_ptr(lfile);
	int chr = fgetc(file);
	return chr == EOF ? nil() : integ(chr);
}


LObj* filedotreaddotline(LObj*) __asm__("\"file.read.line\"");
LObj* filedotreaddotline(LObj* o) {
	if (!is_cons(o)) return err(ER_SYNTAX, "file.read.line", "expected one argument; got none");
	if (!is_nil(cdr_unowned(o))) return err(ER_SYNTAX, "file.read.line", "expected one argument; got more");
	LObj* lfile CLEANUP = Eval(car_unowned(o));
	if (!is_ext(lfile) || ext_type(lfile) != T_FILE) return is_err(lfile) ? clone(lfile) : err(ER_TYPE, "file.read.line", "expected file; got other type");
	FILE* file = ext_ptr(lfile);
	long start = ftell(file);
	int size = 1;
	for (int c = fgetc(file); c != EOF && c != '\n'; c = fgetc(file)) size++;
	char* buf = malloc(size+1);
	buf[size] = '\0';
	fseek(file, start, SEEK_SET);
	buf = fgets(buf, size, file);
	return str_sized(buf, size);
}

LObj* filedotreaddotstr(LObj*) __asm__("\"file.read.str\"");
LObj* filedotreaddotstr(LObj* o) {
	if (!is_cons(o)) return err(ER_SYNTAX, "file.read.str", "expected two arguments; got none");
	if (!is_cons(cdr_unowned(o))) return err(ER_SYNTAX, "file.read.str", "expected two arguments; got one");
	if (!is_nil(cdr_unowned(cdr_unowned(o)))) return err(ER_SYNTAX, "file.read.str", "expected two arguments; got more");
	LObj* llen CLEANUP = Eval(car_unowned(cdr_unowned(o)));
	if (!is_int(llen)) return is_err(llen) ? clone(llen) : err(ER_TYPE, "file.read.str", "expected int length as second argument; got other type");
	LObj* lfile CLEANUP = Eval(car_unowned(o));
	if (!is_ext(lfile) || ext_type(lfile) != T_FILE) return is_err(lfile) ? clone(lfile) : err(ER_TYPE, "file.read.str", "expected file as first argument; got other type");
	FILE* file = ext_ptr(lfile);
	int amt = int_val(llen);
	char* buf = malloc(amt+1);
	int n = fread(buf, 1, amt, file);
	buf[n] = '\0';
	return str_sized(buf, n);
}

LObj* filedotreaddotfull(LObj*) __asm__("\"file.read.full\"");
LObj* filedotreaddotfull(LObj* o) {
	if (!is_cons(o)) return err(ER_SYNTAX, "file.read.full", "expected one argument; got none");
	if (!is_nil(cdr_unowned(o))) return err(ER_SYNTAX, "file.read.full", "expected one argument; got more");
	LObj* lfile CLEANUP = Eval(car_unowned(o));
	if (!is_ext(lfile) || ext_type(lfile) != T_FILE) return is_err(lfile) ? clone(lfile) : err(ER_TYPE, "file.read.full", "expected file; got other type");
	FILE* file = ext_ptr(lfile);
	long curr = ftell(file);
	fseek(file, 0, SEEK_END);
	long len = ftell(file) - curr;
	fseek(file, curr, SEEK_SET);
	char* buf = malloc(len+1);
	int n = fread(buf, 1, len, file);
	buf[n] = '\0';
	return str_sized(buf, n);
}

LObj* filedotseek(LObj*) __asm__("\"file.seek\"");
LObj* filedotseek(LObj* o) {
	if (!is_cons(o)) return err(ER_SYNTAX, "file.seek", "expected two arguments; got none");
	if (!is_cons(cdr_unowned(o))) return err(ER_SYNTAX, "file.seek", "expected two arguments; got one");
	if (!is_nil(cdr_unowned(cdr_unowned(o)))) return err(ER_SYNTAX, "file.seek", "expected two arguments; got more");
	LObj* lfile CLEANUP = Eval(car_unowned(o));
	if (!is_ext(lfile) || ext_type(lfile) != T_FILE) return is_err(lfile) ? clone(lfile) : err(ER_TYPE, "file.seek", "expected file first arg; got other type");
	LObj* laddr CLEANUP = Eval(car_unowned(cdr_unowned(o)));
	if (!is_int(laddr)) return is_err(laddr) ? clone(laddr) : err(ER_TYPE, "file.seek", "expected int second arg; got other type");
	FILE* file = ext_ptr(lfile);
	int addr = int_val(laddr);
	if (addr >= 0) fseek(file, addr, SEEK_SET);
	else fseek(file, addr, SEEK_END);
	return nil();
}

LObj* filedotskip(LObj*) __asm__("\"file.skip\"");
LObj* filedotskip(LObj* o) {
	if (!is_cons(o)) return err(ER_SYNTAX, "file.skip", "expected two arguments; got none");
	if (!is_cons(cdr_unowned(o))) return err(ER_SYNTAX, "file.skip", "expected two arguments; got one");
	if (!is_nil(cdr_unowned(cdr_unowned(o)))) return err(ER_SYNTAX, "file.skip", "expected two arguments; got more");
	LObj* lfile CLEANUP = Eval(car_unowned(o));
	if (!is_ext(lfile) || ext_type(lfile) != T_FILE) return is_err(lfile) ? clone(lfile) : err(ER_TYPE, "file.skip", "expected file first arg; got other type");
	LObj* laddr CLEANUP = Eval(car_unowned(cdr_unowned(o)));
	if (!is_int(laddr)) return is_err(laddr) ? clone(laddr) : err(ER_TYPE, "file.skip", "expected int second arg; got other type");
	FILE* file = ext_ptr(lfile);
	int addr = int_val(laddr);
	fseek(file, addr, SEEK_CUR);
	return nil();
}

LObj* filedotskipdotline(LObj*) __asm__("\"file.skip.line\"");
LObj* filedotskipdotline(LObj* o) {
	if (!is_cons(o)) return err(ER_SYNTAX, "file.skip.line", "expected one argument; got none");
	if (!is_nil(cdr_unowned(o))) return err(ER_SYNTAX, "file.skip.line", "expected one argument; got more");
	LObj* lfile CLEANUP = Eval(car_unowned(o));
	if (!is_ext(lfile) || ext_type(lfile) != T_FILE) return is_err(lfile) ? clone(lfile) : err(ER_TYPE, "file.skip.line", "expected file; got other type");
	FILE* file = ext_ptr(lfile);
	for (int c = fgetc(file); c != EOF && c != '\n'; c = fgetc(file));
	return nil();
}

LObj* filedoteofq(LObj*) __asm__("\"file.eof?\"");
LObj* filedoteofq(LObj* o) {
	if (!is_cons(o)) return err(ER_SYNTAX, "file.eof?", "expected one argument; got none");
	if (!is_nil(cdr_unowned(o))) return err(ER_SYNTAX, "file.eof?", "expected one argument; got more");
	LObj* lfile CLEANUP = Eval(car_unowned(o));
	if (!is_ext(lfile) || ext_type(lfile) != T_FILE) return is_err(lfile) ? clone(lfile) : err(ER_TYPE, "file.eof?", "expected file; got other type");
	FILE* file = ext_ptr(lfile);
	return feof(file) ? tok("T") : nil();
}

LObj* filedoterrq(LObj*) __asm__("\"file.err?\"");
LObj* filedoterrq(LObj* o) {
	if (!is_cons(o)) return err(ER_SYNTAX, "file.err?", "expected one argument; got none");
	if (!is_nil(cdr_unowned(o))) return err(ER_SYNTAX, "file.err?", "expected one argument; got more");
	LObj* lfile CLEANUP = Eval(car_unowned(o));
	if (!is_ext(lfile) || ext_type(lfile) != T_FILE) return is_err(lfile) ? clone(lfile) : err(ER_TYPE, "file.err?", "expected file; got other type");
	FILE* file = ext_ptr(lfile);
	return ferror(file) ? tok("T") : nil();
}

LObj* filedotparse(LObj*) __asm__("\"file.parse\"");
LObj* filedotparse(LObj* o) {
	if (!is_cons(o)) return err(ER_SYNTAX, "file.parse", "expected one argument; got none");
	if (!is_nil(cdr_unowned(o))) return err(ER_SYNTAX, "file.parse", "expected one argument; got more");
	LObj* lfile CLEANUP = Eval(car_unowned(o));
	if (!is_ext(lfile) || ext_type(lfile) != T_FILE) return is_err(lfile) ? clone(lfile) : err(ER_TYPE, "file.parse", "expected file; got other type");
	FILE* file = ext_ptr(lfile);
	struct Parsed parsed = parse_file(file);
	return parsed.value;
}

LObj* filedotstdin(LObj*) __asm__("\"file.stdin\"");
LObj* filedotstdin(LObj* o) {
	if (!is_nil(o)) return err(ER_SYNTAX, "file.stdin", "expected no arguments; got more");
	return ext_static(T_FILE, stdin, (void*)file_print);
}
LObj* filedotstdout(LObj*) __asm__("\"file.stdout\"");
LObj* filedotstdout(LObj* o) {
	if (!is_nil(o)) return err(ER_SYNTAX, "file.stdout", "expected no arguments; got more");
	return ext_static(T_FILE, stdout, (void*)file_print);
}
LObj* filedotstderr(LObj*) __asm__("\"file.stderr\"");
LObj* filedotstderr(LObj* o) {
	if (!is_nil(o)) return err(ER_SYNTAX, "file.stderr", "expected no arguments; got more");
	return ext_static(T_FILE, stderr, (void*)file_print);
}

LObj* filedotdescriptor(LObj*) __asm__("\"file.descriptor\"");
LObj* filedotdescriptor(LObj* o) {
	if (!is_cons(o)) return err(ER_SYNTAX, "file.descriptor", "expected one argument; got none");
	if (!is_nil(cdr_unowned(o))) return err(ER_SYNTAX, "file.descriptor", "expected one argument; got more");
	LObj* lfile CLEANUP = Eval(car_unowned(o));
	if (!is_ext(lfile) || ext_type(lfile) != T_FILE) return is_err(lfile) ? clone(lfile) : err(ER_TYPE, "file.descriptor", "expected file; got other type");
	FILE* file = ext_ptr(lfile);
	return integ(fileno(file));
}

#define T_TABLE 453
struct Table {
	char* key;
	LObj* val;
	struct Table* left;
	struct Table* right;
};

static void table_free(struct Table* t) {
	if (t == NULL) return;
	free(t->key);
	drop(t->val);
	table_free(t->left);
	table_free(t->right);
	free(t);
}

static uint table_print_internal(struct Table* table, char* buf, uint cap) {
	uint x = 0;
	if (table != NULL) {
		x += table_print_internal(table->left, buf+x, cap < x ? 0 : cap - x);
		x += snprintf(buf+x, cap < x ? 0 : cap - x, "%s : ", table->key);
		x += tostring(table->val, buf+x, cap < x ? 0 : cap - x);
		x += snprintf(buf+x, cap < x ? 0 : cap - x, "; ");
		x += table_print_internal(table->right, buf+x, cap < x ? 0 : cap - x);
	}
	return x;
}

static uint table_print(struct Table* table, char* buf, uint cap) {
	uint x = snprintf(buf, cap, "{ ");
	x += table_print_internal(table, buf+x, cap < x ? 0 : cap - x);
	x += snprintf(buf+x, cap < x ? 0 : cap - x, "}");
	return x;
}

static LObj* _table(int len, char** keys, LObj** vals) {
	struct Table* table = NULL;
	for (int i = 0; i < len; i++) {
		struct Table elem = { .key = keys[i], .val = vals[i] };
		if (table == NULL) {
			table = malloc(sizeof(struct Table));
			*table = elem;
			continue;
		}
		int comp;
		struct Table* curr = table;
		struct Table* prev = curr;
		while (curr != NULL) {
			prev = curr;
			comp = strcmp(elem.key, curr->key);
			if (comp == 0) goto match;
			curr = comp < 0 ? prev->left : prev->right;
		}
		curr = malloc(sizeof(struct Table));
		*curr = elem;
		if (comp < 0) prev->left = curr;
		else prev->right = curr;
		continue;
		match:
		drop(elem.val);
	}
	return ext(T_TABLE, table, (void*)table_free, (void*)table_print);
}

static LObj* _table_get(struct Table* table, int key_len, const char* key) {
	if (table == NULL) return nil();
	while (table != NULL) {
		int comp = strlen(key) - key_len;
		if (comp == 0) comp = memcmp(key, table->key, key_len);
		if (comp == 0) return clone(table->val);
		table = comp < 0 ? table->left : table->right;
	}
	return nil();
}

LObj* constotable(LObj*) __asm__("\"cons->table\"");
LObj* constotable(LObj* o) {
	if (is_nil(o)) return nil();
	if (!is_cons(o)) return err(ER_SYNTAX, "cons->table", "bad args");
	struct Table* table = NULL;
	while (is_cons(o)) {
		LObj* arg = car_unowned(o);
		if (!is_cons(arg)) {
			table_free(table);
			return err(ER_SYNTAX, "cons->table", "expected (key val); got other");
		}
		if (!is_cons(cdr_unowned(arg))) {
			table_free(table);
			return err(ER_SYNTAX, "cons->table", "expected (key val); got (key) or (key . ??\?)");
		}
		if (!is_nil(cdr_unowned(cdr_unowned(arg)))) {
			table_free(table);
			return err(ER_SYNTAX, "cons->table", "expected (key val); got (key val . ??\?)");
		}
		LObj* lkey CLEANUP = Eval(car_unowned(arg));
		if (!is_str(lkey)) {
			table_free(table);
			return is_err(lkey) ? clone(lkey) : err(ER_TYPE, "cons->table", "expected (\"key\" val); got (??? val)");
		}
		LObj* lval = Eval(car_unowned(cdr_unowned(arg)));

		char* key = malloc(str_len(lkey)+1);
		strcpy(key, name(lkey));

		struct Table elem = { .key = key, .val = lval };
		if (table == NULL) {
			table = malloc(sizeof(struct Table));
			*table = elem;
			goto end;
		}
		int comp;
		struct Table* curr = table;
		struct Table* prev = curr;
		while (curr != NULL) {
			prev = curr;
			comp = strcmp(elem.key, curr->key);
			if (comp == 0) goto match;
			curr = comp < 0 ? prev->left : prev->right;
		}
		curr = malloc(sizeof(struct Table));
		*curr = elem;
		if (comp < 0) prev->left = curr;
		else prev->right = curr;
		goto end;
		match:
		drop(lval);
		end:
		o = cdr_unowned(o);
	}
	return ext(T_TABLE, table, (void*)table_free, (void*)table_print);
}

LObj* tableq(LObj*) __asm__("\"table?\"");
LObj* tableq(LObj* o) {
	if (!is_cons(o)) return err(ER_SYNTAX, "table?", "expected one argument; got none");
	if (!is_nil(cdr_unowned(o))) return err(ER_SYNTAX, "table?", "expected one argument; got more");
	LObj* inp CLEANUP = Eval(car_unowned(o));
	return is_ext(inp) && ext_type(inp)==T_TABLE ? tok("T") : nil();
}

LObj* tabledotget(LObj*) __asm__("\"table.get\"");
LObj* tabledotget(LObj* o) {
	if (!is_cons(o)) return err(ER_SYNTAX, "table.get", "expected two arguments; got none");
	if (!is_cons(cdr_unowned(o))) return err(ER_SYNTAX, "table.get", "expected two arguments; got one");
	if (!is_nil(cdr_unowned(cdr_unowned(o)))) return err(ER_SYNTAX, "table.get", "expected two arguments; got more");
	LObj* ltable CLEANUP = Eval(car_unowned(o));
	if (!is_ext(ltable) || ext_type(ltable)!=T_TABLE) return is_err(ltable) ? clone(ltable) : err(ER_TYPE, "table.get", "expected table first arg; got other type");
	struct Table* table = ext_ptr(ltable);
	LObj* lkey CLEANUP = Eval(car_unowned(cdr_unowned(o)));
	if (!is_str(lkey)) return is_err(lkey) ? clone(lkey) : err(ER_TYPE, "table.get", "expected string second argument; got other type");
	return _table_get(table, str_len(lkey), name(lkey));
}

LObj* filedotstat(LObj*) __asm__("\"file.stat\"");
LObj* filedotstat(LObj* o) {
	if (!is_cons(o)) return err(ER_SYNTAX, "file.stat", "expected one argument; got none");
	if (!is_nil(cdr_unowned(o))) return err(ER_SYNTAX, "file.stat", "expected one argument; got more");
	LObj* lfile CLEANUP = Eval(car_unowned(o));
	if (!is_ext(lfile) || ext_type(lfile) != T_FILE) return is_err(lfile) ? clone(lfile) : err(ER_TYPE, "file.stat", "expected file; got other type");
	FILE* file = ext_ptr(lfile);
	struct stat buf;
	if (fstat(fileno(file), &buf)) return err(-errno, "file.stat", strerror(errno));
	char* keys[] = {
		strdup("dev"),		strdup("ino"),
		strdup("mode"),	strdup("nlink"),
		strdup("uid"),		strdup("gid"),
		strdup("size"),	strdup("blksize"),
		strdup("blocks"),	strdup("atime"),
		strdup("mtime"),	strdup("ctime")
	};
	LObj* vals[] = {
		integ(buf.st_dev),		integ(buf.st_ino),
		integ(buf.st_mode),		integ(buf.st_nlink),
		integ(buf.st_uid),		integ(buf.st_gid),
		integ(buf.st_size),		integ(buf.st_blksize),
		integ(buf.st_blocks),	integ(buf.st_atime),
		integ(buf.st_mtime),		integ(buf.st_ctime)
	};
	return _table(12, keys, vals);
}

//new
LObj* filedotsend(LObj*) __asm__("\"file.send\"");
LObj* filedotsend(LObj* o) {
	if (!is_cons(o)) return err(ER_SYNTAX, "file.send", "expected two args; got none");
	if (!is_cons(cdr_unowned(o))) return err(ER_SYNTAX, "file.send", "expected two args; got one");
	if (!is_nil(cdr_unowned(cdr_unowned(o)))) return err(ER_SYNTAX, "file.send", "expected two args; got more");
	LObj* lsrc CLEANUP = Eval(car_unowned(o));
	if (!is_ext(lsrc) || ext_type(lsrc) != T_FILE) return is_err(lsrc) ? clone(lsrc) : err(ER_TYPE, "file.send", "expected two files; got other type");
	FILE* src = ext_ptr(lsrc);
	LObj* ldst CLEANUP = Eval(car_unowned(cdr_unowned(o)));
	if (!is_ext(ldst) || ext_type(ldst) != T_FILE) return is_err(ldst) ? clone(ldst) : err(ER_TYPE, "file.send", "expected two files; got other types");
	FILE* dst = ext_ptr(ldst);
	return sendfile(fileno(src), fileno(dst), NULL, 0) >= 0 ? nil() : err(-errno, "file.send", strerror(errno));
}

LObj* filedotwritedotchar(LObj*) __asm__("\"file.write.char\"");
LObj* filedotwritedotchar(LObj* o) {
	if (!is_cons(o)) return err(ER_SYNTAX, "file.write.char", "expected two args; got none");
	if (!is_cons(cdr_unowned(o))) return err(ER_SYNTAX, "file.write.char", "expected two args; got one");
	if (!is_nil(cdr_unowned(cdr_unowned(o)))) return err(ER_SYNTAX, "file.write.char", "expected two args; got more");

	LObj* lfile CLEANUP = Eval(car_unowned(o));
	if (!is_ext(lfile) || ext_type(lfile) != T_FILE) return is_err(lfile) ? clone(lfile) : err(ER_TYPE, "file.write.char", "expected file first arg; got other type");
	FILE* file = ext_ptr(lfile);

	LObj* lchr CLEANUP = Eval(car_unowned(cdr_unowned(o)));
	if (!is_int(lchr)) return is_err(lchr) ? clone(lchr) : err(ER_TYPE, "file.write.char", "expected char second arg; got other type");
	char chr = int_val(lchr);

	return fputc(chr, file) == EOF ? tok("EOF") : nil();
}

LObj* filedotwritedotstr(LObj*) __asm__("\"file.write.str\"");
LObj* filedotwritedotstr(LObj* o) {
	if (!is_cons(o)) return err(ER_SYNTAX, "file.write.str", "expected two args; got none");
	if (!is_cons(cdr_unowned(o))) return err(ER_SYNTAX, "file.write.str", "expected two args; got one");
	if (!is_nil(cdr_unowned(cdr_unowned(o)))) return err(ER_SYNTAX, "file.write.str", "expected two args; got more");

	LObj* lfile CLEANUP = Eval(car_unowned(o));
	if (!is_ext(lfile) || ext_type(lfile) != T_FILE) return is_err(lfile) ? clone(lfile) : err(ER_TYPE, "file.write.str", "expected file first arg; got other type");
	FILE* file = ext_ptr(lfile);

	LObj* lstr CLEANUP = Eval(car_unowned(cdr_unowned(o)));
	if (!is_str(lstr)) return is_err(lstr) ? clone(lstr) : err(ER_TYPE, "file.write.str", "expected str second arg; got other type");
	const char* str = name(lstr);
	int len = str_len(lstr);
	int total = fwrite(str, 1, len, file);

	return total == len ? integ(total) : nil();
}

LObj* filedottruncate(LObj*) __asm__("\"file.truncate\"");
LObj* filedottruncate(LObj* o) {
	if (!is_cons(o)) return err(ER_SYNTAX, "file.truncate", "expected two args; got none");
	if (!is_cons(cdr_unowned(o))) return err(ER_SYNTAX, "file.truncate", "expected two args; got one");
	if (!is_nil(cdr_unowned(cdr_unowned(o)))) return err(ER_SYNTAX, "file.truncate", "expected two args; got more");

	LObj* lfile CLEANUP = Eval(car_unowned(o));
	if (!is_ext(lfile) || ext_type(lfile) != T_FILE) return is_err(lfile) ? clone(lfile) : err(ER_TYPE, "file.truncate", "expected file first arg; got other type");
	FILE* file = ext_ptr(lfile);

	LObj* lsize CLEANUP = Eval(car_unowned(cdr_unowned(o)));
	if (!is_int(lsize)) return is_err(lsize) ? clone(lsize) : err(ER_TYPE, "file.truncate", "expected int len second arg; got other type");
	int size = int_val(lsize);
	fflush(file);
	return ftruncate(fileno(file), size) == 0 ? nil() : err(-errno, "file.truncate", strerror(errno));
}

// TODO
char* fmtname(const char* path) {
	struct stat st_buf;
	if (lstat(path, &st_buf) == -1) return NULL;
	mode_t mode = st_buf.st_mode;

	const char* colors = getenv("LS_COLORS");

	bool broken;
	const char* ext;
	size_t suf_len = 0;
	switch (mode & S_IFMT) {
		case S_IFDIR: ext = "di"; suf_len = 1; break;
		case S_IFLNK: {
			broken = access(path, F_OK)!=0;
			ext = broken ? "or" : "ln";
			suf_len = strlen(" -> \x1b[90m\x1b[0m") + st_buf.st_size;
		} break;
		case S_IFIFO: ext = "pi"; break;
		case S_IFSOCK: ext = "so"; break;
		case S_IFBLK: ext = "bd"; break;
		case S_IFCHR: ext = "cd"; break;
		case S_IFREG: {
			if (mode & S_IXUSR) { ext = "ex"; break; }
			ext = strrchr(path, '.');
			if (ext == NULL) ext = "fi";
		} break;
		default: return NULL;
	}

	int ext_len = strlen(ext);

	const char* pre = colors;
	while (pre!=NULL+1 && strncmp(pre, ext, ext_len)!=0) pre = strchr(pre, ':') + 1;
	if (pre == NULL+1) {
		ext = "fi"; ext_len = 2;
		pre = colors;
		while (pre != NULL+1 && strncmp(pre, ext, ext_len)!=0) pre = strchr(pre, ':') + 1;
	}
	if (pre == NULL+1) pre = "";
	pre = strchr(pre, '=') + 1;
	if (pre == NULL+1) pre = "";

	const char* pre_end = strchr(pre, ':');
	if (pre_end == NULL) pre_end = strchr(pre, '\0');
	int pre_len = pre_end - pre;

	int len = strlen("\x1b[m") + pre_len + strlen(path) + strlen("\x1b[0m") + suf_len;
	char* full = malloc(len+1);
	int off = snprintf(full, len + 1, "\x1b[%.*sm%s\x1b[0m", pre_len, pre, path);

	switch (mode & S_IFMT) {
		case S_IFDIR: full[off++] = '/'; full[off++] = '\0'; break;
		case S_IFLNK: {
			int buf_len = st_buf.st_size;
			char buf[buf_len];
			int bl = readlink(path, buf, buf_len);
			off += snprintf(full + off, len - off + 1, " -> \x1b[%sm%.*s\x1b[0m", broken ? "31" : "90", bl, buf);
		} break;
		default: break;
	}

	return full;
}

LObj* filedotformatname(LObj*) __asm__("\"file.format-name\"");
LObj* filedotformatname(LObj* o) {
	if (!is_cons(o)) return err(ER_SYNTAX, "file.format-name", "expected one argument; got none");
	if (!is_nil(cdr_unowned(o))) return err(ER_SYNTAX, "file.format-name", "expected one argument; got more");

	LObj* lpath CLEANUP = Eval(car_unowned(o));
	if (!is_str(lpath)) return is_err(lpath) ? clone(lpath) : err(ER_TYPE, "file.format-name", "expected str path argument; got other type");
	int path_len = str_len(lpath);
	char path[path_len+1];
	memcpy(path, name(lpath), path_len);
	path[path_len] = '\0';

	char* out = fmtname(path);
	if (out == NULL) return err(-errno, "file.format-name", strerror(errno));
	return str_owned(out);
}

#define T_DIR 458

static uint print_dir(DIR* dir, char* buf, uint cap) { return snprintf(buf, cap, "dir:%d", dirfd(dir)); }

LObj* dirdotopen(LObj*) __asm__("\"dir.open\"");
LObj* dirdotopen(LObj* o) {
	if (!is_cons(o)) return err(ER_SYNTAX, "dir.open", "expected one argument; got none");
	if (!is_nil(cdr_unowned(o))) return err(ER_SYNTAX, "dir.open", "expected one argument; got more");
	LObj* lpath CLEANUP = Eval(car_unowned(o));
	if (!is_str(lpath)) return is_err(lpath) ? clone(lpath) : err(ER_TYPE, "dir.open", "expected str path argument; got other type");
	int path_len = str_len(lpath);
	char path[path_len + 1];
	path[path_len] = '\0';
	for (int i = 0; i < path_len; i++) path[i] = name(lpath)[i];

	DIR* dir = opendir(path);
	if (dir == NULL) return err(-errno, "dir.open", strerror(errno));
	return ext(T_DIR, dir, (void*)closedir, (void*)print_dir);
}

LObj* dirq(LObj*) __asm__("\"dir?\"");
LObj* dirq(LObj* obj) {
	LObj* arg CLEANUP = Eval(try(get_arg(obj, "dir?")));
	return is_ext(arg) && ext_type(arg)==T_DIR ? tok("T") : NULL;
}

LObj* dirdotfiles(LObj*) __asm__("\"dir.files\"");
LObj* dirdotfiles(LObj* o) {
	if (!is_cons(o)) return err(ER_SYNTAX, "dir.files", "expected one argument; got none");
	if (!is_nil(cdr_unowned(o))) return err(ER_SYNTAX, "dir.files", "expected one argument; got more");
	LObj* ldir CLEANUP = Eval(car_unowned(o));
	if (!is_ext(ldir) || ext_type(ldir) != T_DIR) return is_err(ldir) ? clone(ldir) : err(ER_TYPE, "dir.files", "expected dir argument; got other type");
	DIR* dir = ext_ptr(ldir);

	LObj* vals[3] = {0};
	LObj* ents = nil();

	for (struct dirent* entry = readdir(dir); entry != NULL; entry = readdir(dir)) {
		vals[0] = integ(entry->d_ino);
		vals[1] = integ(entry->d_type);
		vals[2] = str_owned(strdup(entry->d_name));
		char* keys[4] = { strdup("ino"), strdup("type"), strdup("name") };
		LObj* ent = _table(3, keys, vals);
		ents = unowned_cons(ent, ents);
	}

	return ents;
}

LObj* rldotreadline(LObj*) __asm__("\"rl.readline\"");
LObj* rldotreadline(LObj* o) {
	if (!is_cons(o)) return err(ER_SYNTAX, "rl.readline", "expected one argument; got none");
	if (!is_nil(cdr_unowned(o))) return err(ER_SYNTAX, "rl.readline", "expected one argument; got more");

	LObj* lstr CLEANUP = Eval(car_unowned(o));
	if (!is_str(lstr)) return is_err(lstr) ? clone(lstr) : err(ER_TYPE, "rl.readline", "expected str prompt argument; got other type");
	int len = str_len(lstr);
	char str[len+1];
	memcpy(str, name(lstr), len);
	str[len] = '\0';

	char* line = readline(str);
	if (line == NULL) return nil();
	add_history(line);

	return str_owned(line);
}

LObj* rldothistory(LObj*) __asm__("\"rl.history\"");
LObj* rldothistory(LObj* o) {
	if (!is_nil(o)) return err(ER_SYNTAX, "rl.history", "expected no arguments; got more");

	HIST_ENTRY** cur = history_list();
	int len = history_length;

	LObj* l = nil();
	for (int i = 0; i < len; i++) l = unowned_cons(str_owned(strdup(cur[i]->line)), l);

	return l;
}
