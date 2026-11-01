#include <ctype.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "../inc/parse.h"

static int hexval(char c) { return c<'0' ? -1 : c<='9' ? c-'0' : c<'A' ? -1 : c<='F' ? c-'A'+10 : c<'a' ? -1 : c<='f' ? c-'a'+10 : -1; }

static struct Parsed parse_cons_str(const char* s) {
	while (isspace(*s)) s++;
	if (*s == ')') return (struct Parsed){ Ok, nil(), s+1 };
	struct Parsed car = parse_str(s);
	if (car.result == Err) return car;
	s = car.endptr;
	while (isspace(*s)) s++;
	if (*s == '\0') {
		drop(car.value);
		return (struct Parsed){ Err, err(ER_SYNTAX, "PARSE", "unexpected end of string; unmatched paren"), s };
	}
	struct Parsed cdr = parse_cons_str(s);
	if (cdr.result == Err) return cdr;
	struct Parsed ret = { Ok, _cons(car.value, cdr.value), cdr.endptr };
	drop(car.value); drop(cdr.value);
	return ret;
}

static struct Parsed parse_token_str(const char* s) {
	int end = strcspn(s, "() \t\n");
	return (struct Parsed){ Ok, tok_owned(strndup(s, end)), s + end };
}

static struct Parsed parse_string_str(const char* s) {
	int len = 0; const char* si;
	for (si = s; *si!='\0' && *si!='"'; si++) if (*si != '\\') len++;
	if (*si == '\0') return (struct Parsed){ Err, err(ER_SYNTAX, "PARSE", "unexpected end of string; unmatched quotation"), s };

	int ind = 0;
	char* buf = malloc(len+1);
	for (; *s != '"'; s++) {
		if (*s == '\\') switch (*(++s)) {
			case 'a': buf[ind++] = '\a'; break;
			case 'b': buf[ind++] = '\b'; break;
			case 'f': buf[ind++] = '\f'; break;
			case 't': buf[ind++] = '\t'; break;
			case 'n': buf[ind++] = '\n'; break;
			case 'r': buf[ind++] = '\r'; break;
			case 'v': buf[ind++] = '\v'; break;
			case '\\': buf[ind++] = '\\'; break;
			case '"': buf[ind++] = '"'; break;
			case '\'': buf[ind++] = '\''; break;
			case 'x':
				char c = hexval(*(++s));
				c = c*16 + hexval(*(++s));
				buf[ind++] = c; break;
		} else buf[ind++] = *s;
	}
	buf[ind] = '\0';
	return (struct Parsed){ Ok, str_owned(buf), s+1 };
}

struct Parsed parse_str(const char* s) {
	while (isspace(*s)) s++;
	if (*s == '\0') return (struct Parsed){ Err, err(ER_SYNTAX, "PARSE", "unexpected end of string"), s };
	if (*s == '"') return parse_string_str(s+1);
	if (*s == '(') return parse_cons_str(s+1);
	if (isdigit(*s) || *s == '-') {
		bool sign = *s == '-';
		if (*s == '-') {
			s++;
			if (isspace(*s)) return (struct Parsed){ Ok, tok("-"), s };
		}
		int base = 10;
		if (*s == '0') {
			base = 8;
			switch (*(++s)) {
				case 'x': base = 16; s++; break;
				case 'b': base = 2; s++; break;
			}
		}
		long acc = 0;
		while (isxdigit(*s) && (base == 16 || *s < '0' + base)) {
			acc = acc*base + hexval(*s);
			s++;
		}
		return (struct Parsed){ Ok, integ(sign ? -acc : acc), s };
	}
	if (strchr("'`,", *s) != NULL) {
		struct Parsed rest = parse_str(s+1);
		if (rest.result == Err) return rest;
		char* str = *s == '\'' ? "quote" : *s == '`' ? "semiquote" : *s == ',' ? "unquote" : "unreachable";
		return (struct Parsed){ Ok, unowned_cons(tok(str), unowned_cons(rest.value, nil())), rest.endptr };
	}
	return parse_token_str(s);
}

static struct Parsed parse_cons_file(FILE* f) {
	char c = fgetc(f);
	if (c == ')') return (struct Parsed){ Ok, nil() };
	ungetc(c, f);
	struct Parsed car = parse_file(f);
	if (car.result == Err) return car;
	c = ' ';
	while (isspace(c)) c = fgetc(f);
	if (c == '\0' || c == EOF) {
		drop(car.value);
		return (struct Parsed){ Err, err(ER_SYNTAX, "PARSE", "unexpected end of file; unmatched paren") };
	}
	ungetc(c, f);
	struct Parsed cdr = parse_cons_file(f);
	if (cdr.result == Err) { drop(car.value); return cdr; }
	struct Parsed ret = { Ok, _cons(car.value, cdr.value) };
	drop(car.value); drop(cdr.value);
	return ret;
}

static struct Parsed parse_token_file(FILE* f) {
	long cap = 16;
	long len = 0;
	char* buf = malloc(cap);
	char c = fgetc(f);
	while (!strchr("() \t\n\xFF", c)) {
		buf[len++] = c;
		c = fgetc(f);
		if (cap == len) {
			cap *= 2;
			buf = realloc(buf, cap);
		}
	}
	buf[len] = '\0';
	ungetc(c, f);
	return (struct Parsed){ Ok, tok_owned(buf) };
}

static struct Parsed parse_string_file(FILE* f) {
	int len = 0; int cap = 0; char* buf = NULL;
	void append(char c) {
		if (len == cap) {
			cap = cap == 0 ? 10 : cap*2;
			buf = buf ? realloc(buf, cap) : malloc(cap);
		}
		buf[len++] = c;
	}

	for (char c = fgetc(f); !feof(f) && c != '"'; c = fgetc(f)) {
		if (c == '\\') switch (fgetc(f)) {
			case 'a': append('\a'); break;
			case 'b': append('\b'); break;
			case 'f': append('\f'); break;
			case 't': append('\t'); break;
			case 'n': append('\n'); break;
			case 'r': append('\r'); break;
			case 'v': append('\v'); break;
			case '\\': append('\\'); break;
			case '"': append('"'); break;
			case '\'': append('\''); break;
			case 'x':
				char e = hexval(fgetc(f));
				e = e*16 + hexval(fgetc(f));
				append(e); break;
		} else append(c);
	}
	if (feof(f)) {
		free(buf);
		return (struct Parsed){ Err, err(ER_SYNTAX, "PARSE", "unexpected end of file; unmatched quotation") };
	}
	append('\0');
	return (struct Parsed){ Ok, str_owned(buf) };
}

struct Parsed parse_file(FILE* f) {
	struct Parsed ret;
_redo:
	char c = ' ';
	while (isspace(c)) c = fgetc(f);
	if (c == '\0' || c == EOF) ret = (struct Parsed){ Err, err(ER_SYNTAX, "PARSE", "unexpected end of file") };
	else if (c == '#') { while (c!='\n') c = fgetc(f); goto _redo; }
	else if (c == '(') ret = parse_cons_file(f);
	else if (c == '"') ret = parse_string_file(f);
	else if (strchr("'`,", c)!=NULL) {
		struct Parsed rest = parse_file(f);
		if (rest.result == Err) return rest;
		const char* str = c == '\'' ? "quote" : c == '`' ? "semiquote" : c == ',' ? "unquote" : "unreachable";
		return (struct Parsed){ Ok, unowned_cons(tok(str), unowned_cons(rest.value, nil())) };
	} else if (isdigit(c) || c == '-') {
		bool sign = c == '-';
		if (c == '-') { c = fgetc(f); if (isspace(c)) { ret = (struct Parsed){ Ok, tok("-") }; goto _nan; } }
		int base = 10;
		if (c == '0') {
			base = 8;
			switch ((c = fgetc(f))) {
				case 'x': base = 16; break;
				case 'b': base = 2; break;
				default: ungetc(c, f); break;
			}
			c = fgetc(f);
		}
		long acc = 0;
		while (isxdigit(c) && (base == 16 || c < '0' + base)) {
			acc = acc*base + hexval(c);
			c = fgetc(f);
		}
		ret = (struct Parsed){ Ok, integ(sign ? -acc : acc) };
		ungetc(c, f);
	_nan:
	} else {
		ungetc(c, f);
		ret = parse_token_file(f);
	}
	c = ' ';
	while (isspace(c)) c = fgetc(f);
	ungetc(c, f);
	return ret;
}
