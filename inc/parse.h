#pragma once
#include "lisp.h"

enum Result { Ok, Err };
struct Parsed { enum Result result; LObj* value; const char* endptr; };

struct Parsed parse_str(const char*);
struct Parsed parse_file(FILE*);
