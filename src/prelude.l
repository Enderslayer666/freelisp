(extern "../bin/prelude.so" '(cons nil? cons? err? func? tok? str? int? = error apply && || ^^ & | ^ + * - / % zero? pos? neg? even? cond lambda lambda-macro display type car cdr par seq loop
	cons->str str->cons str.len str.input str.cat str.at str.find str.parse str.sh str<- str.rep
	file.access file.open file.read.full file.read.line file.read.str file.read.char file.write.char file.write.str file.seek file.skip file.skip.line file.format-name
		file.err? file.eof? file? file.parse file.stat file.descriptor file.send file.truncate
	file.stdin file.stdout file.stderr
	cons->table table.get table?
	sys sys.pid sys.cwd sys.cd sys.chdir sys.mv sys.exec sys.sleep sys.exit
	dir.open dir.files dir?
	rl.readline rl.history
))

(cons 'a 'b)

(define defmacro (lambda-macro (name args body) (define name (lambda-macro args body))))
(defmacro defun (name args body) (define name (lambda args body)))

(defmacro if (c t f) (cond (c t) ('T f)))

(defun map (f l) (
	cond
		((cons? l)
			(cons
				(f (car l))
				(map f (cdr l))))
		((err? l) l)
		('T '())
	))

(define mapl map)

(defun Y (f) ((lambda (x) (f (x x))) (lambda (x) (f (x x)))))

(defun parmap (f l) (
	cond
		((cons? l)
			(par
				(f (car l))
				(map f (cdr l))))
		((err? l) l)
		('T '())
	))

(defun foldr (f a c) (cond
	((err? f) f) ((err? a) a) ((err? c) c)
	((nil? c) a)
	((cons? c) (f (car c) (foldr f a (cdr c))))
	('T (error "foldr" "expected cons list third argument; or unexpected dotted tail"))
))

(defun foldl (f a c) (cond
	((err? f) f) ((err? a) a) ((err? c) c)
	((nil? c) a)
	((cons? c) (foldl f (f a (car c)) (cdr c)))
	('T (error "foldl" "expected cons list third argument; or unexpected dotted tail"))
))

(defun filter (f c) (cond 
	((err? f) f) ((err? c) c)
	((nil? c) c)
	((cons? c) (if (f (car c)) (cons (car c) (filter f (cdr c))) (filter f (cdr c))))
	('T (error "filter" "expected cons list second argument; or unexpected dotted tail"))
))

(defun filter-map (f c) (cond
	((err? f) f) ((err? c) c)
	((nil? c) c)
	((cons? c) (let v (f (car c)) (if v (cons v (filter-map f (cdr c))) (filter-map f (cdr c)))))
	('T (error "filter-map" "expected cons list second argument; or unexpected dotted tail"))
))

(defmacro do (l) (mapl eval 'l))
(defmacro rep (n f e) (
	if (zero? n) e
		(f (rep (- n 1) f e))
))
(defmacro repeat (n f) (
	if (zero? n) '()
		(seq (f) (repeat (- n 1) f))
))

(defmacro list (. l) (map eval (quote l)))
(defmacro lets (vs body) ((lambda (var vdr) ((lambda var body) vdr)) (map car vs) (map cdr vs)))
(defmacro let (n v body) ((lambda (n) body) v))

(defun print (l) (mapl (lambda (e) (display (eval e))) l))
(defun file.skip.lines (file n)
	(repeat n (file.skip.line file))
)

(defun abs-path (p) (sys.cd p (sys.cwd)))
(defun sys.ls (. args) (let path (cond ((cons? args) (car args)) ((nil? args) ".") ('T (error "sys.ls" "unexpected dotted tail")) )
	(map
		(lambda (ent) (table.get ent "name"))
		(dir.files (dir.open path)))
))
