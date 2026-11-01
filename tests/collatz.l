#!/bin/env freelisp

(import /usr/share/freelisp-libs/prelude.l)

(defun collatz-step (prev) (
	if (even? prev)
		(/ prev 2)
		(+ 1 (* 3 prev))
))

(defun collatz (seed) (if (= 1 seed)
	(quote (1))
	(cons seed (collatz (collatz-step seed)))
))

(defun collatz-inv (seed) (cons (* seed 2) (if (= 4 (% seed 6)) (cons (/ (- seed 1) 3) nil) nil)))

# (seed . sub) or (seed sub1 . sub2)
(defun collatz-tree-step (tree) (if (cons? tree)
	(cons (car tree) (cond
		((cons? (cdr tree)) (parmap collatz-tree-step (cdr tree)))
		((err? (cdr tree)) (cdr tree))
		(T (collatz-tree-step (cdr tree)))
	))
	(cons tree (collatz-inv tree))
))

(defun collatz-tree (seed inc) (if (zero? inc) seed (collatz-tree (collatz-tree-step seed) (- inc 1))))

(print '(
	"collatz is { odd: 3n+1, even: n/2 }, iterated\n"
	"collatz 27:\n\t"
	(collatz 27) "\n"
	"when it hits one, that loops back to four\n"
	"it is conjectured that every seed falls into this loop\n"
	"\nnow, inverting that step gives branches\n"
	"we draw these trees starting at 16, becuase otherwise it loops:\n\t"
	(collatz-tree 1 5) "\n"
	"deeper collatz tree:\n\t"
	(collatz-tree 16 12) "\n"
))
