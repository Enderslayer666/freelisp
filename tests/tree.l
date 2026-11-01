#!/bin/env freelisp
(import "/usr/share/freelisp-libs/prelude.l")

(defun tree (depth) (map 
	(lambda (ent) (cond
		((= 0x2E (str.at (table.get ent "name") 0)) '())
		((= 4 (table.get ent "type"))
			(seq
				(print '((str.rep depth "   ") (file.format-name (table.get ent "name")) "\n"))
				(sys.cd (table.get ent "name") (tree (+ depth 1))) ))
		('T (print '((str.rep depth "   ") (file.format-name (table.get ent "name")) "\n")) )))
	(dir.files (dir.open "."))
))

(let path (if (nil? $1) "." $1) (seq
	(print '((file.format-name path) "\n"))
	(sys.cd path (tree 1)) ))
