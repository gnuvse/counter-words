(setq *counter* 0)

; получаем дескриптор файла
(setq fo (open "D:/yura/Documents/LISP/Projects/sortsrc/test.lsp"
			   "r"
		 )
)

; log
(setq f (open "D:/yura/Documents/LISP/Projects/sortsrc/log.txt"
			   "w"
		 )
)

(defun write-log (symbol word / str)
	(setq str (strcat  " -> symbol = " symbol " // " "word = " word))
	(write-line str f)
)

; является ли символ скобкой
(defun brackets (symbol)
	(if (or (= symbol "(") (= symbol ")"))
		t
		nil
	)
)

; преобразование подстроки в список
(defun substr->list	(str / s)
	(setq s (read (strcat "(" str ")")))
	s
)




; обработка построчно текста из файла
; и преобразование его в список
(defun str->list (str		 /			current-line-pos		   
				  l			 word		symbol	   quotes-counter
				  flag-quotes flag-slash
				 )
	(setq current-line-pos 1)
	(setq l '())
	(setq word "")

	(setq flag-quotes nil)
	(setq flag-slash nil)

	; Пока есть строки в файле, читаем их
	(while (/= (substr str current-line-pos	1) "")
		(setq symbol (substr str current-line-pos 1))
	

		; обработка кавычек
		(if	(= symbol "\"")
			(if (and (= flag-quotes t) (= flag-slash nil))
				(progn
					(setq word (strcat word symbol))
					(write-log symbol word)
					(setq l (append l (substr->list word)))
					(setq word "")
					(setq symbol " ")

					; обнуляем, чтобы снова не заходить в условие
					(setq flag-quotes nil)
					(setq flag-slash nil)
				 )
				(progn
					(setq flag-quotes t)
					(setq flag-slash nil)
				)
			)
		)
		
		(if	(and (= symbol "\\") (= flag-quotes t))
			(if (= flag-slash t)
				(progn
					(setq flag-slash nil)
				)
				(progn
					(setq flag-slash t)
				)
			)
		)
	
		
		(if	(and (not flag-quotes) (not flag-slash)) 
			(if	(and (/= symbol " ") (not (brackets symbol)))
				(progn
					(setq word (strcat word symbol))
					(write-log symbol word)
				)
				(if (and (brackets symbol) (/= symbol "\""))
					(progn)
					(progn
						(setq l (append l (substr->list word)))
						(setq word "")
						(write-log symbol word)
					)
				)
			)
			(progn
				(setq word (strcat word symbol))
				(write-log symbol word)
			)
		) ; end if



		
		(setq current-line-pos (1+ current-line-pos))
	) ; end while
	

	
	(setq l (append l (substr->list word)))
	l
)


; подсчет слов
(defun words-counter (fo counter / line)
	(setq line " ")
	(while (setq line (read-line fo))
		(setq line (str->list line))
		(princ "\n:TYPE  - > ")
		(princ (type line))
		(princ "\n:VALUE - > ")
		(princ line)
		(princ)
		(setq *counter* (+ *counter* (length line)))
	)								; end while
)									; end defun


(words-counter fo *counter*)
(print *counter*)
