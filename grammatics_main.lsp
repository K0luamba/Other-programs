;детерминированные
(setq L1 '((#\H #\a #\A) (#\A #\b #\B) (#\A #\c #\B) (#\A #\d #\D) (#\B #\a #\D) (#\D #\e #\S)))
(setq S1 "HaA;AbB;AcB;AdD;BaD;DeS;")
(setq L2 '((#\H #\a #\H) (#\H #\b #\B) (#\H #\d #\D) (#\B #\c #\H) (#\B #\l #\M) (#\B #\z #\S) 
(#\B #\f #\D) (#\B #\g #\D) (#\D #\c #\H) (#\D #\o #\B) (#\D #\p #\B) (#\D #\z #\S) 
(#\D #\h #\N) (#\M #\n #\B) (#\N #\i #\D)))
;недетерминированные
(setq L3 '((#\H #\a #\B) (#\H #\a #\C) (#\H #\c #\F) (#\B #\o #\S) (#\C #\o #\S) (#\C #\m #\M) (#\F #\d #\C) (#\M #\c #\C)))
(setq L4 '((#\H #\a #\M) (#\H #\e #\G) (#\M #\b #\B) (#\M #\b #\C) (#\M #\c #\D) (#\M #\c #\E) (#\G #\l #\E)
(#\E #\f #\F) (#\F #\f #\E) (#\E #\f #\V) (#\V #\f #\E) (#\B #\o #\S) (#\C #\o #\S) (#\D #\o #\S) (#\E #\o #\S)))
(setq L5 '((#\H #\m #\A) (#\A #\b #\M) (#\A #\b #\N) (#\A #\b #\O) (#\A #\b #\P) (#\M #\p #\B) (#\B #\f #\S)
(#\N #\f #\S) (#\O #\f #\S) (#\P #\f #\S)))
(setq A1 '((#\H #\a #\A) (#\H #\a #\B) (#\A #\b #\S) (#\B #\a #\S)))
(setq A2 '((#\H #\a #\A) (#\H #\a #\B) (#\A #\b #\S) (#\A #\a #\A) (#\B #\b #\B) (#\B #\a #\S))) 
(setq A3 '((#\H #\a #\A) (#\A #\b #\A)  (#\A #\b #\B) (#\B #\a #\B) (#\B #\a #\A) (#\B #\b #\S)))
(setq A4 '((#\H #\a #\A) (#\H #\a #\B) (#\H #\b #\B) (#\A #\a #\A) (#\A #\a #\S) (#\B #\a #\A) (#\B #\a #\B) (#\B #\b #\S)))
(setq A5 '((#\H #\a #\B) (#\H #\a #\C) (#\C #\b #\B) (#\C #\c #\S) (#\B #\b #\C) (#\B #\b #\S)))
(setq A6 '((#\H #\a #\A) (#\A #\b #\B) (#\A #\b #\A) (#\B #\a #\B) (#\B #\a #\A) (#\B #\b #\S)))
(setq A7 '((#\H #\a #\B) (#\B #\b #\B) (#\B #\a #\A) (#\A #\b #\B) (#\B #\a #\S)))
(setq A8 '((#\H #\a #\A) (#\H #\b #\B) (#\B #\b #\B) (#\B #\a #\S) (#\A #\a #\A) (#\A #\b #\S)))

(defun reduceR (f b l)
    (cond ((null L) b)
          (T (funcall f (car l) (ReduceR f b (cdr l))))
    )
)

;выводит список без скобок, пробелов, в конце добавляет ;
(defun printList (l)
	(cond ((null l) (princ '|;|))
           ((listp (car l)) (princ '|(|) (printState (car l)) (printList (cdr l)))
		   ((printsymb (car l)) (printList (cdr l)))
	)
)

(defun printSymb (x)
	(cond ((eq x '@) (princ '|;|))
		   ((eq x '%) (princ '\|))
		   (T (write-char x))
	)
)

;корректный вывод списка-состояния (B S) и т.п.
(defun printState (l)
   (cond ((null l) (princ '|)|))
         (T (write-char (car l)) (printState (cdr l)))
   )
)

(defun deleteRepeat (l)
	(cond  ((not (listp l)) l)
		   ((null (cdr l)) l)
		   (T (cons (car l) (deleteRepeat(deleteInList (car l) (cdr l)))))
	)
)

;на вход - весь список-граф
(defun delete2level (l)
	(cond ((null l) NIL)
		  (T (cons (list (delete2level1 (caar l)) (cadar l) (delete2level1 (caddar l))) (delete2level (cdr l))))
	)
)

;работа непосредственно с одним звеном ((A B) S (D E)) => (A B S D E)
(defun delete2level1(l)
	(cond ((null l) NIL)
          ((not (listp l)) l)
		  ((listp (car l)) (append (car l) (delete2level1 (cdr l))))
		  (T (cons (car l) (delete2level1 (cdr l))))
	)
)

;удаляет все вхождения элемента x в списке l
(defun deleteInList (x l)
	(cond ((null l) NIL)
		  ((equal x (car l)) (deleteInList x (cdr l)))
		  (T (cons (car l) (deleteInList x (cdr l))))
	)
)

;создает список терминальных символов из списка-графа => T=...;
(defun termSymb (l)
	(append (append '(#\T #\=) (deleteRepeat(mapcar 'cadr l))) (list '@))
)

;создает список нетерминальных символов из списка-графа => N=...;
(defun nonTermSymb (l)
	(append '(#\N #\=) (append (deleteRepeat(append (mapcar 'car l) (list '#\S)))(list '@)))
)

;возвращает списком списоков то, что будет стоять в правой части X=...|...;
(defun listOfDest(x l)
	(deleteInList NIL (mapcar #'(lambda (y) (cond ((equal x (car y)) (cond ((equal (caddr y) '#\S) (list (cadr y))) 
																		 (T (cdr y)))))) l))
)

;обработка того, что нам вернули, выдает список типа (b B | c B | d D ;)
(defun rigthPart (l)
	(cond ((null (cdr l)) (append (car l) (list '@)))
		  (T (append (car l) (list '%) (rigthPart (cdr l))))
	)
)

;выводит в требуемом виде все альтернативы для нетерминала x из списка-графа
(defun rules (x l)
	(append (list x) (list '#\=) (rigthPart(listOfDest x L))) 
)

;формирует правила для всех нетерминалов (a-список нетерминалов)
(defun allRules (a l)
	(cond ((null (cdr a)) (rules (car a) l))
		  ;((equal (car a) 'H) (allRules (cdr a) l))
		  (T (append (rules (car a) l) (allRules (cdr a) l)))
	)
)

;собирает все компоненты грамматики в список для дальнейшего вывода
(defun formGramm (l)
    (cond ((equal l NIL) (print NIL))
          (T (printList(append (termSymb l) (nonTermSymb l) (allRules (deleteRepeat(mapcar 'car l)) l) (list '#\H))))
    )
)

;====================== далее все по ДЕТЕРМИНИЗАЦИИ ======================

;возвращает все звенья вида (X a ...)
(defun searchByTwo (x a l)
	(cond ((null l) NIL)
		  ((and (equal x (caar l)) (equal a (cadar l))) (cons (car l) (searchByTwo x a (cdr l))))
		  (T (searchByTwo x a (cdr l)))
	)
)

(defun memberl (x l)
	(cond ((null l) NIL)
		  ((equal x (car l)) T)
		  (T (memberl x (cdr l)))
	)
)

;объединяет 2+ звеньев из недетерминированной части автомата
(defun zipStates (a)
	(append (list(caar a)) (list(cadar a)) (list(mapcar 'caddr a)))
)

(defun checkZip (a)
	(cond ((equal (length(caddr a)) 1) (list (append (list (car a)) (list (cadr a)) (caddr a))))
		  ((null (car a)) NIL)
		  (T (list a))
	)
)

;оставляет те элементы первого списка, которых нет во втором (S по умолчанию не возвращаем)
(defun subsection (v l)
	(cond ((null v) NIL)
		  ((or (memberl (car v) l) (eq (car v) #\S)) (subsection (cdr v) l))
		  ((listp (car v)) (cond ((memberl (reverse(car v)) l) (subsection (cdr v) l)) (T (cons (car v) (subsection (cdr v) l)))))
		  (T (cons (car v) (subsection (cdr v) l)))
	)
)

;ВХОД на перевод автомата НКА -> ДКА
(defun determine (l)
	(maincycle (list #\H) (list #\H) (deleteRepeat(mapcar 'cadr l)) l NIL)
)

;N = столбец нетерминалов (изначально H); N1 = непроверенные нетерминалы; Y = терминалы (const); A = изначальный автомат (const); D = детерменированный автомат
(defun maincycle (N N1 Y A D)
;	(print N)
;	(print N1)
;	(print Y)
;	(print A)
;	(print D)
;	(terpri)
	(cond ((null N1) (checkDetermine D N)) ;условие выхода - кончились непроверенные состояния
		  (T (let ((L (returnNodes (car N1) Y A)))
				(maincycle (append N (subsection (deleteRepeat(mapcar 'caddr L)) N)) (append (cdr N1) (subsection (deleteRepeat(mapcar 'caddr L)) N)) Y A (append D L))
			  )
		   )
	)
)

(defun checkDetermine (D N)
	(mapcar #'(lambda (l) (cond ((listp (caddr l)) (cond ((and (not(memberl (caddr l) N)) (memberl (reverse (caddr l)) N)) (list (car l) (cadr l) (reverse(caddr l))))
														 (T l)
													)
								)
								(T l)
						  )
			  ) D)
)

;возвращает для нетерминала X все звенья из автомата A вида (X y ...), терминал y из Y
(defun returnNodes (X Y A)
	(cond ((null Y) NIL)
		  ((atom X) (append (checkZip(zipStates(searchByTwo X (car Y) A))) (returnNodes X (cdr Y) A))) 
		  (T (append (checkComplex(makeComplex X (car Y) A)) (returnNodes X (cdr Y) A)))
	)
)

(defun checkComplex (a)
	(cond ((null a) NIL)
          ((listp (caddar a)) (cond ((equal (caar a) (reverse (caddar a))) (list(append (list (caar a)) (list (cadar a)) (list (caar a)))))
								    ((equal (caar a) (list (nth 1 (caddar a)) (nth 2 (caddar a)) (nth 0 (caddar a)))) (list(append (list (caar a)) (list (cadar a)) (list (caar a)))))
                                    (T a)
                              )
          )
          (T a)
	)
)

;для списка состояний выдает одно объединенное звено типа ((A B) a (A S))
(defun makeComplex (X y A)
	(let ((L (checkZip(zipStates(complexNode X y A))))) (cond ((null L) NIL) 
										 (T (list(append (list X) (list (cadar L)) (list (deleteRepeat(caddar L))))))
									)
	)
)


(defun complexNode (X y A)
	(cond ((null X) NIL)
		  (T (append (searchByTwo (car X) y A) (complexNode (cdr X) y A)))
	)
)

(defun main (L)
	(formGramm (determine L))
)

;позволяет задавать автомат строкой
(defun transform (s)
	(toGraph(coerce s 'list))
)

(defun toGraph (l)
	(cond ((null l) NIL)
		  (T (cons (append(list (car l)) (list (cadr l)) (list (caddr l))) (toGraph (cddddr l))))
	)
)

(main L4)