;;(setq debug-on-error t)
(defvar gdev:debug nil)

(require 'json)


(defvar gdev:root-dir "~/.emacs-gdev")

;;
;; util functions

(defun gdev:any (pred seq)
  (let ((ret nil))
    (while (and (not ret) seq)
      (when (funcall pred (car seq))
	(setq ret (car seq)))
      (setq seq (cdr seq)))
    ret))

(defun gdev:foldl (f seed seq)
  (dolist (o seq seed)
    (setq seed (funcall f seed o))))

(defun gdev:filter (pred seq)
  (let ((ret))
    (dolist (x seq (nreverse ret))
      (and (funcall pred x) (push x ret)))))

(defun gdev:filter-map (pred seq)
  (let ((ret))
    (dolist (x seq (nreverse ret))
      (let ((r (funcall pred x)))
	(when r
	  (push r ret))))))
	    
(defun gdev:hash-keys (hash)
  (let ((c nil))
    (maphash
     (lambda (k o) (setq c (cons k c)))
     hash)
    (nreverse c)))

(defun gdev:hash-values (hash)
  (let ((c nil))
    (maphash
     (lambda (k o) (setq c (cons o c)))
     hash)
    (nreverse c)))


;;
;; document management

(defvar gdev:doc-table (make-hash-table :test 'equal))
(defvar gdev:each-buf-data (make-hash-table :test 'equal))

(defun gdev:add-doc (name filepath units)
  (when gdev:debug
    (message "add-doc")
    (message "name %s" name)
    (message "filepath %s" filepath))
  (let ((filepath (if (zerop (length filepath))
		      (if (string-match "^#" name)
			  ;;TODO ????
			  (substring name 2)
			"")
		    filepath))
	(units (append units nil))) ;convert vector -> list
    (mapc
     (lambda (unit)
       (setcdr unit (append
		     (list (cons 'docnsme (replace-regexp-in-string " " "\\ " name))
			   (cons 'filepath (replace-regexp-in-string " " "\\ " filepath))
			   (cons 'laded-doc nil))
		     (cdr unit))))
     units)
    (puthash name units gdev:doc-table)))

(defun gdev:set-buf-data (buf-key key data)
  (when gdev:debug
    (message "set-buf-data buf-key %s, key %s" buf-key key))
  (let ((buf-hash (gethash buf-key gdev:each-buf-data)))
    (if buf-hash
	(puthash key data buf-hash)
      (let ((buf-hash (make-hash-table :test 'equal)))
	(puthash key data buf-hash)
	(puthash buf-key buf-hash gdev:each-buf-data)))))

(defun gdev:get-buf-data (buf-key key)
  (when gdev:debug
    (message "get-buf-data buf-key %s, key %s" buf-key key))
  (let ((buf-hash (gethash buf-key gdev:each-buf-data)))
    (if buf-hash
	(gethash key buf-hash)
      nil)))

(defun gdev:set-module-order (buf-key order)
  (when gdev:debug
    (message "set-module-order buf-key %s, order %s" buf-key order))
  (gdev:set-buf-data buf-key 'order order))

(defun gdev:get-module-order (buf-key)
  (when gdev:debug
    (message "get-module-order buf-key %s" buf-key))
  (let ((order (gdev:get-buf-data buf-key 'order)))
    (if order
	order
      (gdev:hash-keys gdev:doc-table))))

(defun gdev:match-unit-in-order-first-match-priority-exact-match (buf-key keyword allow-duplicate)
  (when gdev:debug
    (message "match-unit-in-order-first-match-priority-exact-match buf-key %s, keyword %s" buf-key keyword))
  (let* ((units (gdev:match-unit-in-order-first-match buf-key keyword allow-dbuplicate))
	 (ret units)
	 (len (length keyword)))
    (while units
      (if (= (length (assoc-default 'n (car units))) len)
	  (progn
	    (setq ret (car units))
	    (setq units nil))
	(setq units (cdr units))))
    ret))

(defun gdev:match-unit-in-order-first-match (buf-key keyword allow-duplicate)
  (when gdev:debug
    (message "match-unit-in-order-first-match buf-key %s, keyword %s" buf-key keyword))
  (if allow-duplicate
      (gdev:match-unit-in-order-allow-duplicate buf-key keyword 'gdev:first-match-filter)
    (gdev:match-unit-in-order-no-duplicate buf-key keyword 'gdev:first-match-filter)))

(defun gdev:match-unit-in-order (buf-key keywrod allow-duplicate)
  (when gdev:debug
    (message "match-unit-in-order buf-key %s, keyword %s" buf-key keywrod))
  (if allow-duplicate
      (gdev:match-unit-in-order-allow-duplicate buf-key keywrod 'gdev:unit-name-head-filter)
    (gdev:match-unit-in-order-no-duplicate buf-key keyword 'gdev:unit-name-head-filter)))

(defun gdev:match-unit-in-order-no-duplicate (buf-key keyword comp)
  "[internal]"
  (when gdev:debug
    (message "match-unit-in-order-no-duplicate buf-key %s, keyword %s" buf-key keyword))
  (let ((unit-table (make-hash-table)))
    (mapc
     (lambda (mod)
       (let ((doc (gethash mod gdev:doc-table)))
	(when doc
	  (dolist (unit (if (zerop (length keyword))
			    doc
			  (funcall comp doc keyword)))
	    (puthash (assoc-default 'n unit) unit unit-table)))))
     (gdev:get-module-order buf-key))
    (gdev:hash-values unit-table)))

(defun gdev:match-unit-in-order-allow-duplicate (buf-key keyword comp)
  "[internal]"
  (when gdev:debug
    (message "match-unit-in-order-allow-duplicate buf-key %s, keyword %s" buf-key keyword))
  (let ((unit-table (make-hash-table)))
    (mapc
     (lambda (mod)
       (let ((units (gethash mod gdev:doc-table)))
	(when units
	  (maphash
	   (lambda (k v) (puthash k v unit-table))
	   (gdev:foldl
	    (lambda (acc unit)
	      (let* ((name (assoc-default 'n unit))
		     (item (gethash name acc)))
		(if item
		    (if (consp item)
			(puthash name (cons unit item) acc)
		      (puthash name (list unit item) acc))
		  (puthash name unit acc)))
	      acc)
	    (make-hash-table :test 'equal)
	    (if (zerop (length keyword))
		units
	      (funcall comp units keyword)))))))
     (gdev:get-module-order buf-key))
    (gdev:foldl
     (lambda (acc obj)
       (if (consp obj)
	   (append obj acc)
	 (cons obj acc)))
     nil
     (gdev:hash-values unit-table))))

(defun gdev:first-match-filter (units keyword)
  "[internal]"
  (gdev:filter
   (lambda (u) (string-match (concat "^" keyword) (assoc-default 'n u)))
   units))

(defun gdev:unit-name-head-filter (units keyword)
  "[internal]"
  (gdev:filter
   (lambda (u) (string-match keyword (assoc-default 'n u)))
   units))

;;
;; Communicate gosh-complete

(defvar gdev:process nil)
(defvar gdev:init-count 0)
(setq gdev:process nil)
(setq gdev:init-count 0)

(defun gdev:proc-initialized? ()
  (processp gdev:process))

(defun gdev:init-proc ()
  (when gdev:debug
    (message "init-proc count"))
  (unless gdev:process
    (setq gdev:process
	  (start-process "gdev" nil "gosh"
			 (concat "-I" (expand-file-name gdev:root-dir))
			 (concat (file-name-as-directory (expand-file-name gdev:root-dir)) "gosh_complete.scm")
			 (concat "--generated-doc-directory=" (file-name-as-directory gdev:root-dir) "doc")
			 "--output-name-only"
			 ;;TODO other encoding
		         ;"--io-encoding="
			 ;;TODO supprt restart
			 ;get-load-moudle-text
			 ))
    ;;ignore warning
    (process-kill-without-query gdev:process nil)
    ;;set process filter
    (set-process-filter gdev:process 'gdev:read-from-gauche-complete))
  ;;count up init-count
  (setq gdev:init-count (1+ gdev:init-count)))

(defun gdev:final-proc ()
  (when gdev:debug
    (message "final-proc count %s" gdev:init-count))
  (when gdev:process
    ;;count down init-count
    (setq gdev:init-count (1- gdev:init-count))
    ;;process finish if <= 0
    (when (<= gdev:init-count 0)
      (gdev:write-text "#exit\n")
      (setq gdev:process nil)
      (setq gdev:init-coount 0))))

(defun gdev:write-text (text)
  "[internal]"
  ;;TODO check initialize
  (when gdev:debug
    (message "write-text %s" text))
  (process-send-string gdev:process text))


;;;;;;;
; task structure
; waiting
;(text callback context)
; runtime
;(callback context start-time)
(defvar gdev:task-list nil)
(setq gdev:task-list nil)

(defun gdev:add-async-task (text callback context)
  (when gdev:debug
    (message "add-async-task %s" text))
  (if (zerop (length gdev:task-list))
      (progn
	(gdev:write-text text)
	(setq gdev:task-list (cons
				  (list callback context (cadr (current-time)))
				  nil)))
    (setq gdev:task-list (append
			  gdev:task-list
			  (cons (list text callback context) nil)))))

(defun gdev:empty-async-task? ()
  (zerop (length gdev:task-list)))

(defun gdev:start-next-task ()
  "[internal]"
  (when gdev:debug
    (message "start-next-task %s" (gdev:empty-async-task?)))
  (when gdev:task-list
    (gdev:write-text (caar gdev:task-list))
    (setcar gdev:task-list (append
				(cdar gdev:task-list)
				(cons (cadr (current-time)) nil)))))
			      
(defun gdev:exec-head-task-callback (docs)
  "[internal]"
  (when gdev:debug
    (message "exec-head-task-callback %s, %s" docs gdev:task-list))
  (let ((task (car gdev:task-list)))
    (when (funcall (car task) docs (cadr task))
      (setq gdev:task-list (cdr gdev:task-list))
      (gdev:start-next-task))))

;;TODO check timeout
(defvar gdev:output-list nil)
(defun gdev:read-from-gauche-complete (proc text)
  "[internal] process output filter."
  (when gdev:debug
    (message "read-from-gauche-complete %s" text))
  (let ((l (split-string text "\n")))
    (while (cdr l)
      (gdev:exec-head-task-callback
       (json-read-from-string (mapconcat 'identity (nreverse (cons (car l) gdev:output-list)) "")))
      (setq gdev:output-list nil)
      (setq l (cdr l)))
    (unless (zerop (length (car l)))
      (setq gdev:output-list (cons (car l) gdev:output-list)))))

(provide 'gdev)