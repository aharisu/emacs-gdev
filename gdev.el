;(setq debug-on-error t)
(defvar gdev:debug nil)

(require 'json)


(defvar gdev:root-dir "~/.emacs-gdev")
(defvar gdev:enable-gdev t)
(defvar gdev:task-timeout-sec 7) ;7 seconds

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
;; hooks

(defun gdev:scheme-mode-hooks ()
  "[internal]"
  (when gdev:debug
    (message "gdev:scheme-mode-hooks")
    (message (buffer-name))
    (message buffer-file-name)
    (message mode-name))
  (when gdev:enable-gdev
    (gdev:init-proc)
    (gdev:load-default-module)
    (gdev:parse-cur-buf)))
;;register scheme-mode hook
(add-hook 'scheme-mode-hook 'gdev:scheme-mode-hooks)

(defun gdev:after-save-hooks ()
  "[internal]"
  (when gdev:debug
    (message "gdev:after-save-hooks")
    (message mode-name))
  (when (and gdev:enable-gdev
	     (string= "Scheme" mode-name))
    (gdev:parse-cur-buf-from-file)))
;;register after-save-hook
(add-hook 'after-save-hook 'gdev:after-save-hooks)

;;
;; Register hook is called after parseing
;; Format of the hook function:(lambda (parsed-buffer-name) ...)

(defvar gdev:after-parseing-hooks nil)
(defun gdev:add-after-parseing-hook (hook)
  (setq gdev:after-parseing-hooks
	(cons hook gdev:after-parseing-hooks)))

;;
;; Communicate to gosh-complete

(defun gdev:load-default-module ()
  "[internal]"
  (when gdev:debug
    (message "load-default-module"))
  (unless gdev:default-module-order
    (gdev:add-async-task "#load-default-module\n"
			 'gdev:load-default-module-callback
			 nil)))

(defun gdev:load-default-module-callback (docs context)
  "[internal]"
  (when gdev:debug
    (message "load-default-module-callback")
    (message "%s" docs))
  (when docs
    (setq gdev:default-module-order (assoc-default 'order docs))
    (gdev:add-doc (assoc-default 'docs docs)))
  t)

(defun gdev:parse-cur-buf-from-file ()
  (when gdev:debug
    (message "parse-cur-buf-from-file"))
  (when (and (not (zerop (length buffer-file-name))) (not (buffer-modified-p)))
    (gdev:parse-cur-buf)))

(defun gdev:parse-cur-buf ()
  (when gdev:debug
    (message "parse-cur-buf"))
  (let* ((buf-key (buffer-name))
	 (filename buffer-file-name)
	 (docname (if (zerop (length filename))
		      (concat "#" buf-key "#[No Name]")
		    (concat "#" buf-key "#" filename))))
    (if (or (zerop (length filename)) (buffer-modified-p))
	(let ((basedir (if (zerop (length filename))
			   default-directory
			 (file-name-directory filename))))
	  (gdev:add-async-task (concat "#stdin " basedir " " docname "\n"
				       (buffer-substring-no-properties (point-min) (point-max))
				       "#stdin-eof\n")
			       'gdev:parse-cur-buf-callback
			       buf-key))
      (gdev:add-async-task (concat "#load-file " filename " " docname "\n")
			   'gdev:parse-cur-buf-callback
			   buf-key))))

(defun gdev:parse-cur-buf-callback (docs context)
  "[internal]"
  (when gdev:debug
    (message "parse-cur-buf-callback")
    (message "%s" docs))
  (when docs
    (gdev:add-doc (assoc-default 'docs docs))
    (gdev:set-module-order context
			   (vconcat gdev:default-module-order (assoc-default 'order docs)))
    ;;exec each hook
    (mapc
     (lambda (hook) (funcall hook context))
     gdev:after-parseing-hooks))
  t)


;;
;; document management

(defvar gdev:doc-table (make-hash-table :test 'equal))
(defvar gdev:default-module-order nil)
(defvar gdev:each-buf-data (make-hash-table :test 'equal))

(defun gdev:add-doc (docs)
  "[internal]"
  (mapc
   (lambda (doc)
     (let ((name (assoc-default 'n doc))
	   (filepath (assoc-default 'f doc))
	   (units (assoc-default 'units doc)))
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
	 (puthash name units gdev:doc-table))))
   docs))

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
			 (gdev:get-load-module-text)
			 ))
    ;;ignore warning
    (process-kill-without-query gdev:process nil)
    ;;set process filter
    (set-process-filter gdev:process 'gdev:read-from-gauche-complete))
  ;;count up init-count
  (setq gdev:init-count (1+ gdev:init-count)))

(defun gdev:get-load-module-text ()
  "[internal]"
  (gdev:foldl
   (lambda (acc name)
     (let ((name (gdev:get-loaded-module-name name)))
       (if name
	   (concat acc " --load-module=\"" name "\"")
	 acc)))
   ""
   (gdev:hash-keys gdev:doc-table)))

(defun gdev:get-loaded-module-name (docname)
  "[internal]"
  (if (string-match "^#" docname)
      (let ((name (substring docname (1+ (string-match "#" docname 1)))))
	(if (string= "[No Name]" name)
	    nil
	  (concat "f" name " " docname)))
    (concat "m" docname)))
    
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

(defun gdev:restart-proc ()
  "[internal]"
  (when gdev:process
    (let ((tmp gdev:init-count))
      ;;kill process
      (delete-process gdev:process)
      (setq gdev:process nil)
      ;;restart process
      (setq gdev:init-count 0)
      (gdev:init-proc)
      (setq gdev:init-count tmp))))

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

(defvar gdev:task-timer nil)

(defun gdev:add-async-task (text callback context)
  (when gdev:debug
    (message "add-async-task %s" text))
  (if (zerop (length gdev:task-list))
      (progn ;; Run the task immediately
	(gdev:write-text text)
	(setq gdev:task-list (cons
				  (list callback context (cadr (current-time)))
				  nil))
	;;start timer for checking timeout
	(setq gdev:task-timer (run-at-time t 1 'gdev:check-task-timeout)))
    ;;Stored in the task list
    (setq gdev:task-list (append
			  gdev:task-list
			  (cons (list text callback context) nil)))))

(defun gdev:empty-async-task? ()
  (zerop (length gdev:task-list)))

(defun gdev:start-next-task ()
  "[internal]"
  (when gdev:debug
    (message "start-next-task %s" (gdev:empty-async-task?)))
  (if gdev:task-list
      (progn
	(gdev:write-text (caar gdev:task-list))
	(setcar gdev:task-list (append
				(cdar gdev:task-list)
				(cons (cadr (current-time)) nil))))
    ;;stop timer
    (cancel-timer gdev:task-timer)
    (setq gdev:task-timer nil)))
			      
(defun gdev:exec-task-callback (docs)
  "[internal]"
  (when gdev:debug
    (message "exec-head-task-callback %s, %s" docs gdev:task-list))
  (let ((task (car gdev:task-list)))
    (when (funcall (car task) docs (cadr task))
      (setq gdev:task-list (cdr gdev:task-list))
      (gdev:start-next-task))))

(defvar gdev:output-list nil)
(defun gdev:read-from-gauche-complete (proc text)
  "[internal] process output filter."
  (when gdev:debug
    (message "read-from-gauche-complete %s" text))
  (let ((l (split-string text "\n")))
    (while (cdr l)
      (gdev:exec-task-callback
       (json-read-from-string (mapconcat 'identity (nreverse (cons (car l) gdev:output-list)) "")))
      (setq gdev:output-list nil)
      (setq l (cdr l)))
    (unless (zerop (length (car l)))
      (setq gdev:output-list (cons (car l) gdev:output-list)))))

(defun gdev:check-task-timeout ()
  "[internal] check async task timeout"
  (when gdev:debug
    (message "check-task-timeout %s" (current-time)))
  (let ((task (car gdev:task-list)))
    (when (< gdev:task-timeout-sec  (- (cadr (current-time)) (car (cddr task))))
      ;;timeout
      (gdev:restart-proc)
      (gdev:exec-task-callback nil))))

(provide 'gdev)