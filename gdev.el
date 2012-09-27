;(setq debug-on-error t)
(defvar gdev:debug nil)

(require 'json)


(defvar gdev:root-dir "~/.emacs-gdev")
(defvar gdev:enable-gdev t)
(defvar gdev:task-timeout-sec 7) ;7 seconds

(defvar gdev:default-split-window-direc 'above)
(defvar gdev:default-split-window-height 7)
(defvar gdev:default-split-window-width 40)

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
;  (when gdev:debug
;    (message "start-next-task %s" (gdev:empty-async-task?)))
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
;  (when gdev:debug
;    (message "exec-head-task-callback %s, %s" docs gdev:task-list))
  (let ((task (car gdev:task-list)))
    (if (funcall (car task) docs (cadr task))
	(progn
	  (setq gdev:task-list (cdr gdev:task-list))
	  (gdev:start-next-task))
      ;;extend the timeout
      (setcar (cddr task) (cadr (current-time))))))

(defvar gdev:output-list nil)
(defun gdev:read-from-gauche-complete (proc text)
  "[internal] process output filter."
;  (when gdev:debug
;    (message "read-from-gauche-complete %s" text))
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
;  (when gdev:debug
;    (message "check-task-timeout %s" (current-time)))
  (let ((task (car gdev:task-list)))
    (when (< gdev:task-timeout-sec  (- (cadr (current-time)) (car (cddr task))))
      ;;timeout
      (gdev:restart-proc)
      (gdev:exec-task-callback nil))))


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
;  (when gdev:debug
;    (message "load-default-module-callback")
;    (message "%s" docs))
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
;  (when gdev:debug
;    (message "parse-cur-buf-callback")
;    (message "%s" docs))
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
			  (list (cons 'docname (replace-regexp-in-string " " "\\ " name))
				(cons 'filepath (replace-regexp-in-string " " "\\ " filepath))
				(cons 'loaded? nil))
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
  (let* ((units (gdev:match-unit-in-order-first-match buf-key keyword allow-duplicate))
	 (ret units)
	 (len (length keyword)))
    (while units
      (if (= (length (assoc-default 'n (car units))) len)
	  (progn
	    (setq ret (cons (car units) nil))
	    (setq units nil))
	(setq units (cdr units))))
    ret))

(defun gdev:match-unit-in-order-first-match (buf-key keyword allow-duplicate)
  (when gdev:debug
    (message "match-unit-in-order-first-match buf-key %s, keyword %s" buf-key keyword))
  (if allow-duplicate
      (gdev:match-unit-in-order-allow-duplicate buf-key keyword 'gdev:first-match-filter)
    (gdev:match-unit-in-order-no-duplicate buf-key keyword 'gdev:first-match-filter)))

(defun gdev:match-unit-in-order (buf-key keyword allow-duplicate)
  (when gdev:debug
    (message "match-unit-in-order buf-key %s, keyword %s" buf-key keyword))
  (if allow-duplicate
      (gdev:match-unit-in-order-allow-duplicate buf-key keyword 'gdev:unit-name-head-filter)
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
;; Show ginfo

(defvar gdev:prev-show-ginof-symbol nil)
(defun ginfo ()
  (interactive)
  (let* ((units (gdev:match-unit-in-order (buffer-name) "" nil))
	 (units (mapcar (lambda (u) (cons (assoc-default 'n u) u)) units)))
    (if (zerop (length units))
	(message "No Candidate.")
      (let* ((prompt (if gdev:prev-show-ginof-symbol
			 (concat "Symbol(" gdev:prev-show-ginof-symbol "): ")
		       "Symbol: "))
	     (sym (completing-read prompt units nil t)))
	(when (zerop (length sym))
	  (setq sym gdev:prev-show-ginof-symbol))
	(when (and sym (not (zerop (length sym))))
	  (let ((u (assoc-default sym units)))
	    (gdev:show-info (assoc-default 'docname u) sym))
	  (setq gdev:prev-show-ginof-symbol sym))))))

(defvar gdev:prev-ginfo-all-module-symbol nil)
(defun ginfo-all-module ()
  (interactive)
  (let ((modules (gdev:get-all-module-list)))
    (if (zerop (length modules))
	(message "No Candidate.")
      (let* ((prompt (if gdev:prev-ginfo-all-module-symbol
			 (concat "Module(" gdev:prev-ginfo-all-module-symbol "): ")
		       "Module: "))
	     (mod (completing-read prompt modules nil t)))
	(when (zerop (length mod))
	  (setq mod gdev:prev-ginfo-all-module-symbol))
	(when (and mod (not (zerop (length mod))))
	  (let* ((units (gdev:get-all-symbol-in mod))
		 (sym (completing-read (concat "Symbol In [" mod "]: ") units nil t)))
	    (when (and sym (not (zerop (length sym))))
	      (let ((u (assoc-default sym units)))
		(gdev:show-info (assoc-default 'docname u) sym)))))))))
		       	    
(defvar gdev:all-module-list nil)
(defun gdev:get-all-module-list (&optional update)
  (unless (and gdev:all-module-list (not update))
    (setq gdev:all-module-list nil)
    (gdev:add-async-task "#load-all-module\n"
			 'gdev:load-all-module-callback
			 nil)
    (while (not gdev:all-module-list)
      (sleep-for 0.05)))
  gdev:all-module-list)

(defun gdev:load-all-module-callback (modules context)
  "[internal]"
  (setq gdev:all-module-list
	(mapcar
	 (lambda (mod) (cons (assoc-default 'n mod) mod))
	 modules))
  t)

(defvar gdev:all-symbol-in-list nil)
(defvar gdev:all-symbol-in-finish nil)
(defun gdev:get-all-symbol-in (module)
  (let ((symbol-list (assoc-default module gdev:all-symbol-in-list)))
    (unless symbol-list
      (setq gdev:all-symbol-in-finish nil)
      (gdev:add-async-task (concat "#load-symbol-in " module "\n")
			   'gdev:load-symbol-in-callback
			   module)
      (while (not gdev:all-symbol-in-finish)
	(sleep-for 0.05))
      (setq symbol-list (assoc-default module gdev:all-symbol-in-list)))
    symbol-list))

(defun gdev:load-symbol-in-callback (doc module)
  "[internal]"
  (when doc
    (let ((docname (assoc-default 'n (aref doc 0))))
      (setq gdev:all-symbol-in-list
	    (append
	     (cons (cons module
			 (mapcar
			  (lambda (unit) (cons (assoc-default 'n unit)
					       `((n . ,(assoc-default 'n unit))
						 (docname . ,docname))))
			  (assoc-default 'units (aref doc 0))))
		   nil)
	     (gdev:filter
	      (lambda (mod) (not (equal (car mod) module)))
	      gdev:all-symbol-in-list)))))
  (setq gdev:all-symbol-in-finish t)
  t)

;;
;;show ginfo buffer

(defun gdev:show-info (module symbol &optional direc size)
  (when gdev:debug
    (message "gdev:show-info"))
  (let* ((doc (gethash module gdev:doc-table))
	 (ginfo-list (if doc
			 (gdev:get-unit-ginfo (gdev:match-unit-in-doc doc symbol) t)
		       (gdev:get-unit-ginfo  `(((n . ,symbol) (docname . ,module) (loaded? . nil))) nil))))
    (unless (zerop (length ginfo-list))
      (unless (or (eq direc 'above) (eq direc 'below))
	(setq direc gdev:default-split-window-direc))
      (unless size
	(if (or (eq direc 'left) (eq direc 'right))
	    (setq size gdev:default-split-window-width)
	  (setq size gdev:default-split-window-height)))
      (gdev:open-preview (gdev:ginfo-list-to-text ginfo-list)
			 "*ginfo*"
			 nil ;no enter
			 direc size))))

(defun gdev:match-unit-in-doc (doc symbol)
  "[internal]"
  (when gdev:debug
    (message "gdev:match-unit-in-doc %s,%s" doc symbol))
  (gdev:filter
   (lambda (unit) (string= symbol (assoc-default 'n unit)))
    doc))

(defvar gdev:get-unit-ginfo-complete t)
(defvar gdev:unit-ginfo nil)
(defun gdev:get-unit-ginfo (units in-table?)
  "[internal]"
  (when gdev:debug
    (message "gdev:get-unit-ginfo %s, %s" units in-table?))
  (let ((loaded (if units
		    (not (gdev:any
			  (lambda (unit) (not (assoc-default 'loaded? unit)))
			  units))
		  t)))
    (if loaded
	units
      (let ((unitname (assoc-default 'n (car units)))
	    (docname (assoc-default 'docname (car units)))
	    (filepath (assoc-default 'filepath (car units))))
	(setq gdev:get-unit-ginfo-complete nil)
	;;call gosh-complete
	(gdev:add-async-task (concat "#get-unit " docname " " unitname "\n")
			     'gdev:get-unit-ginfo-callback
			     nil)
	;;wait until get unit
	(while (not gdev:get-unit-ginfo-complete)
	  (sleep-for 0.05))
	(mapc
	 (lambda (unit)
	   (setcdr unit (append
			 (list (cons 'docname docname)
			       (cons 'filepath filepath)
			       (cons 'loaded? t))
			 (cdr unit))))
	 gdev:unit-ginfo)
	;;update gdev:doc-table
	(when in-table?
	  (puthash docname
		   (append
		    (gdev:filter
		     (lambda (unit) (not (string= (assoc-default 'n unit) unitname)))
		     (gethash docname gdev:doc-table))
		    gdev:unit-ginfo)
		   gdev:doc-table))
	(prog1 gdev:unit-ginfo
	  (setq gdev:unit-ginfo nil))))))

(defun gdev:get-unit-ginfo-callback (docs context)
  "[internal]"
  (when gdev:debug
    (message "gdev:get-unit-ginfo-callback %s" docs))
  (when docs
    (setq gdev:unit-ginfo (append docs nil)))
  (setq gdev:get-unit-ginfo-complete t)
  t)

(defun gdev:adjustment-newline ()
  "[internal]"
  (goto-char (point-max))
  (unless (re-search-backward "\n\n\\'" (- (point-max) 2) t)
    (goto-char (point-max))
    (let ((result (re-search-backward "\n\\'" (1- (point-max)) t)))
      (goto-char (point-max))
      (if result (princ "\n") (princ "\n\n"))))
  (goto-char (point-max)))

(defun gdev:ginfo-list-to-text (units)
  "[internal]"
  (when gdev:debug
    (message "gdev:ginfo-list-to-text"))
  (with-temp-buffer
    (let ((standard-output (current-buffer))
	  (first? t))
      (mapc
       (lambda (unit)
	 (if first?
	     (progn
	       (gdev:princ-unit-type-kind unit)
	       (princ "  ---")
	       (gdev:princ-unit-module unit)
	       (princ "\n"))
	   (gdev:adjustment-newline))
	 (setq first? nil)
	 (gdev:princ-unit-interface unit)
	 (princ "\n")
	 ;;princ unit return
	 (let ((ret (assoc-default 'r unit)))
	   (when (and ret (not (zerop (length ret))))
	     (princ ":: ")
	     (princ ret)
	     (princ "\n")))
	 (princ "\n")
	 (gdev:princ-unit-description unit))
       units)
      (buffer-string))))

(defun gdev:princ-unit-type-kind (unit)
  "[internal]"
  (princ (let ((type (assoc-default 't unit)))
	   (cond
	    ((string= "F" type) "Function")
	    ((string= "Method" type) "Method")
	    ((string= "var" type) "Variable")
	    ((string= "C" type) "Constant Variable")
	    ((string= "Parameter" type) "Parameter")
	    ((string= "Class" type) "Class")
	    ((string= "Macro" type) "Macro")
	    (t "")))))

(defun gdev:princ-unit-module (unit)
  "[internal]"
  (princ (let ((module (assoc-default 'docname unit)))
	   (if (string-match "^#" module)
	       (substring module (1+ (string-match "#" module 1)))
	     ;;TODO
	     module))))

(defun gdev:princ-unit-interface (unit)
  "[internal]"
  (let ((type (assoc-default 't unit)))
    (if (or (string= "F" type) (string= "Method" type) (string= "Macro" type))
	(progn
	  (princ "(")
	  (princ (assoc-default 'n unit))
	  (mapc
	   (lambda (p)
	     (princ " ")
	     (princ (assoc-default 'n p)))
	   (assoc-default 'p unit))
	  (princ ")"))
      (princ (assoc-default 'n unit)))))

(defun gdev:trim-both (text)
  "[internal]"
  (while (string-match "\\`\n" text)
    (setq text (substring text 1)))
  (while (string-match "\n\\'" text)
    (setq text (substring text 0 -1)))
  text)

(defun gdev:princ-unit-description (unit)
  "[internal]"
  (let ((type (assoc-default 't unit)))
    (when (or (string= "F" type) (string= "Method" type) (string= "Macro" type) (string= "Class" type))
      ;;check slot type
      (let ((name)
	    (show-empty?))
	(if (string= "Class" type)
	    (progn
	      (setq name 's)
	      (setq show-empty? t))
	  (setq name 'p)
	  (setq show-empty? nil))
	;;princ each parameter description
	(let ((first? t)
	      (index 0))
	  (mapc
	   (lambda (param)
	     (unless first?
	       (princ "\n"))
	     (when (gdev:princ-param-description param show-empty? index first?)
	       (setq first? nil))
	     (setq index (1+ index)))
	   (assoc-default name unit)))
	;;princ unit description
	(let ((description (assoc-default 'd unit)))
	  (when description
	    (gdev:adjustment-newline)
	    (princ "--description--\n")
	    (princ (gdev:trim-both description))))))))

(defun gdev:princ-param-description (param show-empty? index first?)
  "[internal]"
  (let ((has-description? nil)
	(acceptable (assoc-default 'a param))
	(description (assoc-default 'd param)))
    (let ((text (with-output-to-string
		  (princ (gdev:trim-both (assoc-default 'n param)))
		  (when acceptable
		    (setq has-description? t)
		    (princ "\n     Acceptable:")
		    (mapc
		     (lambda (a)
		       (princ " ")
		       (princ a))
		     acceptable))
		  (when description
		    (setq has-description? t)
		    (princ "\n   ")
		    (princ (gdev:trim-both description))))))
      (if (or show-empty? has-description?)
	  (progn
	    (when first?
	      (princ "--interface--\n"))
	    (princ " ")
	    (princ index)
	    (princ ":")
	    (princ text)
	    t)
	nil))))

;;
;; Open info buffer
      
(defun gdev:open-preview (text bufname enter? direc size)
  (when gdev:debug
    (message "gdev:open-preview %s, %s, %s" enter? direc size))
  (with-current-buffer (get-buffer-create bufname)
    (erase-buffer)
    (insert text)
    (goto-char (point-min)))
  (let ((win (get-buffer-window bufname)))
    (unless win
      (cond
       ((eq 'below direc)
	(setq win (split-window nil (- (window-height) size))))
       ((eq 'right direc)
	(setq win (split-window nil (- (window-width) size) t)))
       ((eq 'left direc)
	(setq win (selected-window))
	(select-window (split-window nil size t)))
       (t ;(eq 'above direc)
	(setq win (selected-window))
	(select-window (split-window nil window-min-height)))))
    (set-window-buffer win bufname)
    (when enter?
      (select-window win))))

;;
;; Jump to definition

(defun gdev:gosh-goto-define (keyword split-direc)
  (let ((units (gdev:match-unit-in-order-first-match-priority-exact-match (buffer-name) keyword nil))
	(success? nil))
    (when (= 1 (length units))
      (let ((line (string-to-int (assoc-default 'l (car units))))
	    (filepath (assoc-default 'filepath (car units))))
	(when (and (< 0 line) filepath (not (zerop (length filepath))))
	  (cond
	   ((eq split-direc 'v) (select-window (split-window-horizontally)))
	   ((eq split-direc 'h) (select-window (split-window-vertically))))
	  (find-file filepath)
	  (goto-line line)
	  (setq success? t))))
    (unless success?
      (message "Sorry. Can't jump."))))

(provide 'gdev)