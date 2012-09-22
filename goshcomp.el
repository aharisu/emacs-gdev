;(setq debug-on-error t)
(defvar goshcomp:debug nil)

(require 'gdev)
(require 'auto-complete)

(defvar goshcomp:enable-goshcomp t)

(defvar goshcomp:default-module-order nil)

;;
;; hooks

(defun goshcomp:scheme-mode-hooks ()
  "[internal]"
  (when goshcomp:debug
    (message "goshcomp:scheme-mode-hooks")
    (message (buffer-name))
    (message buffer-file-name)
    (message mode-name))
  (when goshcomp:enable-goshcomp
    (gdev:init-proc)
    (goshcomp:load-default-module)
    (goshcomp:parse-cur-buf)))
;;register scheme-mode hook
(add-hook 'scheme-mode-hook 'goshcomp:scheme-mode-hooks)

(defun goshcomp:after-save-hooks ()
  "[internal]"
  (when goshcomp:debug
    (message "goshcomp:after-save-hooks")
    (message mode-name))
  (when (and goshcomp:enable-goshcomp
	     (string= "Scheme" mode-name))
    (goshcomp:parse-cur-buf-from-file)))
;;register after-save-hook
(add-hook 'after-save-hook 'goshcomp:after-save-hooks)


;;
;; auto-complete sources

(defvar goshcomp:last-init-time 0)
(defvar goshcomp:last-candidate nil)
(defvar goshcomp:last-keyword nil)

(defmacro goshcomp:create-init-func (sym-candidate filter-type-list)
  "[internal]"
  `(lambda ()
     (when goshcomp:debug
       (message "ac-initb")
       (message "%s" ,filter-type-list))
     (setq ,sym-candidate
	   (gdev:filter-map
	    (lambda (word)
	      (let ((type (assoc-default 'type word)))
		(and  (gdev:any (lambda (ty) (string= ty type)) ,filter-type-list)
		      (assoc-default 'word word))))
	    (if (and (string= ac-prefix goshcomp:last-keyword) (< (- (cadr (current-time)) goshcomp:last-init-time) 1))
		goshcomp:last-candidate
	      (let* ((keyword (concat "^" ac-prefix))
		     (cand (gdev:filter
			    (lambda (word) (string-match keyword (assoc-default 'word word)))
			    (gdev:get-buf-data (buffer-name) 'words))))
		(setq goshcomp:last-candidate cand)
		(setq goshcomp:last-keyword ac-prefix)
		(setq goshcomp:last-init-time (cadr (current-time)))
		cand))))))

(defvar goshcomp:func-candidate nil)
(ac-define-source goshcomp:func
  `((candidates . goshcomp:func-candidate)
    (init .  ,(goshcomp:create-init-func goshcomp:func-candidate '("f")))
    (cache)
    (symbol . "f")
    ))

(defvar goshcomp:variable-candidate nil)
(ac-define-source goshcomp:variable
  `((candidates . goshcomp:variable-candidate)
    (init . ,(goshcomp:create-init-func goshcomp:variable-candidate '("v")))
    (cache)
    (symbol . "v")
    ))

(defvar goshcomp:macro-candidate nil)
(ac-define-source goshcomp:macro
  `((candidates . goshcomp:macro-candidate)
    (init . ,(goshcomp:create-init-func goshcomp:macro-candidate '("m")))
    (cache)
    (symbol . "m")
    ))

(defvar goshcomp:class-candidate nil)
(ac-define-source goshcomp:class
  `((candidates . goshcomp:class-candidate)
    (init . ,(goshcomp:create-init-func goshcomp:class-candidate '("c")))
    (cache)
    (symbol . "c")
    ))

(defvar goshcomp:all-source
  (list
   ac-source-goshcomp:func
   ac-source-goshcomp:variable
   ac-source-goshcomp:macro
   ac-source-goshcomp:class
   ))

;;
;; Build candidate for auto-complete

(defun goshcomp:build-word-list (buf-key)
  "[internal]"
  (gdev:set-buf-data
   buf-key 'words
   (gdev:hash-values
    (gdev:foldl
     (lambda (acc unit)
       (let ((name (assoc-default 'n unit)))
	 (puthash name `((word . ,name)
			 (summary . ,(goshcomp:get-unit-summary unit))
			 (type . ,(goshcomp:get-unit-type (assoc-default 't unit)))
			 )
		  acc)
	 acc))
     (make-hash-table)
     ;;get all unit (without duplicate)
     (gdev:match-unit-in-order-first-match buf-key "" nil)))))

(defun goshcomp:get-unit-type (type)
  "[internal]"
  (cond
   ((or (string= type "F") (string= type "Method"))
    "f")
   ((or (string= type "var") (string= type "C") (string= type "Parameter"))
    "v")
   ((string= type "Class")
    "c")
   ((string= type "Macro")
    "m")
   (t "")))

(defun goshcomp:get-unit-summary (unit)
  "[internal]"
  ;;TODO
  (concat "[gosh] "
	  (assoc-default 'docname unit))
  )

;;
;; Communicate to gosh-complete

(defun goshcomp:add-doc (docs)
  "[internal]"
  (mapc
   (lambda (doc)
     (gdev:add-doc (assoc-default 'n doc)
		   (assoc-default 'f doc)
		   (assoc-default 'units doc)))
   docs))

(defun goshcomp:load-default-module ()
  "[internal]"
  (when goshcomp:debug
    (message "load-default-module"))
  (unless goshcomp:default-module-order
    (gdev:add-async-task "#load-default-module\n"
			 'goshcomp:load-default-module-callback
			 nil)))

(defun goshcomp:load-default-module-callback (docs context)
  "[internal]"
  (when goshcomp:debug
    (message "load-default-module-callback")
    (message "%s" docs))
  (when docs
    (setq goshcomp:default-module-order (assoc-default 'order docs))
    (goshcomp:add-doc (assoc-default 'docs docs)))
  t)

(defun goshcomp:parse-cur-buf-from-file ()
  "[internal]"
  (when goshcomp:debug
    (message "parse-cur-buf-from-file"))
  (when (and (not (zerop (length buffer-file-name))) (not (buffer-modified-p)))
    (goshcomp:parse-cur-buf)))

(defun goshcomp:parse-cur-buf ()
  "[internal]"
  (when goshcomp:debug
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
			       'goshcomp:parse-cur-buf-callback
			       buf-key))
      (gdev:add-async-task (concat "#load-file " filename " " docname "\n")
			   'goshcomp:parse-cur-buf-callback
			   buf-key))))

(defun goshcomp:parse-cur-buf-callback (docs context)
  "[internal]"
  (when goshcomp:debug
    (message "parse-cur-buf-callback")
    (message "%s" docs))
  (when docs
    (goshcomp:add-doc (assoc-default 'docs docs))
    (gdev:set-module-order context
			   (vconcat goshcomp:default-module-order (assoc-default 'order docs)))
    (goshcomp:build-word-list (buffer-name)))
  t)

(provide 'goshcomp)