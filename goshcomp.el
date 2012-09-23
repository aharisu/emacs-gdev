;(setq debug-on-error t)
(defvar goshcomp:debug nil)

(require 'gdev)
(require 'auto-complete)




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
;; Register hook 
(gdev:add-after-parseing-hook 'goshcomp:build-word-list)

(provide 'goshcomp)