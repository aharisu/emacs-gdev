;(setq debug-on-error t)

(require 'anything)
(require 'gdev)

(defvar gany:ginfo-candidate nil)

(defmacro gany:open-func-create (direc)
  `(lambda (unit)
     (gdev:show-info (assoc-default 'docname unit)
		     (assoc-default 'n unit)
		     ,direc)))

(define-anything-type-attribute 'ginfo
  `((action ("Open" . ,(gany:open-func-create nil))
	    ("Open Above" . ,(gany:open-func-create 'above))
	    ("Open Below" . ,(gany:open-func-create 'below))
	    ("Open Left" . ,(gany:open-func-create 'left))
	    ("Open Right" . ,(gany:open-func-create 'right))
	    )
    "Gauche Info."))

;;
;;ginfo-anything

(defvar anything-ginfo-source
  '((name . "ginfo")
    (init . (lambda ()
	      (setq gany:ginfo-candidate
		    (mapcar
		     (lambda (u) (cons (assoc-default 'n u) u))
		     (if anything-execute-action-at-once-if-one
			 (gdev:match-unit-in-order-first-match-priority-exact-match
			  (buffer-name anything-current-buffer) anything-pattern nil)
		       (gdev:match-unit-in-order (buffer-name anything-current-buffer) "" nil))))
	      (when (zerop (length gany:ginfo-candidate))
		(message "No Candidate."))))
    (candidates . gany:ginfo-candidate)
    (type . ginfo)))

(defun ginfo-with-word-anything ()
  (interactive)
  (let ((word  (symbol-name (symbol-at-point))))
    (anything :sources 'anything-ginfo-source
	      :buffer "*ginfo-anything*"
	      :input word
	      :execute-action-at-once-if-one (= (length (gdev:match-unit-in-order-first-match-priority-exact-match (buffer-name) word nil)) 1))
	      ))

(defun ginfo-anything ()
  (interactive)
  (anything :sources 'anything-ginfo-source
	    :buffer "*ginfo-anything*"
	    ))

;;
;;ginfo-all-symbol-anything

(defvar anything-ginfo-all-symbol-source
  '((name . "ginfo all symbol")
    (init . gany:ginfo-all-symbol-init)
    (cleanup . gany:ginfo-all-symbol-cleanup)
    (candidates . gany:ginfo-candidate)
    (volatile)
    (type . ginfo)))

(defvar gany:symbol-selected nil)
(defvar gany:finish-load-all-symbol nil)
(defun gany:ginfo-all-symbol-init ()
  (setq gany:ginfo-candidate nil)
  (setq gany:symbol-selected nil)
  (setq gany:finish-load-all-symbol nil)
  ;;exec load-all-symbol task
  (gdev:add-async-task "#load-all-symbol\n"
		       'gany:load-all-symbol-callback
		       0))

(defun gany:load-all-symbol-callback (docs context)
  "[internal]"
   (if (stringp docs)
      (if (and (string= docs "##") (not gany:symbol-selected))
	  ;;resume 
	  (progn
	    (gdev:write-text "#resume-load-all-symbol\n")
	    nil)
	;;finish load-all-symobl task
	(setq gany:finish-load-all-symbol t)
	t)
    (when (not gany:symbol-selected)
      ;;append candidate
      (setq gany:ginfo-candidate
	    (let ((docname (assoc-default 'n (aref docs 0))))
	      (append
	       gany:ginfo-candidate
	       (mapcar
		(lambda (u)
		  (cons (assoc-default 'n u)
			`((n . ,(assoc-default 'n u))
			  (docname . ,docname))))
		(assoc-default 'units (aref docs 0))))))
      ;;update candidate
      (anything-update (anything-get-selection nil t)))
    nil))

(defun gany:ginfo-all-symbol-cleanup ()
  "[internal]"
  (setq gany:symbol-selected t)
  ;;stop load-all-symbol task
  (gdev:write-text "#end-load-all-symbol\n")
  ;;wait for fisnish
  (while (not gany:finish-load-all-symbol)
    (sleep-for 0.05)))

(defun ginfo-all-symbol-anything ()
  (interactive)
  (anything :sources 'anything-ginfo-all-symbol-source
	    :buffer "*ginfo-anything*"
	    ))
  
(provide 'gosh-anything)

