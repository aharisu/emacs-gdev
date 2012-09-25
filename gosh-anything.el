;(setq debug-on-error t)

(require 'anything)
(require 'gdev)

(defvar gany:ginfo-candidate nil)

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

(provide 'gosh-anything)

