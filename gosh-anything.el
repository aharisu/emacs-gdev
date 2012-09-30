;=============================================================================
; gosh-anything.el
; AUTHOR:  aharisu <foo.yobina@gmail.com>
; Last Modified: 30 Sep 2012.
; License: MIT license
;     Permission is hereby granted, free of charge, to any person obtaining
;     a copy of this software and associated documentation files (the
;     "Software"), to deal in the Software without restriction, including
;     without limitation the rights to use, copy, modify, merge, publish,
;     distribute, sublicense, and/or sell copies of the Software, and to
;     permit persons to whom the Software is furnished to do so, subject to
;     the following conditions:
;
;     The above copyright notice and this permission notice shall be included
;     in all copies or substantial portions of the Software.
;
;     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
;     OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;     IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;     TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;     SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;=============================================================================
;
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

(defun ginfo-anything-all-symbol ()
  (interactive)
  (anything :sources 'anything-ginfo-all-symbol-source
	    :buffer "*ginfo-anything*"
	    ))

;;
;;ginfo-anything-all-module

(defvar anything-ginfo-all-module-source
  '((name . "ginfo all module")
    (candidates . gdev:get-all-module-list)
    (action ("Symbol in" . (lambda (mod)
			  (anything :sources `((name . ,(concat "ginfo all symbol in " (assoc-default 'n mod)))
					       (candidates . ,(gdev:get-all-symbol-in (assoc-default 'n mod)))
					       (type . ginfo))
				    :buffer "*ginfo-anything*"
				    ))))
    "Gauche Info."))

(defun ginfo-anything-all-module ()
  (interactive)
  (anything :sources 'anything-ginfo-all-module-source
	    :buffer "*ginfo-anything*"
	    ))
    
	  
  
(provide 'gosh-anything)

