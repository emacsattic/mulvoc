;;;; mulvoc.el -- multi-lingual vocabulary
;;; Time-stamp: <2009-05-21 11:33:06 jcgs>

;;  This program is free software; you can redistribute it and/or modify it
;;  under the terms of the GNU General Public License as published by the
;;  Free Software Foundation; either version 2 of the License, or (at your
;;  option) any later version.

;;  This program is distributed in the hope that it will be useful, but
;;  WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;  General Public License for more details.

;;  You should have received a copy of the GNU General Public License along
;;  with this program; if not, write to the Free Software Foundation, Inc.,
;;  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

;;; Written by John C G Sturdy in October 2004
;;; Completely re-written by John C G Sturdy in May 2009
;;; IPR declaration: entirely my original work

(require 'thingatpt)

;;;;;;;;;;;;;;;;;;;
;; Customization ;;
;;;;;;;;;;;;;;;;;;;

(defgroup mulvoc nil
  "MUlti-Lingual VOcabulary."
  :prefix "mulvoc-"
  :group 'mulvoc)

(defcustom mulvoc-setup-hook nil
  "*Functions to run just before setting up mulvoc data structures."
  :type 'hook
  :group 'mulvoc)

(defcustom mulvoc-post-setup-hook nil
  "*Functions to run just after setting up mulvoc data structures."
  :type 'hook
  :group 'mulvoc)

(defcustom mulvoc-vocabulary-program "vocmerge"
  "The name of the program for creating the vocabulary list.
It is given options from `mulvoc-vocabulary-program-options'."
  :group 'mulvoc
  :type 'string)

(defcustom mulvoc-vocabulary-program-options '("-r")
  "Options to give to `mulvoc-vocabulary-program'."
  :group 'mulvoc
  :type '(repeat string))

(defcustom mulvoc-key-language "ENG"
  "The key language."
  :group 'mulvoc
  :type 'string)

(defcustom mulvoc-dictionary-files 
  (list (expand-file-name "~/.vocabulary.csv"))
  "The vocabulary files to read."
  :group 'mulvoc
  :type '(repeat 'file))

(defcustom mulvoc-use-overlays nil
  "Experimental feature.
If nil, mulvoc does not use overlays.
If a number, it uses that many of them, per buffer.
If anything else, it uses any number of them."
  :group 'mulvoc
  :type '(choice boolean integer))

(defcustom mulvoc-use-tooltips t
  "Whether to use tooltips, if they are available."
  :type 'boolean
  :group 'mulvoc)

(defcustom mulvoc-decorate-buffers t
  "Whether to set translation properties on buffers.
Enabling this allows tooltip display on mouseover, for example."
  :group 'mulvoc
  :type 'boolean)

;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal variables ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defvar mulvoc-vocab-table (make-vector 1511 nil)
  "Obarray for translations of keys.")

(defvar mulvoc-phrase-ending-words (make-vector 1511 nil)
  "Obarray for mulvoc phrase lookup.
Each symbol is the last word of a recognized phrase, and it is bound
to a cons of the string of the whole phrase, and the preceding words
of the phrase, in reverse order, i.e. the word just before the ending
word comes first in the list.")

(defvar mulvoc-pre-mulvoc-abbrev-active nil
  "Whether abbrev-mode was active before mulvoc-mode was turned on.")

(make-variable-buffer-local 'mulvoc-pre-mulvoc-abbrev-active)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Displaying translations ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun mulvoc-mode (&optional arg)
  "Minor mode to show translated words.
With no ARG, toggle the mode.
With ARG, switch the mode on if ARG is greater than zero."
  (interactive "P")
  (if (if arg
	  (> (prefix-numeric-value arg) 0)
	(not mulvoc-mode))
      (progn
	(setq mulvoc-mode t
	      mulvoc-pre-mulvoc-abbrev-active abbrev-mode
	      abbrev-mode t)
	(mulvoc-setup)
	(when mulvoc-decorate-buffers
	  (mulvoc-decorate-words-buffer)))
    (setq mulvoc-mode nil
	  abbrev-mode mulvoc-pre-mulvoc-abbrev-active)))

(defcustom mulvoc-mode nil
  "Toggle mulvoc-mode.
Setting this variable directly does not take effect the first
time it is used; use either \\[customize] or the function
`mulvoc-mode'."
  :group 'mulvoc
  :type 'boolean
  :set 'custom-set-minor-mode
  :require 'mulvoc)

(make-variable-buffer-local 'mulvoc-mode)

(defun mulvoc-word-display-string (word)
  "Return the display string for WORD."
  (unless (symbolp word)
    (setq word (intern-soft word mulvoc-vocab-table)))
  (if word
      (symbol-value word)
    nil))

(defun mulvoc-abbrev-expander ()
  "Show translations for the word just expanded."
  (when mulvoc-mode
    (save-excursion
      (let* ((start (progn (forward-word -1) (point)))
	     (end (progn (forward-word 1) (point)))
	     (word (buffer-substring-no-properties start end)))
	(let ((translation (mulvoc-word-display-string word)))
	  (when (stringp translation)
	    (mulvoc-add-overlay start end translation)
	    ;; (mulvoc-scrolling-display translation)
	    (when (and mulvoc-decorate-buffers
		       mulvoc-decorated-buffer)
	      (mulvoc-decorate-word start end translation))
	    (mulvoc-display "%s" translation)))
	;; 	(let ((mwob (intern-soft word mulvoc-phrase-ending-words)))
	;; 	  (when mwob
	;; 	    (let* ((phrase-value (symbol-value mwob))
	;; 		   (match (mulvoc-preceding-words-match
	;; 			   start
	;; 			   (cdr phrase-value))))
	;; 	      (when match
	;; 		(let ((translation (mulvoc-word-display-string
	;; 				    (intern (car phrase-value)
	;; 					    mulvoc-words))))
	;; 		  (when (stringp translation)
	;; 		    (mulvoc-add-overlay match end translation)
	;; 		    (mulvoc-scrolling-display translation)
	;; 		    (when mulvoc-decorated-buffer
	;; 		      (mulvoc-decorate-word match end translation))
	;; 		    (mulvoc-display "%s" translation)))))))
	))))

(defun mulvoc-display (format &rest args)
  "Using FORMAT, display &rest ARGS."
  (if (and (boundp 'tooltip-mode)
	   tooltip-mode
	   mulvoc-use-tooltips)
      (tooltip-show (apply 'format format args))
    (apply 'message args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Marking words using overlays ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar mulvoc-overlays nil
  "Translation overlays.")

(make-variable-buffer-local 'mulvoc-overlays)

(defvar mulvoc-overlay-before-change-text nil
  "Some text we keep around to look for whether there is a real change or just a property change.")

(defun mulvoc-overlay-modification-function (overlay after begin end &optional length)
  "Function to remove overlays when the word changes."
  (if after
      (unless (equal mulvoc-overlay-before-change-text
		     (buffer-substring-no-properties begin end))
	(if nil
	    (mulvoc-remove-overlays)
	  (delete-overlay overlay)))
    (setq mulvoc-overlay-before-change-text (buffer-substring-no-properties begin end))))

(defun mulvoc-add-overlay (begin end translation)
  "Add a translation overlay to BEGIN...END, with TRANSLATION."
  (when mulvoc-use-overlays
    (let ((new-overlay
	   (if (and (integerp mulvoc-use-overlays)
		    (>= (length mulvoc-overlays) mulvoc-use-overlays))
	       (let* ((new-end-pair (nthcdr (- mulvoc-use-overlays 2) mulvoc-overlays))
		      (recyled-element (cadr new-end-pair))
		      (surplus (cddr new-end-pair)))
		 (rplacd new-end-pair nil)
		 (mapcar 'delete-overlay surplus)
		 (move-overlay recyled-element begin end)
		 recyled-element)
	     (make-overlay begin end))))
      ;; (put-text-property 0 (length translation) 'face (cons 'italic t) translation)
      (put-text-property 0 (length translation) 'display '(height .9) translation)
      (overlay-put new-overlay 'evaporate t)
      (overlay-put new-overlay 'after-string (format " {%s}" translation))
      (overlay-put new-overlay 'modification-hooks '(mulvoc-overlay-modification-function))
      (setq mulvoc-overlays (cons new-overlay
				 mulvoc-overlays)))))

(defun mulvoc-remove-overlays ()
  "Remove all translation overlays in the current buffer."
  (interactive)
  (mapcar 'delete-overlay mulvoc-overlays)
  (setq mulvoc-overlays nil))

(defadvice recenter (before remove-vocab first () activate)
  (mulvoc-remove-overlays))

(defadvice fill-paragraph (before remove-vocab first () activate)
  (mulvoc-remove-overlays))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ``Decorating'' text with translation properties ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mulvoc-point-show-decoration (a b)
  ;; todo: not quite right yet
  (message "%s" (get-text-property a 'help-echo)))

(defun mulvoc-show-decoration (a b)
  ;; todo: not quite right yet
  (mulvoc-display "%s" (get-text-property a 'help-echo)))

(defvar mulvoc-decorated-buffer nil
  "Whether this buffer is decorated.")

(make-variable-buffer-local 'mulvoc-decorated-buffer)

(defun mulvoc-decorate-word-1 (begin end translation)
  "Decorate the word between BEGIN and END with TRANSLATION.
This is meant as an internal funtion for `mulvoc-decorate-word'
and `mulvoc-decorate-words-region', which handle modification
state, etc."
  (put-text-property begin end 'help-echo translation)
  (put-text-property begin end 'point-entered 'mulvoc-point-show-decoration))

(defun mulvoc-decorate-word (begin end translation)
  "Decorate the word between BEGIN and END with TRANSLATION."
  (let ((inhibit-read-only t)
	(modified (buffer-modified-p)))
    (mulvoc-decorate-word-1 begin end translation)
    ;; (mulvoc-add-overlay begin end translation) ; don't want to do this twice!
    (set-buffer-modified-p modified)))

(defun mulvoc-decorate-words-region (begin end)
  "Mark words between BEGIN and END with properties that display translations.
Translations are displayed when point moves into the word, or the mouse goes over it."
  (interactive "r")
  (let ((inhibit-read-only t)
	(modified (buffer-modified-p)))
    (save-excursion
      (goto-char begin)
      (while (re-search-forward "\\b\\w+\\b" end t)
	(let ((translation (mulvoc-word-display-string
			    (match-string-no-properties 0))))
	  (when (stringp translation)
	    (mulvoc-decorate-word-1 (match-beginning 0) (match-end 0)
				    translation)))))
    (set-buffer-modified-p modified)))

(defun mulvoc-decorate-words-buffer ()
  "Mark translations in the current buffer."
  (interactive)
  (setq mulvoc-decorated-buffer t)
  ;; todo: set the buffer up so that any words added also get marked
  (mulvoc-decorate-words-region (point-min) (point-max)))

;;;;;;;;;;;;;;;;;;
;; Reading data ;;
;;;;;;;;;;;;;;;;;;

(defun mulvoc-parse-buffer (buffer)
  "Parse the vocabulary data in BUFFER."
  (set-buffer buffer)
  (goto-char (point-min))
  (while (re-search-forward "^\\([^:]+\\): \\(.+\\)$" (point-max) t)
    (let* ((key (match-string 1))
	   (translations (match-string 2)))
      (if (string-match " " key)
	  (let ((words (split-string key)))
	    (message "Got phrase %S" words)
	    )
	(let ((old (mulvoc-word-display-string key)))
	  (when old (setq translations (concat old "; " translations)))
	  ;; (message "Defining %s to %s" key translations)
	  (define-abbrev global-abbrev-table
	    key t
	    (symbol-function 'mulvoc-abbrev-expander)
	    0 t)
	  (let ((symbol (intern key mulvoc-vocab-table)))
	    (set symbol translations)))))))

(defun mulvoc-read-data (key-language files)
  "Read vocabulary data, ordered by KEY-LANGUAGE, from FILES.
KEY-LANGUAGE is a language code in the form used in the header
lines of FILES.
FILES are CSV files according to MuLVoc's standard, and are
processed by the program named in `mulvoc-vocabulary-program' into the
format read by `mulvoc-parse-buffer'."
  (let* ((vocab-buffer (get-buffer-create " *vocabulary*")))
    (set-buffer vocab-buffer)
    (erase-buffer)
    (apply 'call-process
	   mulvoc-vocabulary-program
	   nil				; infile
	   vocab-buffer			; buffer
	   nil				; display
	   (append mulvoc-vocabulary-program-options files))
    (mulvoc-parse-buffer vocab-buffer)))

(defun mulvoc-read-file (file)
  "Read vocabulary data from FILE.
FILE must be in the format output by the `vocmerge' program,
and not a CSV file."
  (interactive "fRead vocabulary file: ")
  (save-excursion
    (mulvoc-parse-buffer (find-file-noselect file))))

(defvar mulvoc-loaded nil
  "Whether MuLVoc has been loaded.")

;;;###autoload
(defun mulvoc-setup (&optional force)
  "Set up the MUlti-Lingual VOcabulary system.
With optional argument FORCE, ignore the cached vocabulary file, and
get the original data."
  (interactive "P")
  (unless mulvoc-loaded
    (run-hooks 'mulvoc-setup-hook)
    (mulvoc-read-data mulvoc-key-language mulvoc-dictionary-files)
    (unless (assq 'mulvoc-mode minor-mode-alist)
      (push (list 'mulvoc-mode " MuLVoc") minor-mode-alist))
    (setq mulvoc-loaded t)
    (run-hooks 'mulvoc-post-setup-hook)))

(provide 'mulvoc)

;;; end of mulvoc.el
