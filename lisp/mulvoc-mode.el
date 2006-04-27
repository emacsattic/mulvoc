;;;; mulvoc-mode.el -- mulvoc presented through a minor mode -- its original use
;;; Time-stamp: <2006-04-18 14:35:19 john>

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

(provide 'mulvoc-mode)

(defvar mulvoc-mode t
  "Whether mulvoc mode is active.")

;;;###autoload
(defun mulvoc-mode (arg)
  "Minor mode to show translated words."
  (interactive "p")
  (if (> arg 0)
      (progn
	(setq mulvoc-mode t)
	(mulvoc-ensure-loaded)
	(mulvoc-setup-hook-function)
	(setq mulvoc-cursor-active t))
    (setq mulvoc-mode nil
	  mulvoc-cursor-active nil)))

(defun mulvoc-display (format &rest args)
  "Using FORMAT, display &rest ARGS."
  (if (and (boundp 'tooltip-mode)
	   tooltip-mode
	   mulvoc-use-tooltips)
      (tooltip-show (apply 'format format args))
    (apply 'message args)))

(defun mulvoc-word-display-string (word)
  "Return a display string for WORD.
The languages listed in mulvoc-displayed-languages are shown."
  ;; todo: optional argument showing the language and word in which it came in; and support functions for finding this
  (let ((got (get word 'display-string)))
    (if got
	got
      (if (boundp word)
	  (let ((value (symbol-value word))
		(word-string (symbol-name word))
		(languages (if mulvoc-displayed-languages
			       (mapcar (function
					(lambda (lang)
					  (if (symbolp lang)
					    lang
					    (intern lang))))
				       mulvoc-displayed-languages)
			     (mapcar 'identity ; symbol-name  
				     (mapcar 'car
					     mulvoc-languages))))
		(collected nil)
		(causes nil) ; language-words that might make this be displayed
		)
	    (dolist (from-language value)
	      ;; (message "Scanning for from-language %S" from-language)
	      (dolist (type (cdr from-language))
		;; (message "  Scanning for type %S within %S" type from-language)
		(dolist (language languages)
		  ;; (message "    Scanning %S for language %S and %S for cause %S" type language (cdr type) word-string)
		  (let ((got-language (assoc language type))
			(got-cause (rassoc word-string (cdr type))))
		    (when got-language
		      ;; (message "      Got language %S" got-language)
		      (pushnew got-language collected :test 'equal))
		    (when got-cause
		      ;; (message "      Got cause %S" got-cause)
		      (pushnew (car got-cause) causes :test 'equal))))))
	    ;; (message "causes are %S" causes)
	    (setq collected
		  (delete-if (lambda (item)
			       ;; (message "looking at %S, %s" item (if (member (car item) causes) "will delete" "will not delete"))
			       (member (car item) causes))
			     collected))
	    (let ((string (format "%s:%s = %s"
				  (mapconcat 'symbol-name
					     causes
					     ","
					     )
				  word-string
				  (mapconcat (lambda (pair)
					       (format "%s: %s"
						       (car pair)
						       (mulvoc-word-as-string (cdr pair))))
					     collected
					     ", "))))
	      (put word 'display-string string)
	      string))
	nil))))

(defun mulvoc-command-hook-function ()
  "When mulvoc-cursor-active is non-nil, show translations of the word under point,
whenever point moves to another word."
  (when (and mulvoc-mode mulvoc-cursor-active)
    (let ((new (thing-at-point 'word)))
      (unless (equal new mulvoc-latest-word)
	(setq mulvoc-latest-word new)
	(let ((ob (intern-soft mulvoc-latest-word mulvoc-words)))
	  (when ob
	    (let ((display-string (mulvoc-word-display-string ob)))
	      (when (stringp display-string) (mulvoc-display "%s" display-string)))))))))

(defun mulvoc-preceding-words-match (place words)
  "Check whether the preceding words before PLACE are WORDS.
They are in the reverse order, i.e. the first in the list is the
most immediately preceding one."
  (if words
      (save-excursion
	(goto-char place)
	(let* ((start (progn (forward-word -1) (point)))
	       (end (progn (forward-word 1) (point)))
	       (word (buffer-substring-no-properties start end)))
	  (if (string= word (car words))
	      (mulvoc-preceding-words-match start (cdr words)))))
    place))

(defvar mulvoc-expanded-words nil
  "Words which have been handled by mulvoc-abbrev-expander.")

(defun mulvoc-abbrev-expander ()
  "Show translations for the word just expanded."
  (when mulvoc-mode
    (save-excursion
      (let* ((start (progn (forward-word -1) (point)))
	     (end (progn (forward-word 1) (point)))
	     (word (buffer-substring-no-properties start end)))
	(let ((ob (intern-soft word mulvoc-words)))
	  (if ob
	      (let ((translation (mulvoc-word-display-string ob)))
		(setq mulvoc-expanded-words
		      (cons ob mulvoc-expanded-words))
		(when (stringp translation)
		  (mulvoc-add-overlay start end translation)
		  (when mulvoc-decorated-buffer
		    (mulvoc-decorate-word start end translation))
		  (mulvoc-display "%s" translation)))
	    (let ((mwob (intern-soft word mulvoc-phrase-ending-words)))
	      (when mwob
		(let* ((phrase-value (symbol-value mwob))
		       (match (mulvoc-preceding-words-match start (cdr phrase-value))))
		  (when match
		    (let ((translation (mulvoc-word-display-string (intern (car phrase-value) mulvoc-words))))
		      (when (stringp translation)
			(mulvoc-add-overlay match end translation)
			(when mulvoc-decorated-buffer
			  (mulvoc-decorate-word match end translation))
			(mulvoc-display "%s" translation)))))))))))))

;;;###autoload
(defun mulvoc-abbrev-setup ()
  "Set up mulvoc's abbreviation system"
  (interactive)
  (mulvoc-ensure-loaded)
  (mapcar (lambda (word-obarray)
	    (mapatoms (lambda (word-atom)
			(let ((word-string (symbol-name word-atom)))
			    ;; todo: try to make this respect the existing abbrev function, you can get this using (symbol-function (intern-soft word-string global-abbrev-table))

			  (define-abbrev global-abbrev-table word-string word-string 'mulvoc-abbrev-expander)))
		      word-obarray))
	  (list mulvoc-words mulvoc-phrase-ending-words))
  (setq abbrev-mode t))

;;;; decorate words in region

(defun mulvoc-show-decoration (a b)
  ;; todo: not quite right yet
  (mulvoc-display "%s" (get-text-property a 'help-echo)))

(defvar mulvoc-decorated-buffer nil
  "Whether this buffer is decorated.")

(make-variable-buffer-local 'mulvoc-decorated-buffer)

(defun mulvoc-decorate-word-1 (begin end translation)
  "Decorate the word between BEGIN and END with TRANSLATION."
  (put-text-property begin end 'help-echo translation)
  (put-text-property begin end 'point-entered 'mulvoc-show-decoration))

(defun mulvoc-decorate-word (begin end translation)
  "Decorate the word between BEGIN and END with TRANSLATION."
  (let ((modified (buffer-modified-p)))
    (mulvoc-decorate-word-1 begin end translation)
    ;; (mulvoc-add-overlay begin end translation) ; don't want to do this twice!
    (set-buffer-modified-p modified)))

(defun mulvoc-decorate-words-region (begin end)
  "Mark words between BEGIN and END with properties that display translations.
Translations are displayed when point moves into the word, or the mouse goes over it."
  (interactive "r")
  (let ((modified (buffer-modified-p)))
    (save-excursion
      (goto-char begin)
      (while (re-search-forward "\\b\\w+\\b" end t)
	(let* ((ob (intern-soft (match-string-no-properties 0) mulvoc-words)))
	  (if (and ob mulvoc-use-tooltips)
	      (let ((translation (mulvoc-word-display-string ob)))
		(when (stringp translation)
		  (mulvoc-decorate-word-1 (match-beginning 0) (match-end 0)
					 translation)))))))
    (set-buffer-modified-p modified)))

(defun mulvoc-decorate-words-buffer ()
  "Mark translations in the current buffer."
  (interactive)
  (setq mulvoc-decorated-buffer t)
  ;; todo: set the buffer up so that any words added also get marked
  (mulvoc-decorate-words-region (point-min) (point-max)))

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
      ;; (overlay-put new-overlay 'face (cons 'bold t))
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

;;; end of mulvoc-mode.el
