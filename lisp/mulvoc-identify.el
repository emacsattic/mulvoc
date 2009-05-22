;;;; mulvoc-identify.el -- identify language and parts of speech in text
;;; Time-stamp: <2009-05-22 17:28:15 jcgs>

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

;; (provide 'mulvoc-identify)

;; (defun mulvoc-apply-to-words (fn from to &rest fnargs)
;;   "Apply FN to words between FROM and TO in the buffer.
;; FN will be called with the word, the mulvoc data for the word, start
;; and end positions, and any extra arguments passed in to
;; mulvoc-apply-to-words."
;;   (save-excursion
;;     (goto-char from)
;;     (while (re-search-forward "\\<\\sw+\\>" to t)
;;       (let* ((word-string (intern-soft (match-string-no-properties 0) mulvoc-words))
;; 	     (word-symbol word-string)
;; 	     (word-value (if (boundp word-symbol)
;; 			     (symbol-value word-symbol)
;; 			   nil)))
;; 	;; (message "word at %d..%d: %s" (match-beginning 0) (match-end 0) word-string)
;; 	(apply fn word-string word-value
;; 	       (match-beginning 0) (match-end 0) fnargs)))))

;; (defun mulvoc-test-apply ()
;;   (interactive)
;;   (mulvoc-apply-to-words
;;    (lambda (word data from to)
;;      (message "%d..%d: %s: %S" from to word data))
;;    (region-beginning)
;;    (region-end)))

;; (defun mulvoc-count-language-words-region (from to)
;;   "Count the words in each language, between FROM and TO.
;; The result is an list in order of decreasing likeliness, of languages
;; and the numbers of words from them."
;;   (interactive "r")
;;   (let ((languages nil))
;;     (mulvoc-apply-to-words
;;      (function
;;       (lambda (word data from to)
;; 	;; (message "%S %S" word data)
;; 	(mapcar
;; 	 (function
;; 	  (lambda (langdata)
;; 	    ;; (message "  %S" (car langdata))
;; 	    (let ((langpair (assoc (car langdata) languages)))
;; 	      (if langpair
;; 		  (rplacd langpair (1+ (cdr langpair)))
;; 		(setq languages (cons (cons (car langdata) 1)
;; 				      languages))))))
;; 	 data)))
;;      from to)
;;     (let ((result (sort languages
;; 			(function
;; 			 (lambda (a b)
;; 			   (> (cdr a) (cdr b)))))))
;;       (message (mapconcat (lambda (lp) (format "%s: %d" (car lp) (cdr lp))) result "; "))
;;       result)))

;; (defun mulvoc-identify-language-region (from to)
;;   "Attempt to identify the predominant language between FROM and TO."
;;   (interactive "r")
;;   (let ((result (caar (mulvoc-count-language-words-region from to))))
;;     (message "Most words from: %S" result)
;;     result))

;; (defun mulvoc-part-of-speech-echo (window object position)
;;   (mapconcat 'symbol-name (get-text-property position 'parts-of-speech) ", "))

;; (defun mulvoc-parts-of-speech-region (from to)
;;   "Decorate words between FROM and TO with their parts of speech."
;;   (interactive "r")
;;   (let ((main-language (mulvoc-identify-language-region from to)))
;;     (mulvoc-apply-to-words
;;      (function
;;       (lambda (word data from to)
;; 	(let ((chosen-language-data (cdr (or (assoc main-language data)
;; 					(car data)))))
;; 	  (message "%S: using %S" word chosen-language-data)
;; 	  (put-text-property from to 'meanings chosen-language-data)
;; 	  (put-text-property from to 'parts-of-speech (mapcar 'car chosen-language-data))
;; 	  (put-text-property from to 'help-echo 'mulvoc-part-of-speech-echo)
;; 	  )
;; 	))
;;      from to)))

;;; end of mulvoc-identify.el
