;;;; mulvoc-flashcards.el -- flashcards based on mulvoc
;;; Time-stamp: <2009-05-22 17:27:45 jcgs>

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

;; (provide 'mulvoc-flashcards)

;; (defvar mulvoc-flashcard-meanings nil
;;   "Alist of meanings for which flashcards are being asked.
;; The car of each element is the success level, and the cdr is the
;; meaning itself.")

;; (defun mulvoc-add-flashcard (word)
;;   "Add a flashcard for WORD."
;;   (interactive (list (completing-read "Word: " mulvoc-words nil t)))
;;   (when (stringp word)
;;     (setq word (intern-soft word mulvoc-words)))
;;   (when word
;;     (let ((meaning (symbol-value word)))
;;       (unless (rassoc meaning mulvoc-flashcard-meanings)
;; 	(setq mulvoc-flashcard-meanings
;; 	      (cons (cons 0 meaning) mulvoc-flashcard-meanings))))))

;; (defun mulvoc-flashcard (from-language)
;;   (interactive (list 'ENG))
;;   "Ask a vocabulary question."
;;   (setq mulvoc-flashcard-meanings
;; 	(sort mulvoc-flashcard-meanings (function (lambda (a b) (< (car a) (car b))))))
;;   (let* ((n (length mulvoc-flashcard-meanings))
;; 	 (limit (floor (exp n)))
;; 	 (rand (1+ (random limit)))
;; 	 (i (floor (log rand)))
;; 	 (card (nth i mulvoc-flashcard-meanings))
;; 	 (from-language-name (language-name from-language)))
;;     (let* ((all-from (cdr (assoc from-language card)))
;; 	   (this-from (nth (random (length all-from)) all-from))
;; 	   (this-type (car this-from))
;; 	   (this-from-word (cdr (assoc from-language (cdr this-from))))
;; 	   (possible-targets nil))
;;       (setq this-from (cdr this-from))
;;       (while this-from
;; 	(unless (or (eq (caar this-from) 'origins)
;; 		    (eq (caar this-from) from-language))
;; 	  (setq possible-targets (cons (car this-from) possible-targets)))
;; 	(setq this-from (cdr this-from)))
;;       (let* ((target (nth (random (length possible-targets)) possible-targets))
;; 	     (target-language (language-name (car target)))
;; 	     (target-word (cdr target))
;; 	     (answer (read-from-minibuffer (format "Translate the %s %s \"%s\" into %s: "
;; 						   from-language-name
;; 						   this-type
;; 						   this-from-word
;; 						   target-language))))
;; 	(if (string= answer target-word)
;; 	    (progn
;; 	      (rplaca card (1+ (car card)))
;; 	      (message "Correct"))
;; 	  (rplaca card (1- (car card)))
;; 	  (message "The %s for the %s %s \"%s\" is \"%s\""
;; 		   target-language from-language-name this-type
;; 		   this-from-word target-word))))))

;;; end of mulvoc-flashcards.el
