;;; Helpers

;;; Search Helper

;;; The match string for flushing and keeping
(setq import-match "import")

(defun buffer-flush-matching-lines (regex)
  "Flushes matching lines from buffer."
  (mark-whole-buffer)
  (flush-lines regex))

(defun buffer-flush-not-matching-lines (regex)
  "Removes non-matching lines from buffer."
    (mark-whole-buffer)
    (keep-lines regex))

(defun first-matching-line (regex)
  "Finds the start of line for the first line matching regex."
  (beginning-of-buffer)
  (search-forward regex)
  (move-beginning-of-line nil))

;;; Parse Helpers

(defun buffer-to-list ()
  "Converts a buffer to a list of lines."
  (split-string (buffer-string) "\n" t))

(defun parse-import-statement (line)
  "Takes a Python import statement as a string.
Returns a dotted list of: (import-statement . sort-term)."
  (let* ((matcher "\\(\\w+ import \\|import \\)\\(\\w\\)")
         (match (string-match matcher line)))
    (cons line (match-string 2 line))))

(defun parse-import-statements (statements &optional a-list)
  "Parses each statement in statements. Returns a list of (statement . sort-term)."
  (if (null statements)
      a-list
    (parse-import-statements
     (rest statements)
     (cons (parse-import-statement (first statements))
           a-list))))


;;; sort helpers

;;; sorting helper
(defun python-module-less-than (lhs rhs)
  "Returns true if the module name of the left hand side is less than the module name of the right hand side."
  (string< (second lhs) (second rhs)))

;;; sorting helper
(defun sort-python-modules (structures)
  "Sorts data structures using data-structure-less-than"
  (sort structures python-module-less-than))


;; replace helpers

(defun insert-python-import-statement (ds)
  "Inserts the string (car) of a data structure into current buffer"
  (insert (car ds))
  (newline))

(defun insert-python-import-statements (structures)
  "Inserts the ordered statements into current buffer."
  (mapc insert-python-import-statement structures))


;;; Main

(defun python-import-sort ()
  (interactive)
  (fundamental-mode)
  (save-excursion
    (first-matching-line import-match)
    (let ((place-to-insert (point-marker))
          (whole-buffer (buffer-string)))
      (with-temp-buffer
        (fundamental-mode)
        (insert whole-buffer)
        (buffer-flush-not-matching-lines import-match)
        (let* ((modules (parse-import-statements (buffer-to-list)))
               (sorted-modules (sort-python-modules modules)))
          (flush-lines "")
          (insert-python-import-statements sorted-modules))
        (setq whole-buffer (buffer-string))); end with-temp-buffer
      (buffer-flush-matching-lines import-match)
      (goto-char place-to-insert)
      (insert whole-buffer))))
