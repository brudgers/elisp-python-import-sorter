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

<sort-import-statements-by-name>>

;;; Copy Python Buffer

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

;; use fundamental mode to avoid capturing fontlock information
;; when calling buffer-string
(fundamental-mode)
(save-excursion
  (first-matching-line import-match)
  (let ((place-to-insert (point-marker))
        (whole-buffer (buffer-string)))
    ;; delete the matching lines from original buffer
    (buffer-flush-matching-lines import-match)
    (insert
     (with-temp-buffer
       (fundamental-mode)
       (insert whole-buffer)
       (buffer-flush-not-matching-lines import-match)
       (let
           ((statements (parse-import-statements (mark-whole-buffer))))
         
         
       ))) ; end insert
    ))
;; restore default mode for file extension
(normal-mode)
