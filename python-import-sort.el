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
