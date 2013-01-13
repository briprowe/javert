(require 'cl)
(require 'nrepl)

(defvar nrepl-inspect-translations
  '(;; Java types such as #<Method ....>
    (lambda (value) 
      (replace-regexp-in-string "#<[[:alpha:]]+\\([[:ascii:]]+?\\)>" "\"\\1\"" value))
    'quote-clojure-sets)                      ;; Clojure sets
  "Translate the output from Clojure into values that can be read by elisp.")

(defun quote-clojure-sets (value &optional start)
  "Wrap clojure sets in quotes."
  (let ((start (if (null start) 0 start))
        (pos (string-match "#{" value start)))
    (if (null pos)
        value
      (let ((brace-end (find-braces value "{" "}" pos)))
        (quote-clojure-sets (replace-in-string value
                                               (substring value pos brace-end)
                                               (format "\"%s\"" (substring value pos brace-end)))
                            (+ brace-end 2))))))

;; FIXME: There is probably a much better way to do this... perhaps
;; using the clojure syntax table and paredit?
(defun find-braces (string start-brace end-brace start-pos &optional depth)
  (if (>= start-pos (length string))
      nil
    (let* ((pos (1+ start-pos))
           (depth (if (null depth) 0 depth))
           (cur-char (substring string start-pos pos)))
      (cond
       ((and (equal cur-char end-brace) (= depth 1))
        pos)

       ((and (equal cur-char end-brace) (> depth 0))
        (find-braces string start-brace end-brace pos (1- depth)))

       ((and (equal cur-char start-brace))
        (find-braces string start-brace end-brace pos (1+ depth)))

       (t (find-braces string start-brace end-brace pos depth))))))

(defun nrepl-inspect-clj-string (ns symbol)
  (let ((header (format "\"Inspecting symbol: %s\"" symbol)))
    (format "%s" `(do (require 'inspector.javert)
                      (in-ns ,(format "'%s" ns))
                      (inspector.javert/inspect ,(format "%s" symbol))))))

(defun nrepl-inspect-print-list (value)
  (cond ((eq (car value) :value) (nrepl-inspect-print-value (cadr value)))
        ((eq (car value) :newline) (insert "\n"))))

(defun nrepl-inspect-print-value (value)
  (cond ((stringp value) (insert value))
        ((listp value) (nrepl-inspect-print-list value))
        (t (insert (format "%s" value)))))

(defun nrepl-inspect-print-result (buffer result)
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (mapcar 'nrepl-inspect-print-value result))))

(defun nrepl-inspect-clean-value (value)
  "Make java types readable (both for humans and the elisp reader)."
  (reduce (lambda (value translation)
            (funcall translation value))
          nrepl-inspect-translations
          :initial-value value))

(defun debug (output)
  (with-current-buffer (get-buffer-create "nrepl-inspect-debug")
    (insert output)))

(defun nrepl-inspect ()
  (interactive)
  (let* ((code (nrepl-inspect-clj-string nrepl-buffer-ns (symbol-at-point)))
         (inspect-buffer (nrepl-popup-buffer "*nREPL inspect*" t)))
    (nrepl-send-string code (nrepl-make-response-handler
                             inspect-buffer
                             (lambda (buffer value)
                               (nrepl-inspect-print-result buffer
                                                           (read (nrepl-inspect-clean-value
                                                                  (format "%s" value)))))
                             '()
                             (lambda (buffer str)
                               (nrepl-emit-into-popup-buffer buffer "Error"))
                             '()))))
