(require 'cl)
(require 'nrepl)

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
  (replace-regexp-in-string "#<[[:alpha:]]+\\([[:ascii:]]+?\\)>" "\"\\1\"" value))

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
