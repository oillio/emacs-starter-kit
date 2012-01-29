;; Ensure braces are handled correctly in ruby.

(require 'ruby-electric)

(defvar my-ruby-close-brace-goto-close t
  "Non-nill indicates to move point to the next }, otherwise insert }
and delete the following }.")

;; (defun ruby-electric-code-at-point-p()
;;   (and ruby-electric-mode
;;        (let* ((properties (text-properties-at (point))))
;;          (and (null (memq 'font-lock-string-face properties))
;;               (null (memq 'font-lock-comment-face properties))))))

(defun my-ruby-close-c-brace ()
  "replacement for ruby-electric-brace for the close brace"
  (interactive)
  (let ((p (point)))
    (if my-ruby-close-brace-goto-close
        (unless (and (ruby-electric-code-at-point-p)
                    (search-forward "}" nil t))
          (message "Inserting close brace")
          (insert "}"))
      (insert "}")
      (save-excursion (if (search-forward "}" nil t)
                           (delete-char -1))))))

(defun my-ruby-close-r-brace ()
  "replacement for ruby-electric-brace for the close brace"
  (interactive)
  (let ((p (point)))
    (if my-ruby-close-brace-goto-close
        (unless (and (ruby-electric-code-at-point-p)
                    (search-forward ")" nil t))
          (message "Inserting close brace")
          (insert ")"))
      (insert ")")
      (save-excursion (if (search-forward ")" nil t)
                           (delete-char -1))))))


(define-key ruby-mode-map "}" 'my-ruby-close-c-brace)
(define-key ruby-mode-map ")" 'my-ruby-close-r-brace)

(provide 'ruby-braces)
