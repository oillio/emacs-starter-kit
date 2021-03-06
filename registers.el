;;; registers.el --- Set up registers

;; Registers allow you to jump to a file or other location
;; quickly. Use C-x r j followed by the letter of the register (i for
;; init.el, r for this file) to jump to it.

(dolist (r `((?d (file . ,(concat dotfiles-dir "dan.el")))
             (?b (file . ,(concat dotfiles-dir "bindings.el")))
             (?r (file . ,(concat dotfiles-dir "registers.el")))))
  (set-register (car r) (cadr r)))

(provide 'registers)
;;; starter-kit-registers.el ends here
