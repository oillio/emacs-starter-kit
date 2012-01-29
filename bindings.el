;;; bindings.el --- Set up some handy key bindings

(global-set-key (kbd "C-S-s") 'find-grep-dired)

(global-set-key [(meta up)] 'beginning-of-buffer)
(global-set-key [(meta down)] 'end-of-buffer)

(global-set-key [(meta shift right)] 'ido-switch-buffer)
(global-set-key [(meta shift up)] 'recentf-ido-find-file)
(global-set-key [(meta shift down)] 'ido-find-file)
(global-set-key [(meta shift left)] 'magit-status)

(global-set-key [(meta H)] 'delete-other-windows)

(global-set-key [(meta D)] 'backward-kill-word) ;; (meta d) is opposite

(global-set-key [(meta N)] 'cleanup-buffer)

(global-set-key [(control \])] 'indent-rigidly)


(global-set-key (kbd "C-c [") 'occur)
(global-set-key (kbd "M-g") 'goto-line)

(provide 'bindings)
;;; bindings.el ends here
