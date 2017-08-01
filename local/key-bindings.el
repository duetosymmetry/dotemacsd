;; This comes from https://github.com/magnars/.emacs.d/blob/master/key-bindings.el
;; I don't need to kill emacs that easily
;; the mnemonic is C-x REALLY QUIT
(global-set-key (kbd "C-x r q") 'save-buffers-kill-emacs)
(global-set-key (kbd "C-x C-c") 'delete-frame)

;; (require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; Enter magit-status
(global-set-key (kbd "C-x g") 'magit-status)

;; Start org-capturing
(global-set-key (kbd "C-c c") 'org-capture)

;; Zooming in a mac fashion
(global-set-key (kbd "s-=") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)
(global-set-key (kbd "s-0") (lambda () (interactive) (text-scale-set 0)))
