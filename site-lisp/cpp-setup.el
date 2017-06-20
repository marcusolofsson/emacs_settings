

(add-hook 'c-mode-common-hook
          (lambda()
            (local-set-key (kbd "C-c o") 'ff-find-other-file)))

(setq c-basic-offset 4)
(setq c++-basic-offset 4)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cc\\'" . c++-mode))
(provide 'cpp-setup)
