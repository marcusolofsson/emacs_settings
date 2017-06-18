;; init.el --- Emacs configuration 

;; INSTALL PACKAGES
;; --------------------------------------

(let ((default-directory "~/.emacs.d/site-lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

(add-to-list 'load-path (expand-file-name "~/.emacs.d/custom"))
(setq bdf-directory-list '(expand-file-name "~/.emacs.d/fonts"))

(require 'package) ;; You might already have this line

(setq package-archives
  '(("gnu" . "http://elpa.gnu.org/packages/")
    ("marmalade" . "https://marmalade-repo.org/packages/")
    ("melpa" . "http://melpa.milkbox.net/packages/")))

(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

(when (not package-archive-contents)
  (package-refresh-contents))

(require 'uniquify)


(defvar myPackages
  '(airline-themes
    better-defaults
    company
    company-flx
    company-irony                       
    company-irony-c-headers
    company-rtags
    ;;ac-clang
    cmake-ide
    cmake-mode
    dired+
    dired-efap
    dtrt-indent
    duplicate-thing
    elpy
    exec-path-from-shell
    f
    flycheck
    flycheck-irony
    flycheck-pyflakes
    flycheck-rtags
    flymake-puppet
    irony
    helm
    helm-c-yasnippet
    helm-company
    helm-ctest
    helm-flycheck
    helm-flx
    helm-projectile
    helm-swoop
    helm-rtags
    magit
    magit-filenotify
    material-theme
    move-text
    multiple-cursors
    org
    org-projectile
    powerline
    projectile
    puppet-mode
    rtags
    s
    smart-mode-line
    spacemacs-theme
    transpose-frame
    undo-tree
    yasnippet
    yaml-mode))



(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      myPackages)

;; BASIC CUSTOMIZATION
;; --------------------------------------
(require 'airline-themes)
(require 'cmake-mode)
(require 'company)
(require 'cpp-setup)
(require 'dired-efap)
(require 'dtrt-indent)
(require 'duplicate-thing)
(require 'eww)
(require 'exec-path-from-shell)
(require 'move-text)
(require 'setup-helm)
(require 'powerline)
(require 'transpose-frame)
(require 'undo-tree)
(require 'yaml-mode)
(require 'yasnippet)

(global-set-key (kbd "<M-S-down>") 'move-text-down)
(global-set-key (kbd "<M-S-up>") 'move-text-up)

; set auto-saves to ~/.emacs.d/auto-save
(setq auto-save-file-name-transforms
          `((".*" ,(concat user-emacs-directory "auto-save/") t)))

; set backups to ~/.emacs.d/backups
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

(define-key dired-mode-map [f2] 'dired-efap)

(set-frame-font "Source Code Pro for Powerline-12")

(exec-path-from-shell-initialize)
;;(setq exec-path (append exec-path '("/usr/local/opt/llvm/lib" "/usr/local/opt/llvm/bin")))
(powerline-center-theme)
;;(setq sml/theme 'dark)
;;(sml/setup)
;; (require 'setup-helm-gtags)
(setq inhibit-startup-message t) ;; hide the startup message
(load-theme 'spacemacs-dark t)
(global-linum-mode t) ;; enable line numbers globally

(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

(dtrt-indent-mode 1)
(global-set-key (kbd "C-D") 'duplicate-thing)
(dtrt-indent-mode 1)
(global-set-key (kbd "C-D") 'duplicate-thing)

(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

(windmove-default-keybindings 'meta)

(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))


(load-theme 'airline-solarized-alternate-gui t)

(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"))

(yas-global-mode 1)
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd  "C-t t") 'yas-expand)

  
(defadvice undo-tree-undo (around keep-region activate)
  (if (use-region-p)
      (let ((m (set-marker (make-marker) (mark)))
            (p (set-marker (make-marker) (point))))
        ad-do-it
        (goto-char p)
        (set-mark m)
        (set-marker p nil)
        (set-marker m nil))
    ad-do-it))

(global-set-key (kbd "C-c g") 'magit-status)
(load-theme 'airline-solarized-alternate-gui t)


(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))
(global-set-key (kbd "C-c b") 'switch-to-previous-buffer)


(elpy-enable)

(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))


(setq powerline-utf-8-separator-left        #xe0b0
      powerline-utf-8-separator-right       #xe0b2
      airline-utf-glyph-separator-left      #xe0b0
      airline-utf-glyph-separator-right     #xe0b2
      airline-utf-glyph-subseparator-left   #xe0b1
      airline-utf-glyph-subseparator-right  #xe0b3
      airline-utf-glyph-branch              #xe0a0
      airline-utf-glyph-readonly            #xe0a2
      airline-utf-glyph-linenumber          #xe0a1)


; Enable company-mode in global.
(add-hook 'after-init-hook 'global-company-mode)

;; CMAKE - stuff
;; -----------------------------------------

; Add cmake listfile names to the mode list.
(require 'cmake-mode)
(setq auto-mode-alist
	  (append
	   '(("CMakeLists\\.txt\\'" . cmake-mode))
	   '(("\\.cmake\\'" . cmake-mode))
	   auto-mode-alist))
(autoload 'cmake-font-lock-activate "cmake-font-lock" nil t)
(add-hook 'cmake-mode-hook 'cmake-font-lock-activate)
; (autoload 'cmake-mode "~/CMake/Auxiliary/cmake-mode.el" t)


; (defun shk-yas/helm-prompt (prompt choices &optional display-fn)
;     "Use helm to select a snippet. Put this into `yas-prompt-functions.'"
;     (interactive)
;     (setq display-fn (or display-fn 'identity))
;     (if (require 'helm-config)
;         (let (tmpsource cands result rmap)
;           (setq cands (mapcar (lambda (x) (funcall display-fn x)) choices))
;           (setq rmap (mapcar (lambda (x) (cons (funcall display-fn x) x)) choices))
;           (setq tmpsource
;                	(list
;                  (cons 'name prompt)
;                  (cons 'candidates cands)
;                  '(action . (("Expand" . (lambda (selection) selection))))
;                  ))
;           (setq result (helm-other-buffer '(tmpsource) "*helm-select-yasnippet"))
;           (if (null result)
;               (signal 'quit "user quit!")
;             (cdr (assoc result rmap))))
;       nil))


;; REZ CONFIGURATION
;; --------------------------------------
(require 'cmake-rez)
(cmake-rez-check-active "~/dev/hive/queen")


;; RTAGS CONFIGURATION
;; --------------------------------------
(require 'rtags)
(require 'company-rtags)

(setq rtags-completions-enabled t)

(eval-after-load 'company
  '(add-to-list
    'company-backends 'company-rtags))

(setq rtags-autostart-diagnostics t)
(rtags-enable-standard-keybindings)

(require 'irony)
(require 'company-irony)
(require 'helm-rtags)

(setq rtags-use-helm t)
; (require 'company-clang)
(require 'company-yasnippet)
(require 'company-irony-c-headers)
(require 'flycheck-rtags)



; (setq cmake-ide-clang-flags-c '(getenv "LIBCLANG_CXXFLAGS"))
; (setq cmake-ide-clang-flags-c++ '(getenv "LIBCLANG_CXXFLAGS"))



;; Irony for C++ dev
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)



(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))

(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)


;; Company for C++ dev
(setq company-backends (delete 'company-semantic company-backends))

(eval-after-load 'company
  '(add-to-list
    'company-backends '(company-irony-c-headers company-irony)))

(setq company-idle-delay 0)
(define-key c-mode-map [(tab)] 'company-complete)
(define-key c++-mode-map [(tab)] 'company-complete)

;; Flycheck for c++ dev
(add-hook 'c++-mode-hook 'flycheck-mode)
(add-hook 'c-mode-hook 'flycheck-mode)
(require 'flycheck-rtags)

(defun my-flycheck-rtags-setup ()
  (flycheck-select-checker 'rtags)
  (setq-local flycheck-highlighting-mode nil)
  (setq-local flycheck-check-syntax-automatically nil))

(add-hook 'c-mode-comon-hook #'my-flycheck-rtags-setup)

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

(cmake-ide-setup)






; (add-hook 'c++-mode-hook 'company-mode)
; (add-hook 'c-mode-hook 'company-mode)


; (defun my-irony-mode-hook ()
;   (define-key irony-mode-map [remap completion-at-point]
;     'irony-completion-at-point-async)
;   (define-key irony-mode-map [remap complete-symbol]
;     'irony-completion-at-point-async))

; (add-hook 'irony-mode-hook 'my-irony-mode-hook)
; (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

; (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
; (setq company-backends (delete 'company-semantic company-backends))

; (eval-after-load 'company
;   '(add-to-list
;     'company-backends '(company-irony-c-headers company-irony)))

; (define-key c-mode-map [(C-tab)] 'company-complete)
; (define-key c++-mode-map [(C-tab)] 'company-complete)

; (setq company-idle-delay 0)


; (setq rtags-completions-enabled t)
; (eval-after-load 'company
;   '(add-to-list
;     'company-backends 'company-rtags))

; (setq rtags-autostart-diagnostics t)

; (rtags-enable-standard-keybindings)

; (setq rtags-use-helm t)
; (cmake-ide-setup)





; (custom-set-variables
;  ;; custom-set-variables was added by Custom.
;  ;; If you edit it by hand, you could mess it up, so be careful.
;  ;; Your init file should contain only one such instance.
;  ;; If there is more than one, they won't work right.
;  '(custom-safe-themes
;    (quote
;     ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb$
;  '(package-selected-packages
;    (quote
;     (helm-c-yasnippet yaml-mode undo-tree spacemacs-theme smart-mode-line s rtags puppet-mode org-projectile multiple-cursors materia$
;  '(safe-local-variable-values
;    (quote
;     ((cmake-ide-build-dir
;       (quote /mnt/pi/sys/profiles/maol/dev/cyclone/build))
;      (cmake-ide-build-dir "/mnt/pi/sys/profiles/maol/dev/cyclone/build/0")
;      (cmake-ide-build-dir
;       (quote
;        (concat
;         (projectile-project-root)
;         "build/0")))
;      (cmake-ide-build-dir
;       (concat
;        (projectile-project-root)
;        "build/0"))))))
; (custom-set-faces
;  ;; custom-set-faces was added by Custom.
;  ;; If you edit it by hand, you could mess it up, so be careful.
;  ;; Your init file should contain only one such instance.
;  ;; If there is more than one, they won't work right.
;  )




; (custom-set-variables
;  ;; custom-set-variables was added by Custom.
;  ;; If you edit it by hand, you could mess it up, so be careful.
;  ;; Your init file should contain only one such instance.
;  ;; If there is more than one, they won't work right.
;  '(package-selected-packages
;    (quote
;     (yaml-mode undo-tree spacemacs-theme smart-mode-line rtags puppet-mode org-projectile multiple-cursors material-theme magit-filenotify magit helm-c-yasnippet helm-flx helm-swoop helm-projectile helm flymake-puppet flycheck-pyflakes flycheck-irony flycheck elpy duplicate-thing dtrt-indent dired+ cmake-mode cmake-ide company-irony-c-headers company-irony company-flx company better-defaults airline-themes))))
; (custom-set-faces
;  ;; custom-set-faces was added by Custom.
;  ;; If you edit it by hand, you could mess it up, so be careful.
;  ;; Your init file should contain only one such instance.
;  ;; If there is more than one, they won't work right.
;  )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (yaml-mode undo-tree spacemacs-theme smart-mode-line rtags puppet-mode org-projectile multiple-cursors material-theme magit-filenotify magit helm-c-yasnippet helm-flx helm-swoop helm-projectile helm flymake-puppet flycheck-pyflakes flycheck-irony flycheck elpy duplicate-thing dtrt-indent dired+ cmake-mode cmake-ide company-irony-c-headers company-irony company-flx company better-defaults airline-themes)))
 '(send-mail-function (quote mailclient-send-it)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
