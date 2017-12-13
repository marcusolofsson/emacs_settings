;;; cmake-rez.el  --- configures the cmake-ide to use rez if there is a package.py file in the root of the project.
;;; Commentary:
;; asdf
;; asdfasdf asdfasdf

;;; Copyright 2017 Marcus Olofsson.

;;; Code:

(require 'eshell)
(require 's)

(defcustom cmake-rez-is-active
  nil
  "Wheter a this is a rez-package or not."
  :group 'cmake-rez
  :type 'booleanp
  :safe #'booleanp)

(defcustom cmake-rez-packages
  nil
  "List of packages that is requested by rez."
  :group 'cmake-rez
  :type '(repeat string))

(defcustom cmake-rez-module-paths
  nil
  "The modules that rez supplies."
  :group 'cmake-rez
  :type '(repeat string))


(defun cmake-rez--active-var ()
  "Return if rez is active in this here."
  (cmake-rez-active))

(defun cmake-rez-check-active (cmake-root-dir)
  "Check if the cmake build is using rez to solve dependecies with CMAKE-ROOT-DIR."
  
  (if (file-exists-p (expand-file-name "package.py" cmake-root-dir))
      (setq cmake-rez-is-active t)
    (setq cmake-rez-is-active nil)))

(defun cmake-rez-get-resolve (cmake-root-dir)
  "Resolve the dependicies that is required for this project. CMAKE-ROOT-DIR"
)

(defun cmake-rez--set-rez-env-variables (project-dir)
  "Set the environment variables that rez needs for it to be used picks data from PROJECT-DIR."
  (setq envs (eshell-command-result (concat "rez-python "
                                            (concat user-emacs-directory "/site-lisp/rez-info.py " project-dir "cpp"))))
  (setq env-list (s-split " " envs))
  (while (env-list)
    (setenv (first (s-split ":" env-list)) (car (last (s-split ":" (first env-list)))))
    (setq env-list (cdr env-list))))

(defun cmake-rez--get-cmake-vars (project-dir)
  "Return the cmake arguments for the cmake run from PROJECT-DIR."
  (car (last
        (s-split "\n"
                 (s-trim-right
                  (eshell-command-result
                   (concat "rez-python "
                           (concat user-emacs-directory "/site-lisp/rez-resolve_cmake.py " project-dir))))))))


(provide 'cmake-rez)
;;; cmake-rez.el ends here
