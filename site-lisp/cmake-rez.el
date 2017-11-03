;;; cmake-rez.el  --- configures the cmake-ide to use rez if there is a package.py file in the root of the project.
;;; Commentary:
;; asdf
;; asdfasdf asdfasdf

;;; Copyright 2017 Marcus Olofsson.

(require 'eshell)


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
  'type '(repeat string))


(defun cmake-rez--active-var ()
  "Return if rez is active in this here."
  (cmake-rez-active))

(defun cmake-rez-check-active (cmake-root-dir)
  "Check if the cmake build is using rez to solve dependecies."
  
  (if (file-exists-p (expand-file-name "package.py" cmake-root-dir))
      (setq cmake-rez-is-active t)
    (setq cmake-rez-is-active nil)))

(defun cmake-rez-get-resolve (cmake-root-dir)
  "Resolve the dependicies that is required for this project."
  )

(defun cmake-rez--set-rez-env-variables (cmake-root-dir)
  "Set the environment variables that rez needs for it to be used."
  (setenv "REZ_BUILD_PROJECT_NAME" )
  (setenv "REZ_BUILD_REQUIRES_UNVERSIONED" )
  (setenv "REZ_BUILD_PROJECT_VERSION" ))
  

(defun cmake-rez--get-cmake-vars ()
  "Return the cmake arguments for the cmake run."
  eshell-command-result (concat "python " (concat user-emacs-directory "/site-lisp/rez-resolve_cmake.py")))


(provide 'cmake-rez)
