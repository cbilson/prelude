;;; psake.el -- run Psake from inside emacs

;; Copyright (C) 2012 Chris Bilson

;; Author: Chris Bilson <cbilson@pobox.com>
;; Version: 0.1.0
;; Keywords: powershell, psake, .net

;;; Commentary:

;; This package contains utility functions for running the psake build
;; automation tool from inside emacs.

(defcustom psake-build-script "default.ps1"
  "The build script to use when invoking psake.")

(defcustom psake-powershell "powershell"
  "The path or command to use to invoke powershell.")

(defcustom psake-module "psake"
  "The path to the psake.psm1 module you want to use. Note, powershell
will automatically find modules in ~/Documents/WindowsPowerShell/v1.0/Modules/<Module Name>, so if you put psake.psm1 there, you can just use the default value of
psake.psm1 for this setting.")

(defun psake--format-psake-property (prop)
  (let ((k (car prop))
        (v (cdr prop)))
    (format (if (stringp v)
                "%s=\"%s\""
              "%s=%s")
            k v)))

(defun psake--format-psake-properties (props)
  (when props
    (format "-Property @{%s}" (mapconcat 'psake--format-psake-property props ", "))))

(defun psake--find-project-root-directory ()
  (locate-dominating-file default-directory psake-build-script))

;;;##autoload
(defun psake-execute (targets &rest properties)
  "Execute psake targets in compile mode"
  (let ((compilation-buffer-name-function
         '(lambda (_) (concat "*psake " targets "*"))))
    (cd (psake--find-project-root-directory))
    (let ((cmd (concat psake-powershell
                       " -NoProfile -Command \"Import-Module "
                       psake-module
                       "; Invoke-Psake "
                       (when targets (concat "-t "  targets))
                       (psake--format-psake-properties properties)
                       "\"")))
      (compile cmd))))

(defun psake-execute-dominating-psake-build-script ()
  "Executes psake with the default target in the next Default.ps1
it finds up the directory tree from the current directory"
  (interactive)
  (psake-execute nil))

(provide 'psake)
