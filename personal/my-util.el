;;; my-util --- utility functions I use
;;; Commentary:
;;; Code:

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

(eval-when-compile (require 'cl))

(defvar my-packages nil
  "The packages I use.")

(defvar my-init-hook '()
  "Gets called after my custom.el is loaded.")

(defvar my-after-init-time nil
  "Set to true after my customizations have been all loaded.")

(defun my-install-packages ()
  "Install any of my packages that aren't already installed."
  (dolist (package (remove-if 'package-installed-p my-packages))
    (package-install package)))

(defun my-add-load-paths (&rest paths)
  "Add PATHS as load paths."
  (setq load-path (concatenate 'list load-path paths)))

(defun my-add-packages (&rest packages)
  "Add site-specific PACKAGES to be installed."
  (setq my-packages (concatenate 'list my-packages packages)))

(defun my-add-eval-after-init (form)
  "Like eval prelude-eval-after-init, but evaluate FORM after my custom file is done."
  (if my-after-init-time
      (eval form)
    (add-hook 'my-init-hook (list 'lambda nil form))))

(defun my--site-customizations ()
  "Get available site-specific customization files."
  (let* ((base-names (list (symbol-name system-type) system-name user-login-name))
         (paths (mapcar (lambda (base-name)
                          (format "%s/%s.el" prelude-personal-dir base-name))
                        base-names)))
    (remove-if-not 'file-exists-p  paths)))

(defun my-load-extra-customizations ()
  "Load extra customizations defined for the OS, machine name, or user name."
  (dolist (file (my--site-customizations))
    (load file)))

(defun my-tabify-buffer ()
  "Use hard tabs in the current buffer."
  (tabify (point-min) (point-max)))

(defun my-concat-string-list (list)
  "Return a string which is a concatenation of all elements of LIST separated by spaces."
  (mapconcat '(lambda (obj) (format "%s" obj)) list " "))

(defun my-recentf-ido-find-file-other-window ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file-other-window file))))

(provide 'my-util)
;;; my-util ends here
