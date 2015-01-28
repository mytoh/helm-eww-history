;;; helm-eww-history -*- lexical-binding: t; coding: utf-8; -*-

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'seq)

(defface helm-eww-history-title
    '((t :inherit font-lock-preprocessor-face))
  "face for history title"
  :group 'eww
  :group 'helm)

(defface helm-eww-history-url
    '((t :inherit font-lock-builtin-face))
  "face for history title"
  :group 'eww
  :group 'helm)

(cl-defun helm-eww-history-init ()
  (unless eww-history
    (user-error "No history"))
  (setq helm-eww-history-candidates
        (helm-eww-history-create-candidates)))

(cl-defun helm-eww-history-create-candidates ()
  (seq-map
   (lambda (hist)
     (cons
      (seq-concatenate 'string
                       (propertize (cl-getf hist :title)
                                   'face 'helm-eww-history-title)
                       " "
                       (propertize (cl-getf hist :url)
                                   'face 'helm-eww-history-url))
      hist))
   eww-history))

(cl-defun helm-eww-history-action-browse (candidate)
  (eww-browse-url (cl-getf candidate :url)))

(cl-defun helm-eww-history-action-copy-url (candidate)
  (cl-letf ((url (cl-getf candidate :url)))
    (kill-new url)
    (message "copied %s" url)))

(defclass helm-eww-history-source (helm-source-sync)
  ((init :initform helm-eww-history-init)
   (candidates :initform helm-eww-history-candidates)
   (action :initform
           (helm-make-actions
            "Browse history page" 'helm-eww-history-action-browse
            "Copy url" 'helm-eww-history-action-copy-url))))

(defvar helm-source-eww-history
  (helm-make-source "History"
      'helm-eww-history-source))

;;;###autoload
(cl-defun helm-eww-history ()
  "helm source for eww histories"
  (interactive)
  (helm :sources '(helm-source-eww-history)
        :buffer "*helm eww history*"
        :prompt "History: "))

(provide 'helm-eww-history)

;;; helm-eww-history.el ends here
