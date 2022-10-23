;;; company-eglot.el --- Company mode backend for Eglot  -*- lexical-binding: t -*-
;; Author: Tyler Dodge
;; Version: 0.5
;; Keywords: convenience, matching
;; Package-Requires: ((emacs "26.1") (company "0.8.0") (dash "2.19.1"))
;; URL: https://github.com/tyler-dodge/company-eglot
;; Git-Repository: git://github.com/tyler-dodge/company-eglot.git
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;;
;;;
;;; Commentary:
;;;
;;; Code:

(require 'rx)

(defcustom company-eglot-disable-resolve-modes
  '(swift-mode)
  "Major modes that are members of this list will not do an additional call to completionItem/resolve."
  :type 'list)

(defvar company-eglot--resolution-cache (ht)
  "Cache for calls to completionItem/resolve.")

(defvar company-eglot--prefix-regexp (rx (or ?< ?\n whitespace ?. ?: ?, ?= ?\) ?\} ?\]))
  "The regexp used to find when the prefix should start for company-mode.")

;;;###autoload
(defun company-eglot (command &optional arg &rest ignored)
  "Company backend for eglot."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-eglot))
    (prefix (company-eglot--prefix))
    (kind
     (plist-get (get-text-property 0 :eglot-item arg) :kind))
    (no-cache nil)
    (post-completion
     (when arg
       (company-eglot--cached-resolve
        (get-text-property 0 :eglot-item arg)
        (lambda (result)
          (atomic-change-group
            (-some--> (plist-get result :insertText)
              (progn (delete-char (- (length arg)))
                     (progn
                       (let ((trigger-char (char-before (point))))
                         (delete-char -1)
                         (if (s-starts-with-p "?" it)
                             (s-chop-prefix "init" it)
                           (s-chop-prefix ".init" (concat (list trigger-char) it))))))
              (progn
                (let ((end (save-excursion (insert it) (--doto (make-marker) (set-marker it (point))))))
                  (prog1
                      (buffer-substring (point) end)
                    (delete-region (point) end)
                    (set-marker end nil))))
              (yas-expand-snippet it (point) (point)))
            (cl-loop for edit in
                     (append (plist-get result :additionalTextEdits) nil)
                     do
                     (-let [(&plist
                             :newText new-text
                             :range (&plist :start (:line start-line :character start-col)
                                            :end (:line end-line :character end-col)))
                            edit]
                       (let ((start-pt (save-excursion
                                         (goto-char (point-min))
                                         (forward-line start-line)
                                         (forward-char start-col)
                                         (point))))
                         (delete-region
                          start-pt
                          (save-excursion
                            (goto-char (point-min))
                            (forward-line end-line)
                            (forward-char end-col)
                            (point)))
                         (save-excursion
                           (goto-char start-pt)
                           (insert new-text))))))))))
    (doc-buffer
     (cons :async
           (lambda (callback)
             (--doto (company-doc-buffer)
               (company-eglot--cached-resolve
                (get-text-property 0 :eglot-item arg)
                (lambda (item)
                  (funcall callback
                           (with-current-buffer it
                             (-some--> (plist-get item :label)
                               (insert it))
                             (insert  " -> ")
                             (-some--> (plist-get item :detail) (insert it))
                             (insert "\n\n")
                             (-some--> (plist-get item :documentation)
                               (plist-get it :value)
                               (insert it))
                             (current-buffer)))))))))
    (annotation
     (cons :async
           (lambda (callback)
             (company-eglot--cached-resolve (get-text-property 0 :eglot-item arg)
                                            (lambda (result)
                                              (funcall callback
                                                       (--> (plist-get result :detail)
                                                            (if (> (length it) 100) (substring it 0 100) it))))))))
    (sorted t)
    (candidates
     (let ((server (eglot--current-server-or-lose)))
       (when (not eglot--recent-changes)
         (setq-local eglot--recent-changes `((:line ,(line-number-at-pos (point)) :character ,(current-column)))))
       (setq eglot--versioned-identifier (1+ eglot--versioned-identifier))
       (ignore-errors (eglot--signal-textDocument/didChange))
       (cons :async
             (lambda (callback)
               (jsonrpc-async-request
                server
                :textDocument/completion (eglot--CompletionParams)
                :error-fn
                (lambda (error)
                  (message "Failed to complete with error: %S." error)
                  (funcall callback nil))
                :success-fn
                (lambda (result)
                  (funcall callback
                           (->> (append (plist-get result :items) nil)
                                (--filter (string-prefix-p arg (plist-get it :label)))
                                (--map (propertize (plist-get it :label) :eglot-item it)))))
                :deferred :textDocument/completion)))))))

;;;###autoload
(defun company-complete-with-eglot ()
  (interactive)
  (company-abort)
  (company-begin-backend 'company-eglot))

(defun company-eglot--prefix ()
  "Return the prefix for company-eglot at point."
  (and (eglot-managed-p)
       (-some--> (save-excursion
                   (re-search-backward company-eglot--prefix-regexp nil t)
                   (1+ (point)))
         (if (< it (point))
             (cons (buffer-substring it (max it (point))) t)
           (cons "" t)))))

(defun company-eglot--cached-resolve (item callback)
  "Handles calling resolve if the item is not in the cache already.
Skips the call immediately if the `major-mode' is a member of `company-eglot-disable-resolve-modes'."
  (cond
   ((memq major-mode company-eglot-disable-resolve-modes)
    (funcall callback item))
   (t
    (if-let ((resolved-item (ht-get company-eglot--resolution-cache (md5 (format "%S" (plist-get item :data))))))
        (prog1 t
          (funcall callback resolved-item))
      (let ((server (eglot--current-server-or-lose)))
        (jsonrpc-async-request
         server
         :completionItem/resolve item
         :error-fn (lambda (&rest args) (message "Failed to resolve: %S." args))
         :timeout-fn (lambda (&rest args) (message "Timed out when resolving: %S." args))
         :success-fn
         (lambda (result)
           (ht-set company-eglot--resolution-cache (md5 (format "%S" (plist-get result :data))) result)
           (funcall callback result))))))))

(provide 'company-eglot)
;;; company-eglot.el ends here
