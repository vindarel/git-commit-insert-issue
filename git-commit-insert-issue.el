;;; git-commit-insert-issue.el --- Get issues list when typing "Fixes #"

;; Copyright (C) 2015 vindarel <ehvince@mailz.org>

;; Author: Vindarel
;; URL: https://gitlab.com/emacs-stuff/git-commit-insert-issue/
;; Keywords: git, commit, issues
;; Version: 0.1.0
;; Package-Requires: ((helm "0") (projectile "0") (s "0") (github-issues "0") (gitlab "0"))
;; Summary: Get issues list when typeng "Fixes #" in a commit message. github only atm.

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This library provides a minor mode and an interactive function to
;; fetch issues of your project when you type "Fixes #" in a commit
;; message.

;;; Code:

(require 'helm)
(require 'projectile)
(require 's)
(require 'github-issues)
(require 'gitlab)

(defvar git-commit-insert-issue-helm-source
      '((name . "Select an issue")
        (candidates . issues-get-issues)
        (action . (lambda (candidate)
                    candidate))))

(defun git-commit-insert-issue-helm ()
  (interactive)
  (helm :sources '(git-commit-insert-issue-helm-source))
)

(defun git-username ()
  (s-trim (shell-command-to-string "git config user.name")))

(defun git-commit-insert-issue-gitlab-issues ()
  ""
  (interactive)
  (let* ((username (git-username))
         (project-name (projectile-project-name))
         (user-project (format "%s/%s" username project-name))
         (project (gitlab-get-project user-project))
         (project-id (assoc-default 'id project)))
    ;;XXX: needs unmerged feature of emacs-gitlab. See PR #40.
    (gitlab-list-project-issues user-project nil nil '((state . "opened")))))

(defun git-commit-insert-issue-gitlab-issues-format ()
  "Get issues and return a list of strings formatted with '#id - title'"
  (--map (format "#%i - %s" (assoc-default 'iid it) (assoc-default 'title it))
                 (git-commit-insert-issue-gitlab-issues)))

;;;###autoload
(defun git-commit-insert-issue-get-issues (&optional username project-name)
  "Get all the issues from the current project.
   Return a list."
  (let* ((username (or username (git-username)))
         (project-name (or project-name (projectile-project-name)))
         (issues (github-api-repository-issues username project-name)))
    (if (string= (plist-get issues ':message) "Not Found")
        `(,(concat "Not found with user " (git-username)) )
      (progn
        ;;todo: watch for api rate limit.
        (setq git-commit-insert-issue-project-issues (--map
                              (format "#%i - %s" (plist-get it ':number) (plist-get it ':title))
                              issues))
        ))))

(defvar git-commit-insert-issue-github-keywords '("Fixes" "fixes" "fix" "fixed"
                                "close" "closes" "closed"
                                "resolve" "resolves" "resolved") "List of keywords that github accepts to close issues.")

(defun git-commit-insert-issue--construct-regexp (kw)
  "From a list of words, constructs a regexp to match each one at
  a start of a line followed by a blank space:
  (\"fix\" \"close\") => \"^fix |^close \" "
  (let ((regexp (concat "^" (car kw) " ")))
    (concat regexp (mapconcat (lambda (it) (concat "\\|^" it " "))
               (cdr kw)
               ""))))

(defun git-commit-insert-issue--ask-issues ()
  "Ask for the issue to insert."
  (interactive)
  ;; This helm call doesn't work alone, but isn't actually needed.
  ;; (helm :sources '(issues-helm-source)))
  (insert (ido-completing-read "Choose the issue: " (git-commit-insert-issue-get-issues))))

(defun git-commit-insert-issue-gitlab-insert ()
  "Choose and insert the issue id"
  (interactive)
  (insert (ido-completing-read "Gitlab issue ? " (git-commit-insert-issue-gitlab-issues-format))))

(defun insert-issue--get-remotes ()
  "Get this repo's remote names"
  (s-split "\n" (s-trim (shell-command-to-string "git remote"))))

(defun insert-issue--get-first-remote ()
  "Get the first remote name found in git config. It should be the prefered one."
  (let* ((first-remote
          (with-temp-buffer
            (insert-file-contents (concat (projectile-project-root) ".git/config"))
            (if (search-forward "[remote \"")
                (progn
                  (buffer-substring-no-properties (line-beginning-position) (line-end-position))))))
         (first-remote (car (cdr (s-split " " first-remote))))
         (first-remote (s-replace "\"" "" first-remote))
         (first-remote (s-chop-suffix "]" first-remote)))
    first-remote))

(defun insert-issue--get-remote-url ()
  "Get the url of the first remote" ;XXX: shall we ask if many remotes ?
  (shell-command-to-string (format "git config remote.%s.url"
                                   ;; (-first-item (insert-issue--get-remotes))))) ;; -first-item may not be the one we want.
                                   (insert-issue--get-first-remote))))

(defun insert-issue--get-server ()
  "Check the gitlab host.
   From git@server.com:group/project.git, get server.com"
  (let* ((url (insert-issue--get-remote-url)) ;; git@gitlab.com:emacs-stuff/project-name.git
         (server-group-name (-first-item (cdr (s-split "@" url)))) ;; gitlab.com:emacs-stuff/project-name.git
         (server (car (s-split ":" server-group-name))) ;; gitlab.com
         )
  server))

(defun insert-issue--get-group ()
  "The remote group can be different than the author.
   From git@server.com:group/project.git, get group"
  (let* ((url (insert-issue--get-remote-url)) ;; git@gitlab.com:emacs-stuff/project-name.git
         (server-group-name (-first-item (cdr (s-split "@" url)))) ;; gitlab.com:emacs-stuff/project-name.git
         (group-project (cdr (s-split ":" server-group-name))) ;; emacs-stuff/project-name.git
         (group (-first-item (s-split "/" (-first-item group-project)))) ;; emacs-stuff
         )
    group))

;;;###Autoload
(define-minor-mode git-commit-insert-issue-mode
  "See the issues when typing 'Fixes #' in a commit message."
  :global nil
  :group 'git
  (if git-commit-insert-issue-mode
      (progn
        (define-key git-commit-mode-map "#"
          (lambda () (interactive)
            (setq git-commit-insert-issue-project-issues (git-commit-insert-issue-get-issues))
             (if (looking-back
                  (git-commit-insert-issue--construct-regexp git-commit-insert-issue-github-keywords))
                 (insert (git-commit-insert-issue-helm))
               (self-insert-command 1))))
        )
    (define-key git-commit-mode-map "#" (insert "#")) ;; works. Good ?
    ))

(provide 'git-commit-insert-issue)

;;; git-commit-insert-issue.el ends here
