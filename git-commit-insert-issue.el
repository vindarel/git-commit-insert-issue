(require 'github-issues)  ;; not in MELPA
(require 's)
(require 'projectile)

(defun git-username ()
  (s-trim (shell-command-to-string "git config user.name")))

(defun issues-get-issues (&optional username project-name)
(let* ((username (or username (git-username)))
       (project-name (or project-name (projectile-project-name)))
       (issues (github-api-repository-issues username project-name)))
  (if (string= (plist-get issues ':message) "Not Found")
      "Not Found"
    (progn
      ;;todo: watch for api rate limit.
      (setq issues-project (--map
                            (format "%i - %s" (plist-get it ':number) (plist-get it ':title))
                            issues))
      ))))


(setq issues-helm-source
      '((name . "HELM at the Emacs")
        (candidates . issues-get-issues) ;; BUG: "must be a function"
        (action . (lambda (candidate)
                    candidate))))

(helm :sources '(issues-helm-source))


(define-minor-mode git-commit-insert-issue-mode
  "See the issues when typing 'Fixes #' in a commit message."
  :global nil
  :group 'git
  (if git-commit-insert-issue-mode
      (progn
        (define-key git-commit-mode-map "#"
          (lambda () (interactive)
            (setq issues-project (issues-get-issues))
             (if (looking-back "^Fixes ")
                 (insert (helm :sources '(issues-helm-source)))
               (self-insert-command 1))))
        )
    (define-key git-commit-mode-map "#" (insert "#")) ;; good ?
    ))

(provide 'git-commit-insert-issue)
;; git-commit-insert-issue ends here
