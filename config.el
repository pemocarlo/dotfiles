;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; refresh' after modifying this file!


;; These are used for a number of things, particularly for GPG configuration,
;; some email clients, file templates and snippets.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "monospace" :size 14)
      doom-variable-pitch-font (font-spec :family "ETBembo" :size 15))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. These are the defaults.
;; (setq doom-theme 'doom-one)
(setq doom-theme 'doom-solarized-light)

;; If you intend to use org, it is recommended you change this!
(setq org-directory "~/my_files/")

(setq +org-enable-centralized-exports nil)

(after! org
  (setq +org-capture-todo-file "projects/breathe.org")
  (setq +org-capture-projects-file "projects/projects.org")
  (setq org-agenda-files '("~/my_files/projects/breathe.org"))
  (setq org-agenda-files '("~/my_files/projects/breathe.org"))
  (setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))

  ;; Make it possible to reference code blocks, figures etc.
  (setq org-ref-completion-library 'org-ref-ivy-cite)
  )



(after! org-journal
  (defun org-journal-save-entry-and-exit()
    "Simple convenience function.
  Saves the buffer of the current day's entry and kills the window
  Similar to org-capture like behavior"
    (interactive)
    (save-buffer)
    (kill-buffer-and-window))
  (define-key org-journal-mode-map (kbd "C-x C-s") 'org-journal-save-entry-and-exit)
  (setq org-journal-file-header 'org-journal-file-header-func)

  (defun org-journal-file-header-func ()
  "Custom function to create journal header."
  (concat
    (pcase org-journal-file-type
      (`daily "#+TITLE: Daily Journal\n#+STARTUP: showeverything")
      (`weekly "#+TITLE: Weekly Journal\n#+STARTUP: folded")
      (`monthly "#+TITLE: Monthly Journal\n#+STARTUP: folded")
      (`yearly "#+TITLE: Yearly Journal\n#+STARTUP: folded")))))

(after! org
  (defun org-journal-find-location ()
  ;; Open today's journal, but specify a non-nil prefix argument in order to
  ;; inhibit inserting the heading; org-capture will insert the heading.
  (org-journal-new-entry t)
  ;; Position point on the journal's top-level heading so that org-capture
  ;; will add the new entry as a child entry.
  (goto-char (point-min)))

  (add-to-list 'org-capture-templates
               '("d" "diary"))

  (add-to-list 'org-capture-templates
               '("dn" "New Diary Entry" entry (function org-journal-find-location)
                 (file "~/my_files/templates/diary_entry.org" )))

  (add-to-list 'org-capture-templates
               '("ds" "New Diary Summary" entry (function org-journal-find-location)
                 (file "~/my_files/templates/diary_summary.org")))
  (add-to-list 'org-capture-templates
               '("dl" "New Diary log" entry(function org-journal-find-location)
                 (file "~/my_files/templates/diary_log.org")))

  (defun my-new-weekly-review ()
  (interactive)
  (let ((org-capture-templates '(("w" "Review: Weekly Review" entry (file+olp+datetree "~/my_files/reviews.org")
                                  (file "~/my_files/templates/weekly_review.org")))))
    (progn
      (org-capture nil "w")
      (org-capture-finalize t)
      (org-speed-move-safe 'outline-up-heading)
      (org-narrow-to-subtree)
      ;; (fetch-calendar)
      (org-clock-in)))))

  
(setenv "WORKON_HOME" "/home/cperezm/miniconda3/envs")

;; If you want to change the style of line numbers, change this to `relative' or
;; `nil' to disable it:
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', where Emacs
;;   looks when you load packages with `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

;; Grammar check
(setq langtool-language-tool-jar "~/LanguageTool-4.8/languagetool-commandline.jar" )

(after! dap-mode

  ;; (dap-register-debug-provider
 ;; "cppdbg"
 ;; (lambda (conf)
 ;;   (plist-put conf
 ;;              :dap-server-path
 ;;              '("/home/kyoncho/.vscode/extensions/ms-vscode.cpptools-0.19.0/debugAdapters/OpenDebugAD7"))
 ;;   conf))

(dap-register-debug-template "C++ Run Simulator:lldb"
                             (list :type "lldb"
                                   :cwd "/home/cperezm/master-HMDA/ws19-20/advPT/project/starcraft2-simulator"
                                   :request "launch"
                                   :program "/home/cperezm/master-HMDA/ws19-20/advPT/project/starcraft2-simulator/build/bin/SC2Simulator"
                                   :name "Run simulator lldb"))

(dap-register-debug-template "C++ Run Simulator:gdb"
                             (list :type "gdb"
                                   :cwd "/home/cperezm/master-HMDA/ws19-20/advPT/project/starcraft2-simulator"
                                   :request "launch"
                                   :program "/home/cperezm/master-HMDA/ws19-20/advPT/project/starcraft2-simulator/build/bin/SC2Simulator"
                                   :name "Run simulator gdb"))

  )

;; ~/.doom.d/config.el
(setq +python-ipython-repl-args '("-i" "--simple-prompt" "--no-color-info"))
(setq +python-jupyter-repl-args '("--simple-prompt"))


(use-package! org-ref
  :after org
  :demand t
  :config
  (setq org-ref-default-bibliography '("~/my_files/courses/videos/latex/manuscript.bib")
        bibtex-completion-pdf-field "file"
        org-ref-default-citation-link "parencite")

  (defun org-ref-open-pdf-at-point-in-emacs ()
    "Open the pdf for bibtex key under point if it exists."
    (interactive)
    (let* ((results (org-ref-get-bibtex-key-and-file))
           (key (car results))
           (pdf-file (funcall org-ref-get-pdf-filename-function key)))
      (if (file-exists-p pdf-file)
          (find-file-other-window pdf-file)
        (message "no pdf found for %s" key))))

  (defun org-ref-open-in-scihub ()
    "Open the bibtex entry at point in a browser using the url field or doi field.
Not for real use, just here for demonstration purposes."
    (interactive)
    (let ((doi (org-ref-get-doi-at-point)))
      (when doi
        (if (string-match "^http" doi)
            (browse-url doi)
          (browse-url (format "http://sci-hub.se/%s" doi)))
        (message "No url or doi found"))))

  (setq org-ref-helm-user-candidates
        '(("Open in Sci-hub"  . org-ref-open-in-scihub)
          ("Open in Emacs" . org-ref-open-pdf-at-point-in-emacs)))
  )


(use-package! ox-word
  :after (:all org-ref ox)
  :demand t)
