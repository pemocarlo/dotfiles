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
(setq org-roam-directory "~/my_files/my-notes/org/")

(after! org

  (defvar -org-default-projects-dir (concat org-directory  "projects/")
    "Primary GTD directory.")

  (defvar -org-default-technical-dir
    (concat org-directory  "technical/")
    "Directory of shareable notes.")

  (defvar -org-default-personal-dir
    (concat org-directory  "personal/")
    "Directory of un-shareable, personal notes.")

  (defvar -org-default-completed-dir
    (concat -org-default-projects-dir "trophies/")
    "Directory of completed project files.")

  (defvar -org-default-inbox-file
    (concat -org-default-projects-dir "breathe.org")
    "New stuff collects in this file.")

  (defvar -org-default-tasks-file
    (concat -org-default-projects-dir "tasks.org")
    "Tasks, TODOs and little projects.")

  (defvar -org-default-incubate-file
    (concat -org-default-projects-dir "incubate.org")
    "Ideas simmering on backburner.")

  (defvar -org-default-completed-file
    nil
    "Finished projects")

  (defvar -org-default-notes-file
    (concat -org-default-personal-dir "general-notes.org")
    "Non-actionable, personal notes.")

  (defvar -org-default-media-file
    (concat -org-default-projects-dir "media.org")
    "White papers and links to other things to check out.")

  (defhydra hydra-org-refiler ( :hint nil)
    "
  ^Navigate^      ^Refile^       ^Move^           ^Update^        ^Go To^        ^Dired^
  ^^^^^^^^^^---------------------------------------------------------------------------------------
  _k_: ↑ previous _t_: tasks     _m X_: projects  _T_: todo task  _g t_: tasks    _g X_: projects
  _j_: ↓ next     _i_: incubate  _m P_: personal  _S_: schedule   _g i_: incubate _g P_: personal
  _c_: archive    _p_: personal  _m T_: technical _D_: deadline   _g x_: inbox    _g T_: technical
  _d_: delete     _r_: refile                   _R_: rename     _g n_: notes    _g C_: completed
  "
    ("<up>" org-previous-visible-heading)
    ("<down>" org-next-visible-heading)
    ("k" org-previous-visible-heading)
    ("j" org-next-visible-heading)
    ("c" org-archive-subtree-as-completed)
    ("d" org-cut-subtree)
    ("t" org-refile-to-task)
    ("i" org-refile-to-incubate)
    ("p" org-refile-to-personal-notes)
    ("r" org-refile)
    ("m X" org-refile-to-projects-dir)
    ("m P" org-refile-to-personal-dir)
    ("m T" org-refile-to-technical-dir)
    ("T" org-todo)
    ("S" org-schedule)
    ("D" org-deadline)
    ("R" org-rename-header)
    ("g t" (find-file-other-window -org-default-tasks-file))
    ("g i" (find-file-other-window -org-default-incubate-file))
    ("g x" (find-file-other-window -org-default-inbox-file))
    ("g c" (find-file-other-window -org-default-completed-file))
    ("g n" (find-file-other-window -org-default-notes-file))
    ("g X" (dired -org-default-projects-dir))
    ("g P" (dired -org-default-personal-dir))
    ("g T" (dired -org-default-technical-dir))
    ("g C" (dired -org-default-completed-dir))
    ("[\t]" (org-cycle))
    ("s" (org-save-all-org-buffers) "save")
    ("q" nil "quit")
    )

  (map! :map org "C-c s" #'hydra-org-refiler/body)
  ;; (bind-key "C-c s" 'hydra-org-refiler/body)

  (defun org-refile-to-incubate ()
    "Refile (move) the current Org subtree to `-org-default-incubate-file'."
    (interactive)
    (org-refile nil nil (list nil -org-default-incubate-file nil nil)))

  (defun org-refile-to-task ()
    "Refile (move) the current Org subtree to `-org-default-tasks-file'."
    (interactive)
    (org-refile nil nil (list nil -org-default-tasks-file nil nil)))

  (defun org-refile-to-personal-notes ()
    "Refile (move) the current Org subtree to `-org-default-notes-file'."
    (interactive)
    (org-refile nil nil (list nil -org-default-notes-file nil nil)))

  (defun org-refile-to-completed ()
    "Refile (move) the current Org subtree to `org-default-completed-file',
unless it doesn't exist, in which case, refile to today's journal entry."

    (interactive)
    (if (and org-default-completed-file (file-exists-p
                                         -org-default-completed-file))
        (org-refile nil nil (list nil -org-default-completed-file nil nil))
      (org-refile nil nil (list nil get-journal-file-today nil nil))))

  (defun org-rename-header (label)
    "Rename the current section's header to LABEL, and moves the
point to the end of the line."
    (interactive (list
                  (read-string "Header: "
                               (substring-no-properties (org-get-heading t t t t)))))
    (org-back-to-heading)
    (replace-string (org-get-heading t t t t) label))

  (defun todays-journal-entry ()
    "Return the full pathname to the day's journal entry file.
Granted, this assumes each journal's file entry to be formatted
with year/month/day, as in `20190104' for January 4th.

Note: `org-journal-dir' variable must be set to the directory
where all good journal entries live, e.g. ~/journal."
    (let* ((daily-name   (format-time-string "%Y%m%d"))
           (file-name    (concat org-journal-dir daily-name)))
      (expand-file-name file-name)))

  (defun org-archive-subtree-as-completed ()
    "Archives the current subtree to today's current journal entry."
    (interactive)
    ;; According to the docs for `org-archive-subtree', the state should be
    ;; automatically marked as DONE, but I don't notice that:
    (when (org-get-todo-state)
      (org-todo "DONE"))

    (let* ((org-archive-file (or -org-default-completed-file
                                 (todays-journal-entry)))
           (org-archive-location (format "%s::" org-archive-file)))
      (org-archive-subtree)))



  (defun org-subtree-metadata ()
    "Return a list of key aspects of an org-subtree. Includes the
following: header text, body contents, list of tags, region list
of the start and end of the subtree."
    (save-excursion
      ;; Jump to the parent header if not already on a header
      (when (not (org-at-heading-p))
        (org-previous-visible-heading 1))

      (let* ((context (org-element-context))
             (attrs   (second context))
             (props   (org-entry-properties)))

        (list :region     (list (plist-get attrs :begin) (plist-get attrs :end))
              :header     (plist-get attrs :title)
              :tags       (org-get-subtree-tags props)
              :properties (org-get-subtree-properties attrs)
              :body       (org-get-subtree-content attrs)))))

  (defun org-get-subtree-tags (&optional props)
    "Given the properties, PROPS, from a call to
`org-entry-properties', return a list of tags."
    (unless props
      (setq props (org-entry-properties)))
    (let ((tag-label (if org-get-subtree-tags-inherited "ALLTAGS" "TAGS")))
      (-some->> props
        (assoc tag-label)
        cdr
        substring-no-properties
        (s-split ":")
        (--filter (not (equalp "" it))))))

  (defvar org-get-subtree-tags-inherited t
    "Returns a subtree's tags, and all tags inherited (from tags
  specified in parents headlines or on the file itself). Defaults
  to true.")

  (defun org-get-subtree-properties (attributes)
    "Return a list of tuples of a subtrees properties where the keys are strings."

    (defun symbol-upcase? (sym)
      (let ((case-fold-search nil))
        (string-match-p "^:[A-Z]+$" (symbol-name sym))))

    (defun convert-tuple (tup)
      (let ((key (first tup))
            (val (second tup)))
        (list (substring (symbol-name key) 1) val)))

    (->> attributes
         (-partition 2)                         ; Convert plist to list of tuples
         (--filter (symbol-upcase? (first it))) ; Remove lowercase tuples
         (-map 'convert-tuple)))

  (defun org-get-subtree-content (attributes)
    "Return the contents of the current subtree as a string."
    (let ((header-components '(clock diary-sexp drawer headline inlinetask
                                     node-property planning property-drawer section)))

      (goto-char (plist-get attributes :contents-begin))

      ;; Walk down past the properties, etc.
      (while
          (let* ((cntx (org-element-context))
                 (elem (first cntx))
                 (props (second cntx)))
            (when (member elem header-components)
              (goto-char (plist-get props :end)))))

      ;; At this point, we are at the beginning of what we consider
      ;; the contents of the subtree, so we can return part of the buffer:
      (buffer-substring-no-properties (point) (org-end-of-subtree))))

  (defun org-refile-subtree-to-file (dir)
    "Archive the org-mode subtree and create an entry in the
directory folder specified by DIR. It attempts to move as many of
the subtree's properties and other features to the new file."
    (interactive "DDestination: ")
    (let* ((props      (org-subtree-metadata))
           (head       (plist-get props :header))
           (body       (plist-get props :body))
           (tags       (plist-get props :tags))
           (properties (plist-get props :properties))
           (area       (plist-get props :region))
           (filename   (org-filename-from-title head))
           (filepath   (format "%s/%s.org" dir filename)))
      (apply #'delete-region area)
      (org-create-org-file filepath head body tags properties)))

  (defun org-create-org-file (filepath header body tags properties)
    "Create a new Org file by FILEPATH. The contents of the file is
pre-populated with the HEADER, BODY and any associated TAGS."
    (find-file-other-window filepath)
    (org-set-file-property "TITLE" header t)
    (when tags
      (org-set-file-property "FILETAGS" (s-join " " tags)))

    ;; Insert any drawer properties as #+PROPERTY entries:
    (when properties
      (goto-char (point-min))
      (or (re-search-forward "^\s*$" nil t) (point-max))
      (--map (insert (format "#+PROPERTY: %s %s" (first it) (second it))) properties))

    ;; My auto-insert often adds an initial headline for a subtree, and in this
    ;; case, I don't want that... Yeah, this isn't really globally applicable,
    ;; but it shouldn't cause a problem for others.
    (when (re-search-forward "^\\* [0-9]$" nil t)
      (replace-match ""))

    (delete-blank-lines)
    (goto-char (point-max))
    (insert "\n")
    (insert body))

  (defun org-filename-from-title (title)
    "Creates a useful filename based on a header string, TITLE.
For instance, given the string:    What's all this then?
     This function will return:    whats-all-this-then"
    (let* ((no-letters (rx (one-or-more (not alphanumeric))))
           (init-try (->> title
                          downcase
                          (replace-regexp-in-string "'" "")
                          (replace-regexp-in-string no-letters "-"))))
      (string-trim init-try "-+" "-+")))

  (defun org-set-file-property (key value &optional spot)
    "Make sure file contains a top-level, file-wide property.
KEY is something like `TITLE' or `FILETAGS'. This function makes
sure that the property contains the contents of VALUE, and if the
file doesn't have the property, it is inserted at either SPOT, or
if nil,the top of the file."
    (save-excursion
      (goto-char (point-min))
      (let ((case-fold-search t))
        (if (re-search-forward (format "^#\\+%s:\s*\\(.*\\)" key) nil t)
            (replace-match value nil nil nil 1)

          (cond
           ;; if SPOT is a number, go to it:
           ((numberp spot) (goto-char spot))
           ;; If SPOT is not given, jump to first blank line:
           ((null spot) (progn (goto-char (point-min))
                               (re-search-forward "^\s*$" nil t)))
           (t (goto-char (point-min))))

          (insert (format "#+%s: %s\n" (upcase key) value))))))

  (defun org-refile-to-projects-dir ()
    "Move the current subtree to a file in the `projects' directory."
    (interactive)
    (org-refile-subtree-to-file -org-default-projects-dir))

  (defun org-refile-to-technical-dir ()
    "Move the current subtree to a file in the `technical' directory."
    (interactive)
    (org-refile-subtree-to-file -org-default-technical-dir))

  (defun org-refile-to-personal-dir ()
    "Move the current subtree to a file in the `personal' directory."
    (interactive)
    (org-refile-subtree-to-file -org-default-personal-dir))

  ;; (setq org-refile-targets (append '((-org-default-media-file :level . 1)
  ;;                                     (-org-default-notes-file :level . 0))
  ;;                                   (->>
  ;;                                    (directory-files -org-default-projects-dir nil ".org")
  ;;                                    (-remove-item (file-name-base -org-default-media-file))
  ;;                                    (--remove (s-starts-with? "." it))
  ;;                                    (--map (format "%s/%s" -org-default-projects-dir it))
  ;;                                    (--map `(,it :level . 0)))

  ;;                                   (->>
  ;;                                    (directory-files -org-default-technical-dir nil ".org")
  ;;                                    (--map (format "%s/%s" -org-default-technical-dir it))
  ;;                                    (--map `(,it :level . 0)))

  ;;                                   ))

  ;; (add-to-list org-refile-targets  '((concat -org-default-completed-dir "master-completed.org") :maxlevel . 2 ))

  (add-to-list 'org-refile-targets '("~/my_files/projects/trophies/master-completed.org" :maxlevel . 2))
  (add-to-list 'org-refile-targets '("~/my_files/personal/life_improvement.org" :maxlevel . 1))
  (add-to-list 'org-refile-targets '("~/my_files/personal/conversational.org" :maxlevel . 2))

  (setq +org-capture-todo-file "projects/breathe.org")
  (setq +org-capture-projects-file "projects/projects.org")
  (setq org-agenda-files (list -org-default-projects-dir))
  (setq org-agenda-file-regexp "^[a-z0-9-_]+.org")
  (setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))

  (after! org-agenda
    (setq org-agenda-block-separator nil
          org-agenda-start-with-log-mode t)

    (setq org-agenda-custom-commands
          `(("z" "Custom view"
             ((agenda ""
                      ((org-agenda-span 'day)
                       (org-deadline-warning-days 365)))
              (todo "TODO"
                    ((org-agenda-overriding-header "To Refile")
                     (org-agenda-files `(,-org-default-inbox-file))))
              (todo "TODO"
                    ((org-agenda-overriding-header "Projects")
                     (org-agenda-files `(,(concat -org-default-projects-dir "master.org")))))
              (todo "TODO"
                    ((org-agenda-overriding-header "One-off Tasks")
                     (org-agenda-files `(,-org-default-tasks-file))
                     (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))))
              )))))


  ;; (add-to-list 'org-agenda-custom-commands '(my/org-agenda-todo-view))

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


(use-package! org-roam
  :hook
  (after-init . org-roam-mode)
  :custom-face
  (org-roam-link ((t (:inherit org-link :foreground "#005200"))))
  :init
  (setq
   org-roam-db-location "~/my_files/org-roam.db"
   org-roam-graph-exclude-matcher "private")
  (setq org-roam-buffer-position 'left)
  :config
  (require 'org-roam-protocol)
  (setq org-roam-capture-templates
        '(("d" "default" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "${slug}"
           :head "#+SETUPFILE:./hugo_setup.org
#+HUGO_SECTION: zettels
#+HUGO_SLUG: ${slug}
#+TITLE: ${title}\n"
           :unnarrowed t)
          ("p" "private" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "private-${slug}"
           :head "#+TITLE: ${title}\n"
           :unnarrowed t)))
  (setq org-roam-ref-capture-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "websites/${slug}"
           :head "#+SETUPFILE:./hugo_setup.org
#+ROAM_KEY: ${ref}
#+HUGO_SLUG: ${slug}
#+TITLE: ${title}
- source :: ${ref}"
           :unnarrowed t))))


(after! org-roam
  (defun my/org-roam--backlinks-list (file)
    (if (org-roam--org-roam-file-p file)
        (--reduce-from
         (concat acc (format "- [[file:%s][%s]]\n"
                             (file-relative-name (car it) org-roam-directory)
                             (org-roam--get-title-or-slug (car it))))
         "" (org-roam-sql [:select [file-from]
                                   :from file-links
                                   :where (= file-to $s1)
                                   :and file-from :not :like $s2] file "%private%"))
      ""))

  (defun my/org-export-preprocessor (_backend)
    (let ((links (my/org-roam--backlinks-list (buffer-file-name))))
      (unless (string= links "")
        (save-excursion
          (goto-char (point-max))
          (insert (concat "\n* Backlinks\n" links))))))
  (add-hook 'org-export-before-processing-hook 'my/org-export-preprocessor))



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
(setq python-shell-prompt-detect-failure-warning nil)
(setq +python-ipython-repl-args '("-i" "--simple-prompt" "--no-color-info"))
(setq +python-jupyter-repl-args '("--simple-prompt"))


(use-package! org-ref
  :after org
  :demand t
  :config
  (setq
   org-ref-default-bibliography '("~/my_files/references.bib")
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

(use-package! org-noter
  :after org
  :config
  (setq org-noter-default-notes-file-names nil
        ;; org-noter-always-create-frame nil
        org-noter-notes-search-path '("~/my_files/research-notes")
        org-noter-separate-notes-from-heading t
        org-noter-auto-save-last-location t)
  )


(use-package! org-re-reveal-ref
  :after (:all org-re-reveal org-ref ox)
  :demand t)
