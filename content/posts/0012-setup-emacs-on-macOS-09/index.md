+++
title = 'Fresh Setup Emacs on macOS - E09'
date = 2024-06-09T21:24:18-07:00
toc = true
tags = ['emacs']
+++

## IDE for Python (`eglot`)

Eglot seems to be an awesome package for plain text programming. And eglot is already integrated with emacs 29. To enable it, you just need to run `M-x eglot` and you can have the programming language server setup.

![eglot-python](eglot-python.png)

## IDE for LaTeX (`auctex`)

The classical package for editing LaTeX is `auctex`. My legacy configuration is like below

```elisp
(use-package tex
  :ensure auctex)
(setq-default TeX-master nil)
(setq TeX-parse-self t)
(setq TeX-engine 'xetex)
(setq TeX-command-extra-options "-shell-escape")
(setq TeX-electric-sub-and-superscript t)
(setq TeX-auto-save t)
(setq TeX-command-default "XeLaTeX")
(setq TeX-save-query nil)
(setq TeX-show-compilation nil)
(setq TeX-source-correlate-start-server t)
(add-to-list 'LaTeX-verbatim-environments "minted")
(add-to-list 'TeX-command-list
             '("XeLaTeX" "%`xelatex%(mode)%' -shell-escape -synctex=1 %t" TeX-run-TeX nil t))
;; come back to tex file after compilation finishes
(add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
```

![auctex](auctex.png)

## RSS Reader (`elfeed`)

Emacs famous RSS reader is `elfeed`, below is my legacy configuration.

```elisp
(use-package elfeed)
(setq elfeed-search-title-max-width 150)
(defun concatenate-authors (authors-list)
  (mapconcat (lambda (author) (plist-get author :name)) authors-list ", "))
(defun my-search-print-fn (entry)
  (let* ((date (elfeed-search-format-date (elfeed-entry-date entry)))
         (title (or (elfeed-meta entry :title) (elfeed-entry-title entry) ""))
         (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
         (feed (elfeed-entry-feed entry))
         (feed-title (when feed (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
         (entry-authors (concatenate-authors (elfeed-meta entry :authors)))
         (tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
         (tags-str (mapconcat (lambda (s) (propertize s 'face 'elfeed-search-tag-face)) tags ","))
         (title-width (- (window-width) 10 elfeed-search-trailing-width))
         (title-column (elfeed-format-column
                        title (elfeed-clamp elfeed-search-title-min-width
					    title-width elfeed-search-title-max-width)
                        :left))
         (entry-score (elfeed-format-column
                       (number-to-string
                        (elfeed-score-scoring-get-score-from-entry entry))
                       10 :left))
         (authors-width 135)
         (authors-column (elfeed-format-column
                          entry-authors
                          (elfeed-clamp elfeed-search-title-min-width authors-width 10
					:left)))
	 (insert (propertize date 'face 'elfeed-search-date-face) " ")
	 (insert (propertize title-column 'face title-faces 'kbd-help title) " ")
	 (insert (propertize authors-column 'face 'elfeed-search-date-face 'kbd-help entry-authors) " ")
	 (insert entry-score " ")
	 (when entry-authors (insert (propertize feed-title 'face 'elfeed-search-feed-face) " "))
	 (when tags (insert "(" tags-str ")"))
	 )
    )
  )
(setq elfeed-search-print-entry-function #'my-search-print-fn)
(run-at-time nil (* 8 60 60) #'elfeed-update)
(use-package elfeed-org
  :config
  (setq rmh-elfeed-org-files (list (concat no-littering-var-directory "elfeed.org")))
  (elfeed-org)
  )
(use-package elfeed-score
  :after elfeed
  :config
  (elfeed-score-load-score-file (concat no-littering-var-directory "elfeed.score"))
  (elfeed-score-enable)
  (define-key elfeed-search-mode-map "=" elfeed-score-map))

```

![elfeed](elfeed.png)