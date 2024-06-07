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
