+++
title = 'Fresh Start Emacs on macOS E01'
date = 2024-06-01T09:17:15-07:00
toc = true
+++

My emacs was installed by [emacs-plus](https://github.com/d12frosted/homebrew-emacs-plus) via [brew](https://brew.sh/). It is of version 29.1 (The latest release is 29.3 at the time of writing.)

## Uninstall

There are several things to uninstall. First, the emacs binary is stored at `/opt/homebrew/bin/emacs`. To do so, we run

```sh
brew uninstall emacs-plus
```

Looking at the log, the package is actually stored at `/opt/homebrew/Cellar/emacs-plus@29/29.1`.

## Backup

Of course we have `.emacs.d` folder under `~`. What I wanna do is backup the folder to `.emacs.d.old` by

```sh
mv ~/.emacs.d ~/.emacs.d.old
```

for future reference.

## Things I didn't uninstall

There are two Automation Apps in Application folder. The `Emacs.app` is basically the code below

```sh
/opt/homebrew/opt/emacs-plus@29/bin/emacsclient 
    -c -n -q -u \
    -e "(select-frame-set-input-focus (selected-frame))" \
    -s work
```

From [emacsclient options](https://www.gnu.org/software/emacs/manual/html_node/emacs/emacsclient-Options.html), we know that

- `-c` is `--create-frame`, which creates a new graphical client frame.
- `-n` is `--no-wait`. This let `emacsclient` exit immediately.
- `-q` is `--quiet`, which does not let `emacsclient` to display messages.
- `-u` is `--suppress-output`, which dodes not let `emacsclient` display results from the server.
- `-e` is `--eval`, which runs some Emacs Lisp code. The code snippet `(select-frame-set-input-focus (selected-frame))` is for *get focus*.
- `-s server-name` is `--socket-name=server-name`

The `Capture.app` is basically

```sh
/opt/homebrew/opt/emacs-plus@29/bin/emacsclient \
  -c -n \
  -F '(quote (name . "capture"))' \
  -e "(select-frame-set-input-focus (selected-frame))" "(my/org-capture)" \
  -s work
```

Here

- `-F alist` is `--frame-parameters=alist` which sets the parameters for a newly created graphical frame.

## Reinstall

To reinstall emacs, simply

```sh
brew install emacs-plus --with-ctags --with-dbus --with-debug \
 --with-mailutils --with-no-frame-refocus --with-xwidgets \
 --with-imagemagick --with-poll --with-modern-black-variant-icon
```
The options are explained [here](https://github.com/d12frosted/homebrew-emacs-plus?tab=readme-ov-file#options-1).

- ctags: is a programming tool that generates an index file of names found in source and header files.
- dbus: is an inter-process communication mechanism for appliaitons residing on the same host. It is short for Desktop Bus.

Once the installation complete, you should see message like below:

```sh
Emacs.app was installed to:
  /opt/homebrew/opt/emacs-plus@29

To link the application to default Homebrew App location:
  osascript -e 'tell application "Finder" to make alias file to posix file "/opt/homebrew/opt/emacs-plus@29/Emacs.app" at POSIX file "/Applications" with properties {name:"Emacs.app"}'

Your PATH value was injected into Emacs.app/Contents/Info.plist

Report any issues to https://github.com/d12frosted/homebrew-emacs-plus

To start d12frosted/emacs-plus/emacs-plus@29 now and restart at login:
  brew services start d12frosted/emacs-plus/emacs-plus@29
Or, if you don't want/need a background service you can just run:
  /opt/homebrew/opt/emacs-plus@29/bin/emacs --fg-daemon
```

!!! note
    With `--with-native-comp` flag will result in annoying libgccgit warning and it's claimed that this is still [experimental](https://github.com/d12frosted/homebrew-emacs-plus?tab=readme-ov-file#gccemacs), hence, we didn't include this flag in installation.

## Settings

### Init File Management

The very first step I'd recommend is to use org file (`init.org`) to `init.el` file.

To do so, first create the `init.org` file at `~/.emacs.d/` folder with the following content:

```org
#+TITLE: Emacs Config
#+PROPERTY: header-args:elisp :tangle ./init.el

* Init File Management

#+begin_src elisp
(defun my/org-babel-tangle-config()
  (when (string-equal (buffer-file-name) (expand-file-name "init.org" user-emacs-directory))
    (let ((org-confirm-babel-evaluate nil)) (org-babel-tangle))))
(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook 'my/org-babel-tangle-config)))
#+end_src
```

Evaluate the elisp cell by `C-c C-c` and reload org-mode by `M-x org-mode`. On saving the file, you should see the message saying

```text
Tangled 1 code block from init.org
```

and the `init.el` should be generated with the same elisp code extracted.

### Packages

[melpa](https://melpa.org/#/) has 5,750 packages for emacs at the time of writing. The default package archive contains only `gnu` and `nongnu`, which is far from enough. Another useful package is called [`use-package`](https://github.com/jwiegley/use-package), whose primary purpose is for the configuration and loading of packages. It is already included in emacs 29. To setup the melpa package archive and load use-package, add the following elisp code:

```elisp
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(require 'use-package)
(setq use-package-always-ensure t)
```

### Basic Settings

There are some easy-to-use basic settings that I like to use below:

```elisp
(pixel-scroll-precision-mode)
(global-set-key (kbd "<home>") 'beginning-of-line)
(global-set-key (kbd "<end>") 'end-of-line)
(global-unset-key (kbd "C-z"))
```

### Org (Basic)

There are a lot of settings that belongs to org-mode. To start with, we enables some essential ones in this post.

```elisp
(setq org-support-shift-select t)
(setq org-src-preserve-indentation t)

(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("el" . "src elisp"))
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
```