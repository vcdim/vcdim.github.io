+++
title = 'Fresh Start Emacs on macOS - E03'
date = 2024-06-03T12:00:01-07:00
toc = true
tags = ['emacs']
+++

## Fix Chinese Fonts Not Remembered

In last post, we tweaked on the Chinese Fonts. However, if you restart your computer and run emacs(client), **it will look like the fonts are working at all!t** After some digging, I was inspired by this [post](https://emacs.stackexchange.com/questions/16464/emacs-server-init-when-called-without-file), where I found the reason behind is that when a Emacs server is running, setting face attribute and setting fonts are not meaningful. Hence, at first, I followed this post by wrapping the 5-line font-setting with a function, added another wrapper function, and added the function to the hook `after-make-frame-functions`. I also run into this [post](https://www.gnu.org/software/emacs/manual/html_node/emacs/Saving-Emacs-Sessions.html), where, I found (`server-after-make-frame-hook`) is probably a more appropriate choice. The resulting code snippet looks like this

```elisp
(defun my/set-font ()
  (let ((default-font (font-spec :name "Iosevka" :size 15))
	(cn-font (font-spec :name "Sarasa Mono SC")))
    (set-face-attribute 'default nil :font default-font)
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font t charset cn-font)))
  )

(defun my/frame-behaviors (&optional frame)
  (with-selected-frame (or frame (selected-frame))
    (my/set-font)
    ))

;; for server
(add-hook 'server-after-make-frame-hook 'my/frame-behaviors)

;; for normal start
(my/frame-behaviors)
```

## About Saving Desktop

When I kill an `emacsclient` frame and open another, the opened buffers will be remembered. However, if I turn off computer, and open emacs again, I need to start fresh to open everything.

It's tempting to enable [desktop-save-mode](https://www.gnu.org/software/emacs/manual/html_node/emacs/Saving-Emacs-Sessions.html#Saving-Emacs-Sessions). In fact I tried two hours try to get it working. However, it is not good to use. --- It will not save the desktop when I kill the server. It can only load the desktop once per server start.

There is also an option have have [saveplace package](https://www.emacswiki.org/emacs/SavePlace).