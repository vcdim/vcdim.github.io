+++
title = 'Fresh Start Emacs on macOS - E02'
date = 2024-06-02T10:50:01-07:00
toc = true
+++

## Setup Version Control

In order to keep track of the emacs configuration, it is usually a good idea to have a version control so that you can revert to old stable version when your emacs broke or you can jump start to have the same configuration on other (new) environment.

I use Github as my remote repository. To create a repo, **log into Github** and click on the **+** icon.
![Create Github Repo](create-github-repo.png)
A memu will pop up and click **New repository**. In the Create a new repository page, I prefer use the name `.emacs.d` so that in the future the `git clone` command and be as simple as possible.
![Create New Repo](create-new-repo.png)

Also feel free to add description or README.md as you like. When you are decided, hit the **Create Repository** button.

Next step is to create a git repo in the `~/.emacs.d` folder on your local machine. To do so, you may
```sh
cd ~/.emacs.d/
git init
```
It's better to add a `.gitignore` file to avoid submitting too many temporary files. In my first commit, I have

```git
*~
elpa/archives/
```

in this file. Now you can commit the change in the local repository (by VSCode is easy, but sure you can do it using CLI). Next, I need to set the local repository pointing to the public repository just setup by

```sh
git remote add origin https://github.com/vcdim/.emacs.d.git
```

And finally push it to remote (again, by VSCode / CLI).

Now, to test our workflow, first kill emacs completely by `M-x kill-emacs` and delete the `~/.emacs.d` folder and clone the repo from Github by

```sh
rm -rf ~/.emacs.d
git clone https://github.com/vcdim/.emacs.d.git
```
