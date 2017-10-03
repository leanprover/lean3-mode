This is the Emacs mode for the [Lean theorem prover][lean].

[lean]: https://github.com/leanprover/lean

Installation
============

`lean-mode` requires GNU Emacs 24.3. The recommended way to install it is via [MELPA](https://melpa.org). If you have not already configured MELPA, put the following code in your Emacs init file (typically `~/.emacs.d/init.el`):
```elisp
(require 'package) ; You might already have this line
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize) ; You might already have this line
```
See also [MELPA: Getting Started](https://melpa.org/#/getting-started).

With MELPA configured, you can `M-x package-install` the packages `lean-mode`, `company-lean`, and `helm-lean`. The latter two packages give you auto completion and a searchable list of declarations, respectively, and are strongly recommended.

For `company-lean`, you should also bind a key to trigger completion, if you have not already done so:

```elisp
;; Trigger completion on Shift-Space
(global-set-key (kbd "S-SPC") #'company-complete)
```

Trying It Out
=============

If things are working correctly, you should see the word ``Lean`` in the
Emacs mode line when you open a file with extension `.lean`. If you type
```lean
#check id
```
the word ``#check`` will be underlined, and hovering over it will show
you the type of ``id``. The mode line will show ``FlyC:0/1``, indicating
that there are no errors and one piece of information displayed.

Key Bindings and Commands
=========================

| Key                | Function                                                                        |
|--------------------|---------------------------------------------------------------------------------|
| <kbd>M-.</kbd>     | jump to definition in source file (`lean-find-definition`)                      |
| <kbd>C-c C-k</kbd> | shows the keystroke needed to input the symbol under the cursor                 |
| <kbd>C-c C-x</kbd> | execute lean in stand-alone mode (`lean-std-exe`)                               |
| <kbd>C-c SPC</kbd> | run a command on the hole at point (`lean-hole`)                                |
| <kbd>C-c C-d</kbd> | show a searchable list of definitions (`helm-lean-definitions`)                 |
| <kbd>C-c C-g</kbd> | toggle showing current tactic proof goal (`lean-toggle-show-goal`)              |
| <kbd>C-c C-n</kbd> | toggle showing next error in dedicated buffer (`lean-toggle-next-error`)        |
| <kbd>C-c C-b</kbd> | toggle showing output in inline boxes (`lean-message-boxes-toggle`)             |
| <kbd>C-c C-r</kbd> | restart the lean server (`lean-server-restart`)                                 |
| <kbd>C-c ! n</kbd> | flycheck: go to next error                                                      |
| <kbd>C-c ! p</kbd> | flycheck: go to previous error                                                  |
| <kbd>C-c ! l</kbd> | flycheck: show list of errors                                                   |

In the default configuration, the Flycheck annotation `FlyC:n/n` indicates the
number of errors / responses from Lean; clicking on `FlyC` opens the Flycheck menu.


Message Boxes
================
To view the output of commands such as `check` and `print` in boxes in the buffer, enable the feature using <kbd>C-c C-b</kbd>.
If you then type
```lean
#check id
```
a box appears after the line showing the type of `id`. Customize `lean-message-boxes-enabled-captions` to choose categories of boxes.
In particular, add `"trace output"` to the list to see proof states and other traces in the buffer.

Known Issues and Possible Solutions
===================================

Unicode
-------

If you experience a problem rendering unicode symbols on emacs,
please download the following fonts and install them on your machine:

 - [Quivira.ttf](http://www.quivira-font.com/files/Quivira.ttf)
 - [Dejavu Fonts](http://sourceforge.net/projects/dejavu/files/dejavu/2.35/dejavu-fonts-ttf-2.35.tar.bz2)
 - [NotoSans](https://github.com/googlei18n/noto-fonts/blob/master/hinted/NotoSans-Regular.ttc?raw=true)
 - [NotoSansSymbols](https://github.com/googlei18n/noto-fonts/blob/master/unhinted/NotoSansSymbols-Regular.ttf?raw=true)

Then, have the following lines in your emacs setup to use `DejaVu Sans Mono` font:

```elisp
(when (member "DejaVu Sans Mono" (font-family-list))
  (set-face-attribute 'default nil :font "DejaVu Sans Mono-11"))
```

You may also need to install the [emacs-unicode-fonts](https://github.com/rolandwalker/unicode-fonts) package, after which you should add the following lines to your emacs setup:

```elisp
(require 'unicode-fonts)
(unicode-fonts-setup)
```

Contributions
=============

Contributions are welcome!
