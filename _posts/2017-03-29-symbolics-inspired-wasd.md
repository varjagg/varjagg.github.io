---
layout: post
title: Symbolics-inspired WASD keyboard layout
category: Lisp
---

I'm enjoying my customized mechanical [WASD](http://www.wasdkeyboards.com/) keyboard since last summer:

![Symbolics tribute keyboard](/images/wasd/1.jpg)

Its signage and cap colors are inspired by early Lisp machine 'space cadet' keyboards. Not anywhere a close functonal match certainly but I do like the looks. And unlike the the original it comes in short, messy-desk-space-preserving 87-key version.

The inscriptions are changed to oldskool. Alt is META, AltGr is GREEK, caps is HYPER and left-Win is SUPER. SUPER is bound to X11 layout switch in my system, GREEK is just the normal European 2nd-level shift. HYPER is remapped to.. well actual Hyper. This adds a bunch of new keybinding space in Emacs:

{% highlight elisp %}
(when (eq window-system 'x)
  (shell-command "xmodmap -e 'remove Mod4 = Hyper_L' -e 'add Mod3 = Hyper_L'")
  (shell-command "xmodmap -e 'clear Lock' -e 'keycode 66 = Hyper_L'"))

(global-set-key (kbd "H-k") 'kill-this-buffer)
(global-set-key (kbd "H-f") 'find-file)
(global-set-key (kbd "H-h") 'hyperspec-lookup)
(global-set-key (kbd "H-3") 'split-window-right)
(global-set-key (kbd "H-2") 'split-window-below)
(global-set-key (kbd "H-1") 'delete-other-windows)
(global-set-key (kbd "H-0") 'delete-window)
(global-set-key (kbd "H-s") 'save-buffer)
(global-set-key (kbd "H-a") 'mark-whole-buffer)
(global-set-key (kbd "H-r") 'raise-sexp)
(global-set-key (kbd "H-c") 'slime-selector)
{% endhighlight %}

Norwegian-based layout is [available here](https://github.com/varjagg/wasd-symbolics-tribute).
