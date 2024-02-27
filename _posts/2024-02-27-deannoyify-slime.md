---
layout: post
title: Deannoyifying SLIME
category: Lisp
---
Over all these years I don't think I ever wanted to close all existing SLIME connections when attaching to a remote host. Similarly, the version mismatch between SWANK and SLIME frontend has never stopped me following through with connection. I did however fumbled with y/n confirmations plenty of times. The snippet below removes these interactive checks.

{% highlight lisp linenos %}
(defun slime-check-version (version conn)
  (or (equal version slime-protocol-version)
      (equal slime-protocol-version 'ignore)
      (message
       (format "Versions differ: %s (slime) vs. %s (swank)."
               slime-protocol-version version))
      (slime-net-close conn)
      (top-level)))

(defun slime-connect (host port &optional _coding-system interactive-p &rest parameters)
  "Connect to a running Swank server. Return the connection."
  (interactive (list (read-from-minibuffer
                      "Host: " (cl-first slime-connect-host-history)
                      nil nil '(slime-connect-host-history . 1))
                     (string-to-number
                      (read-from-minibuffer
                       "Port: " (cl-first slime-connect-port-history)
                       nil nil '(slime-connect-port-history . 1)))
                     nil t))
  (slime-setup)
  (message "Connecting to Swank on port %S.." port)
  (slime-setup-connection (apply 'slime-net-connect host port parameters)))
{% endhighlight %}

A useful feature for working on *nix backends is an ability to parse integers as epoch time in SLIME inspector. For this we need to minimally extend `emacs-inspect` found in `swank-fancy-inspector.lisp`:

{% highlight lisp linenos %}
(defmethod emacs-inspect ((i integer))
  (append
   `(,(format nil "Value: ~D = #x~8,'0X = #o~O = #b~,,' ,8:B~@[ = ~E~]"
	      i i i i (ignore-errors (coerce i 'float)))
     (:newline))
   (when (< -1 i char-code-limit)
     (label-value-line "Code-char" (code-char i)))
   (label-value-line "Integer-length" (integer-length i))
   (ignore-errors
     (label-value-line "Universal-time" (format-iso8601-time i t)))
   (ignore-errors
     (label-value-line "Epoch-time"
		       (format-iso8601-time (+ i 2208988800) t)))))
{% endhighlight %}
