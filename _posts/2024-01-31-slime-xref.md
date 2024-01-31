---
layout: post
title: Tweaking SLIME Xref for Remote Images
category: Lisp
---
By the nature of embedded development one spends a lot of time debugging on target devices. SLIME experience for the most part is as smooth as on local host with the exception of cross referencing. Swank backend on target is reporting local paths in xref records which the frontend on your host then tries to open.

The canonical workaround from the [user manual](https://slime.common-lisp.dev/doc/slime.pdf) is using `slime-tramp` contrib, allowing you to navigate the source tree on remote target over SSH. I however greatly prefer to work on the local copy of the source code, with much lower latency (SSH over VPN over cellular to a remote site is no fun) and ability to stage and commit changes immediately. A somehwoat kludgy workflow that does the trick is using a hacked `slime-postprocess-xref` below to substitute `$HOME` in xref records. The source tree in remote home has to be placed in the same relative location as on your host. One should also remember copying their local tree to remote at the end of debugging session, in case the instance has to be restarted.

{% highlight lisp linenos %}
;;; A modified version that substitutes HOME in remote images with local ones
;;; enabling local code navigation with the usual SLIME xref tools
(defun slime-postprocess-xref (original-xref)
  "Process (for normalization purposes) an Xref comming directly
from SWANK before the rest of Slime sees it. In particular,
convert ETAGS based xrefs to actual file+position based
locations."
  (if (not (slime-xref-has-location-p original-xref))
      (list original-xref)
    (let ((loc (slime-xref.location original-xref)))
      (slime-dcase (slime-location.buffer loc)
        ((:etags-file tags-file)
         (slime-dcase (slime-location.position loc)
           ((:tag &rest tags)
            (visit-tags-table tags-file)
            (mapcar (lambda (xref)
                      (let ((old-dspec (slime-xref.dspec original-xref))
                            (new-dspec (slime-xref.dspec xref)))
                        (setf (slime-xref.dspec xref)
                              (format "%s: %s" old-dspec new-dspec))
                        xref))
                    (cl-mapcan #'slime-etags-definitions tags)))))
        (t
         ;; Find the /home/someuser/ path component and replace it
         ;; with our local $HOME, if any
         (setf (cadr (slime-location.buffer loc))
               (replace-regexp-in-string "^/[^/]+/[^/]+"
                                         (getenv "HOME")
                                         (cadr (slime-location.buffer loc))))
         (list original-xref))))))
{% endhighlight %}
