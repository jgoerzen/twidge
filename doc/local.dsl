<!DOCTYPE style-sheet PUBLIC "-//James Clark//DTD DSSSL Style Sheet//EN" [
<!ENTITY dbstyle PUBLIC "-//GtkViaAlcove//DOCUMENT Gtk-doc HTML Stylesheet//EN" CDATA DSSSL>
]>

<style-sheet>
<style-specification use="gtk">
<style-specification-body>

(define (toc-depth nd)
  (if (string=? (gi nd) (normalize "book"))
      1
      1))
(define %generate-article-toc% #t)

;; Don't split up the doc as much.
(define (chunk-element-list)
  (list (normalize "preface")
        (normalize "chapter")
        (normalize "appendix")
        (normalize "article")
        (normalize "glossary")
        (normalize "bibliography")
        (normalize "index")
        (normalize "colophon")
        (normalize "setindex")
        (normalize "reference")
        (normalize "refentry")
        (normalize "part")
        (normalize "book") ;; just in case nothing else matches...
        (normalize "set")  ;; sets are definitely chunks...
        ))


</style-specification-body>
</style-specification>
<external-specification id="gtk" document="dbstyle">
</style-sheet>

<!--
# arch-tag: web stylesheet for documentation
-->
