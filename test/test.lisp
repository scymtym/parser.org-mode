(cl:in-package #:parser.org-mode.test)

(parse "bla /blu foo bar/ bli" 'list 'paragraph)

(parse 'bullet-list "+ Bla
  + Bli
")

(parse "| bla | bli |
| first | second   |
|-------+----------|
| bla   | foobar |
"
       :docutils :rule 'table)

(parse "   + bla :: blup"
       'list 'definition-list-item)

(parse "*** Hello World :bla:
     :PROPERTIES:
     :FOO:  bar
     :CUSTOM_ID: bla
     :END:
bla /bla/
+ Bli
+ Blu
"
       :docutils :rule 'heading)

(parse 'heading "* bla
  bla"
       :junk-allowed t)

(parse "#+BEGIN_SOURCE: lisp
bla
#+END_SOURCE:
"
       'list 'block/source)

(parse 'macro "#+SOURCE:
")

(parse ":PROPERTIES:
:FOO:   bar bla    bla
:END:
"
       'list 'properties)

(parse ":FOO:   bar bla    bla
" 'list 'property)

(parse "  bla    bla  " 'list 'text)

(parse "#+EMAIL: bla
" 'list :rule 'email)

(parse ":a:b:" 'list :rule 'tags)


(let ((*document* (make-node 'document
			     :id   "TODO"
			     :name "TODO")))
  (parse 'org (read-file-into-string "/tmp/bla.org"))
  (with-output-to-file (stream "~/pub/out/bla.html"
			       :if-exists :supersede)
    (write-html stream *document*)))

(let ((*document* (make-node 'document
			     :id   "TODO"
			     :name "TODO")))
  (parse (read-file-into-string "/homes/jmoringe/code/cor-lab/rsb/rsb-cl/README")
	 'list :junk-allowed t)
  (with-output-to-file (stream "~/pub/out/bla.html"
			       :if-exists :supersede)
    (write-html stream *document*)))
