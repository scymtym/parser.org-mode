;;; grammar.lisp --- Grammar for Emacs org-mode markup.
;;
;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This Program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This Program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses>.

;; Note: this grammar has been derived heuristically by looking at
;; existing org-mode documents. Neither the org-mode source nor any
;; kind of org-mode grammar (I think, there is none at the time of
;; writing which is 2013-01-03) has been consulted.
;;
;; The grammar is written in bottom-up manner:
;;
;; 1. Markup
;; 2. Paragraphs
;; 3. Figures
;; 4. Tables
;; 5. Headings
;; 6. Global options
;; 7. Org-mode document (usual entry point)

(cl:in-package #:parser.org-mode)


;;; Markup
;;

(defrule comment
    (and #\# (* (not #\Newline)) #\Newline)
  (:constant nil))

(macrolet
    ((define-markup ((name character
		      &key
		      (end-character character)))
       (let ((rule-name (symbolicate '#:markup/ name)))
	 `(defrule ,rule-name
	      (and ,character text-chunk ,end-character)
	    (:destructure (c1 text c2)
	      (declare (ignore c1 c2))
	      (make-node *builder* :span
			 :kind    ,(make-keyword name)
			 :content text))))))
  (define-markup (italic   #\/))
  (define-markup (bold     #\*))
  (define-markup (verbatim #\~))
  (define-markup (code     #\=)))

(defrule markup
    (or text-chunk markup/italic markup/bold markup/code))

(defrule whitespace
    (+ (or #\Space #\Tab))
  (:lambda (list)
    (length list)))

(defrule alphanumeric ;;; TODO(jmoringe, 2011-12-18): rename
    (or (alphanumericp character) #\- #\_ #\. #\@ #\( #\) #\:))

(defrule text
    (+ (or alphanumeric whitespace))
  (:lambda (list)
    (string-trim
     #(#\Space)
     (apply #'text (substitute-if " " #'numberp list)))))


;;; Paragraphs
;;

(defrule text-chunk
    text
  (:lambda (list)
    (make-node *builder* :text :content (text list))))

(defrule paragraph
    (+ (or markup #\Newline))
  (:lambda (list)
    (reduce (curry #'add-child *builder*) (remove #\Newline list)
	    :initial-value (make-node *builder* :paragraph))))

(defrule listing
    (and macro (+ alphanumeric) #\Newline macro)
  (:destructure (head content newline foot)
    (declare (ignore head newline foot))
    (make-node *builder* :listing :content (text content))))

(declaim (special *enumeration-level*))

(defvar *enumeration-level* nil
  "TODO(jmoringe): document")

(defrule bullet-list
    (+ bullet-list-item)
  (:lambda (items)
    (reduce (curry #'add-child *builder*) items
	    :initial-value (make-node *builder* :bullet-list)))
  (:around ()
    (let ((*enumeration-level* (or *enumeration-level*
				   #+no (attribute (first items) :level))))
      (call-transform))))

(defrule bullet-list-item
    (and (? whitespace) (or #\+ #\-) whitespace paragraph #+no #\Newline)
  (:destructure (whitespace1 bullet-symbol whitespace2 text #+no newline)
    (declare (ignore bullet-symbol whitespace2 #+no newline))
    (let ((item (make-node *builder* :bullet-list-item
			   :level (or whitespace1 0))))
      (add-child *builder* item text)
      item)))

(defrule definition-list-item
    (and whitespace (or #\+ #\- #\*) whitespace (+ alphanumeric)
	 whitespace (and #\: #\:) whitespace (+ alphanumeric))
  (:destructure (whitespace1 bullet-symbol whitespace2 key whitespace3 colons whitespace4 value)
    (declare (ignore bullet-symbol whitespace2 whitespace3 colons whitespace4))
    (reduce (curry #'add-child *builder*)
	    (list (make-node *builder* :term :content (text key))
		  (make-node *builder* :text :content (text value)))
	    :initial-value (make-node *builder* :definition-list-item
				      :level (or whitespace1 0)))))



;;; Figures
;;

(defrule figure/float
    (and (? (or caption label)) link)
  (:destructure (caption link)
		(let ((figure (make-figure *builder*)))
		  (add-child *builder* figure caption))))


;;; Tables
;;

(defrule table-cell
    (and text #\|)
  (:destructure (text pipe)
    (declare (ignore pipe))
    (make-node *builder* :table-cell :content text)))

(defrule table-hrule
    (and (+ #\-) (? #\+))
  (:constant nil))

(defrule table-line/hrule
    (and #\| (+ table-hrule) #\| #\Newline)
  (:destructure (pipe1 segments pipe2 newline)
    (declare (ignore pipe1 pipe2 newline))
    (list (length segments) (make-node *builder* :table-hrule))))

(defrule table-line
    (and #\| (+ table-cell) #\Newline)
  (:destructure (pipe cells newline)
    (declare (ignore pipe newline))
    (list
     (length cells)
     (reduce (curry #'add-child *builder*) cells
	     :initial-value (make-node *builder* :table-row)))))

(defrule table
    (+ (or table-line/hrule table-line))
  (:lambda (lines)
    (let ((num-columns nil))
      ;; Determine number of columns and check consistency.
      (dolist (line lines)
	(let ((length (first line)))
	  (cond
	    ((null num-columns)
	     (setf num-columns length))
	    ((/= num-columns length)
	     (error "~@<Inconsistent number of columns in table: ~D vs. ~D~@:>"
		    num-columns length)))))
      ;; Build table nodes.
      (reduce (curry #'add-child *builder*)
	      lines
	      :initial-value (make-node *builder* :table
					:num-columns num-columns)
	      :key           #'second))))

(defrule table/float
    (and (? (or caption label)) table)
  (:destructure (caption table)
		table))


;;; Headings
;;

(defrule heading-stars
    (+ #\*)
  (:function length))

(defrule heading-title
    (+ (not (or #\Newline #\:)))
  (:text t))

(defrule tag
    (and #\: (+ (not (or #\Newline whitespace #\:))))
  (:destructure (colon value)
    (declare (ignore colon))
    (make-keyword (string-upcase (esrap:text value)))))

(defrule tags
    (and (+ tag) #\:)
  (:destructure (tags colon)
     (declare (ignore colon))
     tags))

(defrule property
    (and (? whitespace) #\: (+ (not (or #\Newline whitespace #\:))) #\:
	 (? whitespace) text #\Newline)
  (:destructure (whitespace1 colon1 key colon2 whitespace2 value newline)
    (declare (ignore whitespace1 colon1 colon2 whitespace2 newline))
    (cons (make-keyword (substitute #\- #\_ (string-upcase (text key))))
	  (text value))))

(defrule properties
    (and (and (? whitespace) ":PROPERTIES:" #\Newline)
	 (? whitespace) (* property)
	 (and (? whitespace) ":END:" #\Newline))
  (:function third))

(defrule heading-element
    (or paragraph bullet-list include))

(defrule heading
    (and heading-stars #\Space heading-title
	 (? whitespace) (? tags) ;;; TODO(jmoringe, 2011-12-18): does not work
	 #\Newline
	 (? properties)
	 (* heading-element))
  (:destructure (level space title ws1 tags newline properties paragraphs)
    (declare (ignore space ws1 newline))
    (reduce (curry #'add-child *builder*) paragraphs
	    :initial-value (apply #'make-node *builder* :heading
				  :level      level
				  :title      title
				  :properties (alist-plist
					       (remove :custom-id properties :key #'car))
				  :tags       tags
				  (when-let ((id (cdr (assoc :custom-id properties))))
				    (list :id id))))))

(defrule macro
    (and (and #\# #\+) (+ alphanumeric) #\: #\Newline)
  (:destructure (hash-plus name colon newline)
    (declare (ignore hash-plus colon newline))
    (text name)))


;;; Global options
;;

(defmacro define-option ((name) &body body)
  (let ((key (string name)))
   `(defrule ,name
	(and (and #\# #\+) ,key #\: (? whitespace) (+ (not #\Newline)) #\Newline)
      (:destructure (hash-plus key colon whitespace value newline)
        (declare (ignore hash-plus colon whitespace newline)
		 (ignorable key value))
	,@(or body `((make-node *builder* :option
				:name  ,(make-keyword name)
				:value (text value))))))))

(define-option (title))

(define-option (author))

(define-option (email))

(define-option (include))

(define-option (caption))
(define-option (label))

(define-option (options))

(defrule global-option
    (or title author email include options))

(defmacro define-block ((name &key (body-var 'content)) args &body body)
  (let ((name  (symbolicate "BLOCK/" name))
	(begin (format nil "#+BEGIN_~A:" name))
	(end   (format nil "#+END_~A:" name)))
    `(defrule ,name
	 (and (and ,begin) (? whitespace) ,@(mapcar #'second args) #\Newline
	      (* (not (or ,end (and #\Newline ,end)))) #\Newline
	      (and ,end #\Newline))
       (:destructure (begin ws1 ,@(mapcar #'first args) newline1
		      ,body-var newline2 end)
         (declare (ignore begin ws1 newline1 newline2 end)
		  (ignorable ,body-var))
	 ,@body))))

(define-block (source :body-var body) ((language (+ (alphanumericp character))))
  (add-child *builder*
	     (make-node *builder* :literal-block
			:language (make-keyword (string-upcase (text language))))
	     (text body)))


;;; Main entry point
;;

(defrule org
    (+ (or (and (? whitespace) #\Newline)
	   comment
	   global-option heading))
  (:lambda (elements)
    ;; A builder can return nothing to indicate that no node should be
    ;; created for a certain part of the input.
    (reduce (curry #'add-child *builder*)
	    (remove-if (of-type '(or null (cons (or null integer) (cons string))))
		       (print elements))
	    :initial-value (make-node *builder* :document))))
