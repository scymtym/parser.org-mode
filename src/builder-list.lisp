;;; builder-list.lisp ---
;;
;; Copyright (C) 2013 Jan Moringen
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

(cl:in-package #:parser.org-mode)

(defmacro define-list-builder-method (kind-and-options args &body body)
  "TODO(jmoringe): document"
  (let+ (((kind &rest options) (ensure-list kind-and-options))
	 ((&plist-r/o (args-var :args-var (gensym "ARGS"))
		      (kind-var :kind-var (gensym "KIND"))) options))
   `(defmethod make-node ((builder   (eql 'list))
			  (,kind-var ,(if (eq kind t) t `(eql ,kind)))
			  &rest ,args-var
			  &key
			  ,@(mapcar #'first args))
      (declare (ignorable ,args-var))
      ,@body)))

(define-list-builder-method (t :kind-var kind :args-var args) ()
  (list* kind nil args))

(define-list-builder-method :properties ((properties list))
  (list* :properties properties))

(define-list-builder-method :text ((content string))
  content)

(defmethod add-child ((builder (eql 'list)) (parent list) (child t))
  (appendf (second parent) (list child))
  parent)
