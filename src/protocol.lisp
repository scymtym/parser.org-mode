;;; protocol.lisp --- Protocol provided by the parser.org-mode system.
;;
;; Copyright (C) 2012, 2013 Jan Moringen
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


;;;
;;

(defun parse (source builder
	      &key
	      (rule         'org)
	      (junk-allowed nil))
  "TODO(jmoringe): document"
  (let ((*builder* builder))
    (values (esrap:parse rule source :junk-allowed junk-allowed))))


;;; Builder protocol
;;

(defgeneric make-node (builder kind
		       &rest args
		       &key &allow-other-keys)
  (:documentation
   "TODO(jmoringe): document"))

(defgeneric add-child (builder parent child)
  (:documentation
   "TODO(jmoringe): document"))
