;;; parser.org-mode.asd --- System definition for parser.org-mode system.
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

(cl:defpackage #:parser.org-mode-system
  (:use
   #:cl
   #:asdf)

  (:export
   #:version/list
   #:version/string))

(cl:in-package #:parser.org-mode-system)


;;; Version stuff
;;

(defparameter +version-major+ 0
  "Major component of version number.")

(defparameter +version-minor+ 2
  "Minor component of version number.")

(defparameter +version-revision+ 0
  "Revision component of version number.")

(defun version/list ()
  "Return a version of the form (MAJOR MINOR REVISION)."
  (list +version-major+ +version-minor+ +version-revision+))

(defun version/string ()
  "Return a version string of the form \"MAJOR.MINOR.REVISION\"."
  (format nil "~{~A.~A.~A~}" (version/list)))


;;; System definition
;;

(defsystem :parser.org-mode
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LLGPLv3"
  :description "Parser for org-mode syntax."
  :depends-on  (:alexandria
		:let-plus
		:esrap)
  :components  ((:module     "src"
		 :serial     t
		 :components ((:file       "package")
			      (:file       "variables")
			      (:file       "protocol")
			      (:file       "grammar")
			      (:file       "builder-list"))))
  :in-order-to (test-op (test-op :parser.org-mode-test)))

(defsystem :parser.org-mode-test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LLGPLv3"
  :description "Unit tests for the parser.org-mode system."
  :depends-on  (:lift

		:parser.org-mode)
  :components  ((:module     "test"
		 :serial     t
		 :components ((:file       "package")))))

(defmethod perform ((operation test-op)
		    (component (eql (find-system :parser.org-mode.test))))

  (funcall (find-symbol "RUN-TESTS" :lift) :config :generic))
