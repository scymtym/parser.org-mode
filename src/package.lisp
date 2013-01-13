;;; package.lisp --- Package definition for the parser.org-mode system.
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

(cl:defpackage #:parser.org-mode
  (:use
   #:cl
   #:alexandria
   #:let-plus)

  (:import-from #:esrap
   #:defrule
   #:?
   #:text
   #:call-transform)

  ;; Variables
  (:export
   #:*builder*)

  ;; Parser Protocol
  (:export
   #:parse)

  ;; Builder Protocol
  (:export
   #:make-node
   #:add-child)

  ;; Rules
  (:export
   )

  (:documentation
   "This package contains a parser for the Emacs org-mode markup
syntax.

For more details see http://orgmode.org/"))
