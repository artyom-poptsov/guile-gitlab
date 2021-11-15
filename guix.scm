;; guix.scm --- GNU Guix package recipe    -*- coding: utf-8 -*-
;;
;; Copyright (C) 2021 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;
;; Author: Artyom V. Poptsov <poptsov.artyom@gmail.com>
;; Created: 15 November 2021
;;
;; This file is part of Guile-GitLab.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; The program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with the program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; GNU Guix development package. To use as the basis for a development
;; environment, run:
;;
;;  guix environment --pure --container -l guix.scm
;;
;; In the new shell, run:
;;
;;  autoreconf -vif && ./configure && make check
;;
;;; Code:

(use-modules (guix packages)
             (guix licenses)
             (guix git-download)
             (guix build-system gnu)
             (gnu packages autotools)
             (gnu packages guile)
             (gnu packages tls)
             (gnu packages guile-xyz)
             (gnu packages pkg-config)
             (gnu packages texinfo))

(package
  (name "guile-gitlab")
  (version "0.1.0")
  (source (string-append "./" name "-" version ".tar.gz"))
  (build-system gnu-build-system)
  (native-inputs
   `(("autoconf" ,autoconf)
     ("automake" ,automake)
     ("pkg-config" ,pkg-config)
     ("texinfo" ,texinfo)))
  (inputs `(("guile"        ,guile-2.2)
            ("guile-json"   ,guile-json)
            ("guile-gnutls" ,guile2.2-gnutls)))
  (propagated-inputs `(("guile-lib" ,guile2.2-lib)))
  (arguments
   '(#:phases (modify-phases %standard-phases
                (add-before 'configure 'set-guilesitedir
                  (lambda _
                    (substitute* "Makefile.in"
                      (("^guilesitedir =.*$")
                       "guilesitedir = \
$(datadir)/guile/site/$(GUILE_EFFECTIVE_VERSION)\n"))
                    (substitute* "modules/Makefile.in"
                      (("^guilesitedir =.*$")
                       "guilesitedir = \
$(datadir)/guile/site/$(GUILE_EFFECTIVE_VERSION)\n"))
                    (substitute* "modules/gitlab/Makefile.in"
                      (("^guilesitedir =.*$")
                       "guilesitedir = \
$(datadir)/guile/site/$(GUILE_EFFECTIVE_VERSION)\n"))
                    (substitute* "modules/gitlab/api/Makefile.in"
                                 (("^guilesitedir =.*$")
                                  "guilesitedir = \
$(datadir)/guile/site/$(GUILE_EFFECTIVE_VERSION)\n"))
                    (substitute* "modules/gitlab/cli/Makefile.in"
                                 (("^guilesitedir =.*$")
                                  "guilesitedir = \
$(datadir)/guile/site/$(GUILE_EFFECTIVE_VERSION)\n"))
                    #t))
                (add-after 'unpack 'autoreconf
                  (lambda _
                    (zero? (system* "autoreconf" "-vfi")))))))
  (home-page "https://github.com/artyom-poptsov/guile-gitlab")
  (synopsis "GNU Guile interface to GitLab CE REST API.")
  (description
   "GNU Guile interface to GitLab CE REST API.")
  (license gpl3))

;;; guix.scm ends here.
