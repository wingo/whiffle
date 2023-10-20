;;; Copyright © 2023 Andy Wingo
;;;
;;; Whippet is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation; either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; Whippet is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with Whippet.  If not, see
;;; <http://www.gnu.org/licenses/>.

;; This file defines a Guix package.  It can be used to spawn an
;; interactive development environment:
;;
;;   guix shell
;;
;; Or it can be used to build Guile from a checkout in an isolated
;; environment:
;;
;;   guix build -f guix.scm
;;
;; Likewise, you may cross-compile it:
;;
;;   guix build -f guix.scm --target=x86_64-w64-mingw32
;;
;; … or perform a native build for another architecture, assuming
;; either offloading or transparent QEMU emulation is set up:
;;
;;   guix build -f guix.scm -s riscv64-linux

(define-module (whiffle-package)
  #:use-module (guix)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bdw-gc)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages version-control))

(define-public whiffle
  (let ((vcs-file? (or (git-predicate
                        (string-append (current-source-directory)
                                       "/../.."))
                       (const #t))))
    (package
      (name "whiffle")
      (version "0.0.1-git")
      (source (local-file "../.." "whiffle-checkout"
                          #:recursive? #t
                          #:select? vcs-file?))
      (build-system gnu-build-system)
      (arguments
       (list #:phases
             #~(modify-phases %standard-phases
                 (delete 'configure))))
      (native-inputs
       (list guile-next))
      (propagated-inputs
       (list gcc-toolchain gnu-make libgc pkg-config))
      (outputs '("out"))
      (synopsis "Scheme implementation intended to test the Whippet garbage collector")
      (description
       "Whiffle is a very simple compile-to-C Scheme implementation, intended to be a
testbed for development of the Whippet garbage collector.")
      (home-page "https://github.com/wingo/whiffle/")
      (license license:lgpl3+))))

whiffle
