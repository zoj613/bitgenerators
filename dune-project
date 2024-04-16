(lang dune 3.14)

(name bitgenerators)

(version v0.1.0)

(generate_opam_files true)

(source
 (github zoj613/bitgenerators))

(authors "Zolisa Bleki")

(maintainers "Zolisa Bleki")

(license BSD-3-Clause)

(documentation https://zoj613.github.io/bitgenerators/bitgenerators/Bitgen/index.html)

(package
 (name bitgenerators)
 (synopsis "PRNG bitgenerators for OCaml users")
 (description "A purely functional port of numpy's random bitgenerator interface for OCaml users")
 (depends
   (ocaml (>= 4.14.0))
   dune
   ctypes
   stdint
   (odoc :with-doc)
   (bisect_ppx
     (and :dev (>= 2.5.0) :with-test))
   (ounit2 :with-test)
   (csv :with-test))
 (tags
  ("scientific computing" "psuedo-random numbers")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
