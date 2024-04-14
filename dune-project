(lang dune 3.14)

(name bitgenerators)

(generate_opam_files true)

(source
 (github zoj613/bitgenerators))

(authors "Zolisa Bleki")

(maintainers "Zolisa Bleki")

(license BSD-3-Clause)

(documentation https://zoj613.github.io/bitgenerators/)

(package
 (name bitgenerators)
 (synopsis "PRNG bitgenerators for OCaml users")
 (description "PRNG bitgenerators for OCaml users")
 (depends ocaml dune ctypes stdint)
 (tags
  ("scientific computing" "psuedo-random numbers")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
