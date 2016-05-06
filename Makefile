program: delete
	ocamllex miniooLEX.mll
	ocamlyacc miniooYACC.mly
	ocamlc -c miniooYACC.mli
	ocamlc -c miniooLEX.ml
	ocamlc -c miniooYACC.ml
	ocamlc -c minioo.ml
	ocamlc -o minioo miniooLEX.cmo miniooYACC.cmo minioo.cmo
delete:
	/bin/rm -f minioo minioo.cmi minioo.cmo miniooLEX.cmi miniooLEX.cmo miniooLEX.ml miniooYACC.cmi miniooYACC.cmo miniooYACC.ml miniooYACC.mli makefile~
