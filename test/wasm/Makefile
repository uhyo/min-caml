MINCAML = ../min-caml
RM = rm
WAT2WASM = wat2wasm
RUNWASM = ./runwasm.js

default: do_test

.PHONY: do_test clean

TESTS = print sum-tail gcd sum fib ack even-odd \
adder funcomp cls-rec cls-bug cls-bug2 cls-reg-bug \
shuffle spill spill2 spill3 join-stack join-stack2 join-stack3 \
join-reg join-reg2 non-tail-if non-tail-if2 \
inprod inprod-rec inprod-loop matmul matmul-flat \
manyargs

do_test: $(TESTS:%=%.cmp)

.PRECIOUS: %.wat %.wasm %.res %.ans %.cmp

%.wat: %.ml $(MINCAML)
	$(MINCAML) $*
%.wasm: %.wat libmincaml.js
	$(WAT2WASM) $< -o $@
%.res: %.wasm
	$(RUNWASM) $< > $@
%.ans: %.ml
	ocaml $< > $@
%.cmp: %.res %.ans
	diff $^ > $@


clean:
	$(RM) -f *.wat *.wasm *.res *.ans *.cmp
