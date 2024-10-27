# Updating dependencies:
# ELPA master branch (for hydra):
#   git clone git://git.savannah.gnu.org/emacs/elpa.git
# ELPA externals setup:
#   make depsetup
# ELPA externals update
#   make depupdate

.POSIX:
EMACS = emacs
EL = bluetooth-device.el bluetooth-lib.el bluetooth-pa.el bluetooth-uuid.el	\
	bluetooth.el bluetooth-plugin.el bluetooth-battery.el

ELPA_EXT = dash compat transient
LDFLAGS = $(foreach dep,$(ELPA_EXT), -L ./dep/$(dep))

.PHONY: compile clean depclean depsetup depupdate run

compile: $(EL:.el=.elc)

clean:
	rm -f *.elc

depclean:
	rm -rf ./dep/*

depsetup:
	mkdir -p ./dep
	$(foreach dep,$(ELPA_EXT),git clone			\
	git://git.savannah.gnu.org/emacs/elpa.git		\
	--single-branch --branch externals/$(dep) ./dep/$(dep);)

depupdate:
	$(foreach dep,$(ELPA_EXT),cd ./dep/$(dep) && git pull && cd ../.. ;)

.SUFFIXES: .el .elc
.el.elc:
	$(EMACS) -Q --batch -L . $(LDFLAGS) -L ./dep/transient/lisp -f batch-byte-compile $<

run: $(EL:.el=.elc)
	$(EMACS) -Q -L . $(LDFLAGS) --eval "(load \"bluetooth\")" 	\
	--eval "(bluetooth-list-devices)" &
