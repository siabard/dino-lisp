LISP ?= sbcl
PROJECT ?= dino-lisp
corefile := core_$(shell date +%Y%m%d_%H)

echo:
	@echo $(PROJECT)
	@echo $(corefile)

core:
	$(LISP) --load $(PROJECT).asd \
		--eval '(ql:quickload "$(PROJECT)")' \
		--eval "(sb-ext:save-lisp-and-die #p\"target/$(corefile)\" :executable t)"

build:
	$(LISP)	--non-interactive \
		--eval '(ql:quickload "deploy")' \
		--load $(PROJECT).asd \
		--eval '(ql:quickload "$(PROJECT)")' \
		--eval '(asdf:make :fantasy_racing)'
