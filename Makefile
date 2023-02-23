LISP ?= sbcl
PROJECT ?= dino-lisp
corefile := core_$(shell date +%Y%m%d_%H)

echo:
	@echo $(PROJECT)
	@echo $(corefile)

clean:
	rm -rf ./target
	rm -rf ./bin
core:
	mkdir -p target
	$(LISP) --load $(PROJECT).asd \
		--eval '(ql:quickload "$(PROJECT)")' \
		--eval "(sb-ext:save-lisp-and-die #p\"target/$(corefile)\" :executable t)"

build:
	mkdir -p bin
	$(LISP)	--non-interactive \
		--eval '(ql:quickload "deploy")' \
		--load $(PROJECT).asd \
		--eval '(ql:quickload "$(PROJECT)")' \
		--eval '(asdf:make :dino-lisp)'
