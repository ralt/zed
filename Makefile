SOURCES := $(wildcard *.lisp) $(wildcard *.asd)
QL_LOCAL=$(PWD)/.quicklocal/quicklisp
LOCAL_OPTS=--noinform --noprint --disable-debugger --no-sysinit --no-userinit
QL_OPTS=--load $(QL_LOCAL)/setup.lisp

all: bin/git-zed

deps:
	@sbcl $(LOCAL_OPTS) $(QL_OPTS) \
		--eval '(push "$(PWD)/" asdf:*central-registry*)' \
		--eval '(ql:quickload :zed)' \
		--eval '(quit)'
	@touch $@

bin/git-zed: $(SOURCES) $(QL_LOCAL)/setup.lisp deps
	@buildapp \
		--asdf-tree $(QL_LOCAL)/local-projects \
		--asdf-tree $(QL_LOCAL)/dists \
		--asdf-path . \
		--load-system zed \
		--eval '(setf *debugger-hook* (lambda (c h) (declare (ignore h)) (format t "~A~%" c) (uiop:quit -1)))' \
		--compress-core \
		--output bin/git-zed --entry zed:main

.PHONY: clean

clean:
	@rm -rf deps .quicklocal bin/git-zed quicklisp.lisp

$(QL_LOCAL)/setup.lisp: quicklisp.lisp
	@sbcl --noinform --noprint --disable-debugger --no-sysinit --no-userinit \
		--load quicklisp.lisp \
		--eval '(quicklisp-quickstart:install :path "$(QL_LOCAL)")' \
		--eval '(quit)'

quicklisp.lisp:
	@wget https://beta.quicklisp.org/quicklisp.lisp
	@echo '4a7a5c2aebe0716417047854267397e24a44d0cce096127411e9ce9ccfeb2c17 quicklisp.lisp' | sha256sum -c -
