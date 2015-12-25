image_name := arnau/icarus

DOCKER := docker
DKR_JOB := $(DOCKER) run --rm -it

default: build

install:
	@$(job) cabal install --only-dependencies --enable-tests
.PHONY: install

build:
	$(DOCKER) build -t $(image_name) .
.PHONY: build

shell:
	@$(call job, bash)
.PHONY: shell

repl:
	@$(call job)
.PHONY: repl

test:
	@$(call job, cabal test)
.PHONY: test

lint:
	@$(call job, hlint .)
.PHONY: lint

lint-src:
	@$(call job, hlint src)
.PHONY: lint-src

define job
  $(DKR_JOB) --volume $(PWD):/source \
             --workdir /source \
             $(image_name) \
             $1
endef
