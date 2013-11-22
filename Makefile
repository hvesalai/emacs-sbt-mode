############################################################-*-Makefile-*-####
# Makefile for compiling the major mode of Emacs
##############################################################################

##############################################################################
# Configuration

ROOT			 = .

SOURCE_DIR		 = $(ROOT)

##############################################################################
# Variables

# Emacs Lisp
MODE_NAME               = sbt-mode

ELISP_COMMAND		?= emacs
ELISP_OPTIONS		+= -batch -no-site-file -q
ELISP_OPTIONS           += -L $(ROOT)

ELISP_FILES		+= $(MODE_NAME)
ELISP_FILES		+= $(MODE_NAME)-buffer
ELISP_FILES		+= $(MODE_NAME)-comint
ELISP_FILES		+= $(MODE_NAME)-project
ELISP_FILES		+= $(MODE_NAME)-rgrep

PKG_FILE		= $(SOURCE_DIR)/$(MODE_NAME)-pkg.el

ELISP_SOURCES		+= $(ELISP_FILES:%=$(SOURCE_DIR)/%.el)

##############################################################################

RM			?= rm -f
RMDIR                   ?= rmdir
TOUCH			?= touch
VERSION                 := $(shell ${ELISP_COMMAND} $(ELISP_OPTIONS) --eval '(princ (format "%s\n" (nth 2 (read (find-file "$(PKG_FILE)")))))')
MODE_NAME_VERSION       = $(MODE_NAME)-$(VERSION)

##############################################################################
# Commands

all: .latest-build

clean:
	$(RM) *.elc .latest-* autoloads.el *.tar
	[ ! -d $(MODE_NAME_VERSION) ] || $(RM) $(MODE_NAME_VERSION)/*
	[ ! -d $(MODE_NAME_VERSION) ] || $(RMDIR) $(MODE_NAME_VERSION)

.PHONY: all
.PHONY: clean

##############################################################################
# Rules

.latest-build: $(ELISP_SOURCES)
	$(ELISP_COMMAND) $(ELISP_OPTIONS) -f batch-byte-compile $(ELISP_SOURCES)
	@$(TOUCH) $@

##############################################################################

autoloads: $(ELISP_SOURCES)
	$(ELISP_COMMAND) $(ELISP_OPTIONS) --eval "(setq make-backup-files nil)" --eval "(setq generated-autoload-file (expand-file-name \"autoloads.el\"))" -f batch-update-autoloads `pwd`


package:
	mkdir -p $(MODE_NAME_VERSION)
	cp $(ELISP_SOURCES) $(PKG_FILE) $(MODE_NAME_VERSION)
	tar cf $(MODE_NAME_VERSION).tar $(MODE_NAME_VERSION)
