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
ELISP_COMMAND		?= emacs
ELISP_OPTIONS		+= -batch -no-site-file
ELISP_OPTIONS           += -L $(ROOT)
ELISP_OPTIONS		+= -f batch-byte-compile


ELISP_FILES		+= sbt-mode
ELISP_FILES		+= sbt-mode-buffer
ELISP_FILES		+= sbt-mode-comint
ELISP_FILES		+= sbt-mode-project
ELISP_FILES		+= sbt-mode-rgrep

ELISP_SOURCES		+= $(ELISP_FILES:%=$(SOURCE_DIR)/%.el)

##############################################################################

RM			?= rm -f
TOUCH			?= touch

##############################################################################
# Commands

all: .latest-build

clean:
	$(RM) *.elc .latest-* autoloads.el

.PHONY: all
.PHONY: clean

##############################################################################
# Rules

.latest-build: $(ELISP_SOURCES)
	$(ELISP_COMMAND) $(ELISP_OPTIONS) $(ELISP_SOURCES)
	@$(TOUCH) $@

##############################################################################

autoloads: $(ELISP_SOURCES)
	emacs -batch -q --no-site-file --eval "(setq make-backup-files nil)" --eval "(setq generated-autoload-file (expand-file-name \"autoloads.el\"))" -f batch-update-autoloads `pwd`

