DOT_FILES = .zshrc .emacs.d

all: install

install: $(foreach f, $(DOT_FILES), link-dot-file-$(f))

zsh: $(foreach f, $(filter .zsh%, $(DOT_FILES)), link-dot-file-$(f))

emacs: $(foreach f, $(filter .emacs%, $(DOT_FILES)), link-dot-file-$(f))

.PHONY: clean
clean: $(foreach f, $(DOT_FILES), unlink-dot-file-$(f))

link-dot-file-%: %
	@echo "Create Symlink $< => $(HOME)/$<"
	@ln -snf $(CURDIR)/$< $(HOME)/$<

unlink-dot-file-%: %
	@echo "Remove Symlink $(HOME)/$<"
	@$(RM) $(HOME)/$<
