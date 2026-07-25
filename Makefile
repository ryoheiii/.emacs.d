SHELL := /bin/bash

EMACS ?= emacs
GIT ?= git

TESTS_DIR := $(CURDIR)/tests
STRAIGHT_DIR ?= $(CURDIR)/loads/straight
STRAIGHT_REPOS ?= $(STRAIGHT_DIR)/repos
STRAIGHT_BUILD ?= $(STRAIGHT_DIR)/build
STRAIGHT_VERSIONS ?= $(STRAIGHT_DIR)/versions
TEST_STRAIGHT_DIR ?= $(STRAIGHT_DIR)

# native-comp 無効ビルド（CI の Nix Emacs 等）では native-comp-eln-load-path が
# 未定義のまま early-init.el の startup-redirect-eln-cache が呼ばれて落ちるため、
# ロード前に defvar で定義する（native ビルドでは既存値を上書きしない）。
EMACS_TEST_OPTIONS = \
	--batch \
	--eval "(setq user-emacs-directory \"$$test_root/\")" \
	--eval "(defvar native-comp-eln-load-path nil)" \
	--eval "(setq native-comp-jit-compilation nil)" \
	--eval "(setq use-package-expand-minimally t)" \
	--eval "(setq use-package-verbose 'errors)" \
	--eval "(setq my-straight-base-dir-override \"$(STRAIGHT_DIR)/../\")" \
	--eval "(defvar my-test--recorded-warnings nil)" \
	--eval "(advice-add 'display-warning :before (lambda (type message &optional level &rest _) (push (list type message level) my-test--recorded-warnings)))"

.PHONY: all prepare-straight lint test-unit test-startup test-keybinding
.PHONY: test-cpp-config test-invariants test-tty
.PHONY: test-setup test clean-test straight-thaw

all: test

define prepare_test_root
test_root="$$(mktemp -d)"; \
test -n "$$test_root"; \
trap 'find "$$test_root" -depth -delete' EXIT; \
$(GIT) checkout-index --all --prefix="$$test_root/"; \
if ! $(GIT) diff --quiet; then \
	$(GIT) diff --binary | $(GIT) -C "$$test_root" apply --whitespace=nowarn; \
fi; \
if [[ -e "$$test_root/loads/straight" || -L "$$test_root/loads/straight" ]]; then \
	find "$$test_root/loads/straight" -depth -delete; \
fi; \
mkdir -p "$$test_root/loads"; \
ln -s "$(TEST_STRAIGHT_DIR)" "$$test_root/loads/straight";
endef

prepare-straight:
	@mkdir -p "$(STRAIGHT_REPOS)" "$(STRAIGHT_BUILD)" "$(STRAIGHT_VERSIONS)"

lint: | prepare-straight
	@set -eu -o pipefail; \
	$(prepare_test_root) \
	lint_dir="$$(mktemp -d)"; \
	trap 'find "$$lint_dir" -depth -delete; find "$$test_root" -depth -delete' EXIT; \
	mapfile -t sources < <($(GIT) ls-files -- \
		'loads/inits/*.el' 'loads/site-elisp/my-*.el'); \
	test "$${#sources[@]}" -gt 0; \
	$(EMACS) $(EMACS_TEST_OPTIONS) \
		-l "$$test_root/early-init.el" \
		-l "$$test_root/init.el" \
		--eval "(setq byte-compile-dest-file-function \
			(lambda (source) \
			  (expand-file-name \
			   (concat (file-name-nondirectory source) \"c\") \
			   \"$$lint_dir/\")))" \
		-f batch-byte-compile "$${sources[@]}"

test-unit: | prepare-straight
	@set -eu; \
	$(prepare_test_root) \
	$(EMACS) $(EMACS_TEST_OPTIONS) \
		-l "$$test_root/early-init.el" \
		-l "$(TESTS_DIR)/my-test-unit.el" \
		--eval "(ert-run-tests-batch-and-exit '(tag :unit))"

test-startup: | prepare-straight
	@set -eu; \
	$(prepare_test_root) \
	$(EMACS) $(EMACS_TEST_OPTIONS) \
		-l "$$test_root/early-init.el" \
		-l "$$test_root/init.el" \
		-l "$(TESTS_DIR)/my-test-startup.el"

test-keybinding: | prepare-straight
	@set -eu; \
	$(prepare_test_root) \
	$(EMACS) $(EMACS_TEST_OPTIONS) \
		-l "$$test_root/early-init.el" \
		-l "$$test_root/init.el" \
		-l "$(TESTS_DIR)/my-test-startup.el" \
		-l "$(TESTS_DIR)/my-test-keybindings.el" \
		--eval "(ert-run-tests-batch-and-exit '(tag :keybinding))"

test-deferred: | prepare-straight
	@set -eu; \
	$(prepare_test_root) \
	$(EMACS) $(EMACS_TEST_OPTIONS) \
		-l "$$test_root/early-init.el" \
		-l "$$test_root/init.el" \
		-l "$(TESTS_DIR)/my-test-startup.el" \
		-l "$(TESTS_DIR)/my-test-deferred.el" \
		--eval "(ert-run-tests-batch-and-exit '(tag :deferred))"

test-invariants: | prepare-straight
	@set -eu; \
	$(prepare_test_root) \
	$(EMACS) $(EMACS_TEST_OPTIONS) \
		-l "$$test_root/early-init.el" \
		-l "$$test_root/init.el" \
		-l "$(TESTS_DIR)/my-test-startup.el" \
		-l "$(TESTS_DIR)/my-test-global-modes.el" \
		-l "$(TESTS_DIR)/my-test-packages.el" \
		--eval "(ert-run-tests-batch-and-exit '(tag :invariant))"

test-tty: | prepare-straight
	@set -eu; \
	$(prepare_test_root) \
	$(EMACS) $(EMACS_TEST_OPTIONS) \
		-l "$$test_root/early-init.el" \
		-l "$$test_root/init.el" \
		-l "$(TESTS_DIR)/my-test-startup.el" \
		-l "$(TESTS_DIR)/my-test-tty.el" \
		--eval "(ert-run-tests-batch-and-exit '(tag :tty))"

test-cpp-config: | prepare-straight
	@set -eu; \
	$(prepare_test_root) \
	$(EMACS) $(EMACS_TEST_OPTIONS) \
		-l "$$test_root/early-init.el" \
		-l "$$test_root/init.el" \
		-l "$(TESTS_DIR)/my-test-startup.el" \
		-l "$(TESTS_DIR)/my-test-cpp-config.el" \
		--eval "(ert-run-tests-batch-and-exit '(tag :cpp-config))"

test-setup:
	@set -eu; \
	test_home="$$(mktemp -d)"; \
	test -n "$$test_home"; \
	trap 'find "$$test_home" -depth -delete' EXIT; \
	HOME="$$test_home" ./test-emacs-setup.sh

test:
	+@$(MAKE) lint
	+@$(MAKE) test-unit
	+@$(MAKE) test-startup
	+@$(MAKE) test-keybinding
	+@$(MAKE) test-cpp-config
	+@$(MAKE) test-deferred
	+@$(MAKE) test-invariants
	+@$(MAKE) test-tty
	+@$(MAKE) test-setup

# CI の部分一致キャッシュを lockfile のリビジョンへ揃える。
# thaw 中の対話プロンプト（例: straight.el 自身のブランチ正規化確認）は
# batch では表示できず error になるため、「c: この repo の処理をキャンセルして
# 先へ進む」を自動応答する。スキップ内容はログの message で確認できる。
straight-thaw: | prepare-straight
	@set -eu; \
	test "$${CI:-}" = "true"; \
	test ! -L "$(STRAIGHT_REPOS)"; \
	test ! -L "$(STRAIGHT_BUILD)"; \
	$(prepare_test_root) \
	$(EMACS) $(EMACS_TEST_OPTIONS) \
		-l "$$test_root/early-init.el" \
		-l "$$test_root/init.el" \
		--eval "(advice-add 'straight--popup-raw :override \
			(lambda (prompt actions) \
			  (message \"thaw prompt を自動キャンセル: %s\" prompt) \
			  (funcall (nth 2 (assoc \"c\" actions)))))" \
		-f straight-thaw-versions

clean-test:
	@find "$(TESTS_DIR)" -type f -name '*.elc' -delete
