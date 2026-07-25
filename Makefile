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
.PHONY: test-cpp-config test-invariants test-tty test-tty-live
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

define MY_TTY_LIVE_SETUP_BODY
mv "$$test_root/early-init.el" "$$test_root/my-tty-early-init-real.el"
cat > "$$test_root/early-init.el" <<'MY_TTY_EARLY_INIT'
;; tty テスト専用 shim(テスト実行時の一時生成物)
;; MY_TTY_TEST_STRAIGHT_BASE_DIR は straight-base-dir に入れる値
;; = 既存ハーネス(EMACS_TEST_OPTIONS)と同じ loads/ ディレクトリ。
;; straight は内部で straight/ を付加するため loads/straight を渡してはならない。
(setq my-straight-base-dir-override (getenv "MY_TTY_TEST_STRAIGHT_BASE_DIR"))
;; native-comp 設定は EMACS_TEST_OPTIONS とのパリティ(CI の Nix Emacs 対応)
(defvar native-comp-eln-load-path nil)
(setq native-comp-jit-compilation nil)
;; 起動時警告の構造化レコーダー(my-test-startup-check-warnings が照合する)
(defvar my-test--recorded-warnings nil)
(advice-add 'display-warning :before
            (lambda (type message &optional level &rest _)
              (push (list type message level) my-test--recorded-warnings)))
(load (expand-file-name "my-tty-early-init-real.el" user-emacs-directory) nil t)
MY_TTY_EARLY_INIT
mkdir -p "$$test_root/xdg-cache"
cat > "$$test_root/run-tty-test.sh" <<MY_TTY_TEST_RUNNER
#!/bin/sh
set -eu
stty cols 120 rows 40
export TERM=xterm-256color
export HOME="$$test_root"
export XDG_CACHE_HOME="$$test_root/xdg-cache"
export MY_TTY_TEST_STRAIGHT_BASE_DIR="$(STRAIGHT_DIR)/../"
exec $(EMACS) -nw --no-site-file --no-site-lisp \
  --init-directory="$$test_root" \
  -L "$(TESTS_DIR)" \
  -l "$(TESTS_DIR)/my-test-startup.el" \
  -l "$(TESTS_DIR)/my-test-tty-live.el"
MY_TTY_TEST_RUNNER
chmod +x "$$test_root/run-tty-test.sh"
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

test-tty-live: export MY_TTY_LIVE_SETUP = $(MY_TTY_LIVE_SETUP_BODY)
test-tty-live: | prepare-straight
	@set -eu; \
	test "$$(uname)" = Linux || { \
		printf '%s\n' "test-tty-live: Linux が必要です" >&2; \
		exit 1; \
	}; \
	command -v script >/dev/null || { \
		printf '%s\n' "test-tty-live: script コマンドが必要です" >&2; \
		exit 1; \
	}; \
	command -v timeout >/dev/null || { \
		printf '%s\n' "test-tty-live: timeout コマンドが必要です" >&2; \
		exit 1; \
	}; \
	$(prepare_test_root) \
	export test_root; \
	$(SHELL) -eu -c "$$MY_TTY_LIVE_SETUP"; \
	sh -n "$$test_root/run-tty-test.sh"; \
	timeout 180 script -qec "$$test_root/run-tty-test.sh" /dev/null

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
	+@$(MAKE) test-tty-live
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
