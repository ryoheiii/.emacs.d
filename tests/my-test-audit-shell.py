#!/usr/bin/env python3
"""監査で再現した破損・誤成功を、使い捨て HOME とスタブで検査する。"""
import os
from pathlib import Path
import platform
import runpy
import contextlib
import io
import re
from unittest import mock
import shutil
import subprocess
import tarfile
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]


class AuditShellTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory(prefix='emacs-audit-test-')
        self.addCleanup(self.tmp.cleanup)
        self.root = Path(self.tmp.name)
        self.home = self.root / 'home'
        self.repo = self.home / '.emacs.d'
        self.repo.mkdir(parents=True)
        self.bin = self.root / 'bin'
        self.bin.mkdir()
        self.env = dict(os.environ, HOME=str(self.home), PATH=f'{self.bin}:{os.defpath}',
                        EMACS_SETUP_TEST_SANDBOX='1')
        # 新たな処理経路が増えても実サービスやホストを変更させない。
        for name in ('sudo', 'curl', 'fnm', 'node', 'npm', 'codex'):
            self.stub(name, 'exit 97\n')
        self.stub('emacs', 'exit 0\n')

    def stub(self, name, body):
        p = self.bin / name
        p.write_text('#!/bin/bash\n' + body)
        p.chmod(0o755)

    def run_cmd(self, *args, cwd=None):
        return subprocess.run(args, env=self.env, cwd=cwd or ROOT,
                              text=True, capture_output=True, timeout=30)

    def setup_cmd(self, *args):
        return self.run_cmd('bash', str(ROOT / 'emacs-setup.sh'), *args)

    def fail_after_move(self, destination, mode):
        """対象の rename 後に一度だけ失敗または親シェルへのシグナルを注入する。"""
        marker = self.root / 'move-fault-fired'
        marker.unlink(missing_ok=True)
        self.env.update(FAIL_DEST=str(destination), FAIL_MODE=mode, FAIL_MARKER=str(marker))
        self.stub('mv', 'if [ "${!#}" = "$FAIL_DEST" ] && [ ! -e "$FAIL_MARKER" ]; then\n'
                  '  /bin/mv "$@" || exit $?\n'
                  '  : > "$FAIL_MARKER"\n'
                  '  if [ "$FAIL_MODE" = error ]; then exit 9; fi\n'
                  '  kill -s "$FAIL_MODE" "$PPID"\n'
                  '  exit 0\n'
                  'fi\nexec /bin/mv "$@"\n')
        return marker

    def package_tree(self):
        s = self.repo / 'loads/straight'
        (s / 'repos/example').mkdir(parents=True)
        (s / 'repos/example/data').write_text('old package')
        (s / 'versions').mkdir()
        (s / 'versions/default.el').write_text('(("example" . "abc"))')
        return s

    def node_archive(self, node='echo v22.0.0\n', npm='echo 10.0.0\n'):
        arch = {'x86_64': 'x64', 'aarch64': 'arm64', 'armv7l': 'armv7l'}[platform.machine()]
        name = f'node-v22.0.0-linux-{arch}'
        src = self.root / 'node-src'
        if src.exists():
            shutil.rmtree(src)
        (src / 'bin').mkdir(parents=True)
        for command, body in (('node', node), ('npm', npm)):
            if body is not None:
                p = src / 'bin' / command
                p.write_text('#!/bin/bash\n' + body)
                p.chmod(0o755)
        dl = self.home / '.local/downloads/node'
        dl.mkdir(parents=True, exist_ok=True)
        with tarfile.open(dl / (name + '.tar.xz'), 'w:xz') as t:
            t.add(src, arcname=name)
        return self.home / '.local/share/nodejs' / name

    def test_surplus_arguments_do_not_modify_home(self):
        marker = self.repo / 'var/backup/precious'
        marker.parent.mkdir(parents=True)
        marker.write_text('unrecoverable')
        for action in ('--clean', '--clean-all', '--uninstall', '--uninstall-node',
                       '--setup-node', '--setup-treesit', '--list', '--packing-package',
                       '--extract-package', '--help'):
            with self.subTest(action=action):
                r = self.setup_cmd(action, '--help')
                self.assertNotEqual(r.returncode, 0, r.stdout)
                self.assertEqual(marker.read_text(), 'unrecoverable')

    def test_failed_archive_preserves_previous_archive(self):
        self.package_tree()
        archive = self.repo / 'package.tar.gz'
        archive.write_bytes(b'precious archive')
        self.stub('tar', 'while [ "$#" -gt 0 ]; do\n'
                  'if [ "$1" = -czf ]; then shift; echo broken > "$1"; exit 9; fi\n'
                  'shift\ndone\nexit 9\n')
        r = self.setup_cmd('--packing-package')
        self.assertNotEqual(r.returncode, 0)
        self.assertEqual(archive.read_bytes(), b'precious archive')
        self.assertEqual(sorted(p.name for p in self.repo.iterdir()), ['loads', 'package.tar.gz'])

    def test_extract_preserves_grammars_and_server(self):
        self.package_tree()
        for part in ('tree-sitter', 'copilot', 'eln-cache'):
            p = self.repo / 'var/package' / part / 'sentinel'
            p.parent.mkdir(parents=True)
            p.write_text(part)
        self.assertEqual(self.setup_cmd('--packing-package').returncode, 0)
        r = self.setup_cmd('--extract-package')
        self.assertEqual(r.returncode, 0, r.stderr)
        for part in ('tree-sitter', 'copilot'):
            self.assertEqual((self.repo / 'var/package' / part / 'sentinel').read_text(), part)
        self.assertFalse((self.repo / 'var/package/eln-cache').exists())

    def test_node_failed_verification_is_failure(self):
        for node, npm in [('exit 9\n', 'echo 10.0.0\n'), ('echo v22.0.0\n', 'exit 10\n'),
                          ('echo v22.0.0\n', None), ('echo nonsense\n', 'echo 10.0.0\n')]:
            with self.subTest(node=node, npm=npm):
                self.node_archive(node, npm)
                r = self.setup_cmd('--setup-node')
                self.assertNotEqual(r.returncode, 0, r.stdout)
                self.assertFalse((self.home / '.local/node').is_symlink())

    def test_node_failed_reinstall_preserves_old_version_and_link(self):
        live = self.node_archive()
        r = self.setup_cmd('--setup-node')
        self.assertEqual(r.returncode, 0, r.stderr)
        old = (live / 'bin/node').read_bytes()
        link = (self.home / '.local/node').readlink()
        self.node_archive(node=None)
        self.assertNotEqual(self.setup_cmd('--setup-node').returncode, 0)
        self.assertEqual((live / 'bin/node').read_bytes(), old)
        self.assertEqual((self.home / '.local/node').readlink(), link)

    def test_node_link_commit_failure_rolls_back(self):
        live = self.node_archive()
        self.assertEqual(self.setup_cmd('--setup-node').returncode, 0)
        old = (live / 'bin/node').read_bytes()
        self.node_archive('echo v22.0.0\n# 新しい配置内容\n')
        self.stub('mv', 'if [ "${!#}" = "$HOME/.local/node" ]; then exit 9; fi\nexec /bin/mv "$@"\n')
        self.assertNotEqual(self.setup_cmd('--setup-node').returncode, 0)
        self.assertEqual((live / 'bin/node').read_bytes(), old)
        self.assertEqual((self.home / '.local/node').readlink(), live)

    def test_bench_rejects_paths_before_deletion(self):
        fake = self.root / 'bench-repo'
        (fake / 'tests').mkdir(parents=True)
        (fake / '.bench').mkdir()
        shutil.copyfile(ROOT / 'tests/my-bench-run.sh', fake / 'tests/my-bench-run.sh')
        victim = fake / 'victim'
        victim.mkdir()
        (victim / 'data').write_text('keep')
        r = self.run_cmd('bash', str(fake / 'tests/my-bench-run.sh'), '../victim', '1', str(self.root / 'out'))
        self.assertNotEqual(r.returncode, 0)
        self.assertEqual((victim / 'data').read_text(), 'keep')
        self.assertFalse((self.root / 'out').exists())

    def test_review_does_not_accept_stale_reply(self):
        prompt, reply = self.root / 'prompt', self.root / 'reply'
        prompt.write_text('review')
        reply.write_text('VERDICT: APPROVED\n')
        self.stub('codex', 'exit 0\n')
        r = self.run_cmd('bash', str(ROOT / '.claude/scripts/codex-review.sh'),
                         str(prompt), str(reply), '--resume', 'fixture-id')
        self.assertEqual(r.returncode, 2, r.stdout + r.stderr)
        self.assertEqual(reply.read_text(), 'VERDICT: APPROVED\n')

    def test_review_session_write_failure_is_failure(self):
        prompt, reply = self.root / 'prompt', self.root / 'reply'
        prompt.write_text('review')
        self.stub('codex', 'while [ "$#" -gt 0 ]; do\n'
                  'if [ "$1" = -o ]; then shift; echo "VERDICT: APPROVED" > "$1"; fi\n'
                  'shift\ndone\necho "session id: fixture-id"\n')
        r = self.run_cmd('bash', str(ROOT / '.claude/scripts/codex-review.sh'),
                         str(prompt), str(reply), str(self.root / 'missing/session'))
        self.assertNotEqual(r.returncode, 0, r.stdout)
        self.assertNotIn('OK:', r.stdout)

    def test_fnm_env_and_runtime_failures_propagate(self):
        self.stub('node', 'echo v22.0.0\n')
        self.stub('npm', 'echo 10.0.0\n')
        self.stub('fnm', 'case "$1" in env) echo ":";; esac\n')
        self.assertEqual(self.setup_cmd('--setup-node').returncode, 0)
        self.stub('fnm', 'if [ "$1" = env ]; then echo ":"; exit 9; fi\n')
        self.assertNotEqual(self.setup_cmd('--setup-node').returncode, 0)
        self.stub('fnm', 'case "$1" in env) echo ":";; esac\n')
        self.stub('npm', 'exit 10\n')
        self.assertNotEqual(self.setup_cmd('--setup-node').returncode, 0)

    def test_non_gnu_mv_replaces_existing_archive_and_node_link(self):
        tree = self.package_tree()
        self.assertEqual(self.setup_cmd('--packing-package').returncode, 0)
        live = self.node_archive()
        self.assertEqual(self.setup_cmd('--setup-node').returncode, 0)
        # BSD の機能判定と -h を模擬し、実際の移動は fixture 内で GNU mv に委譲する。
        self.stub('mv', 'if [ "$1" = --version ]; then exit 1; fi\n'
                  'if [ "$1" = -fh ]; then shift; exec /bin/mv -Tf "$@"; fi\nexec /bin/mv "$@"\n')
        (tree / 'repos/example/data').write_text('new package')
        result = self.setup_cmd('--packing-package')
        self.assertEqual(result.returncode, 0, result.stderr)
        with tarfile.open(self.repo / 'package.tar.gz') as archive:
            self.assertEqual(archive.extractfile('straight/repos/example/data').read(), b'new package')
        self.node_archive('echo v22.0.0\n# new contents\n')
        result = self.setup_cmd('--setup-node')
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual((self.home / '.local/node').readlink(), live)
        self.assertIn('new contents', (live / 'bin/node').read_text())

    def test_ci_cannot_allow_missing_active_mozc(self):
        workflow = (ROOT / '.github/workflows/test.yml').read_text()
        allowed = re.search(r'LOCK_ALLOW_MISSING="([^"]*)"', workflow).group(1).split()
        lock = ROOT / 'loads/straight/versions/default.el'
        entries = dict(re.findall(r'\("([^"\n]+)"\s+\.\s+"([a-f0-9]{40})"\)', lock.read_text()))
        repos = self.root / 'repos'
        for name in entries:
            if name != 'mozc':
                (repos / name).mkdir(parents=True)
        check = runpy.run_path(str(ROOT / 'tests/my-check-lockfile.py'))['check']
        def git_result(args, **_kwargs):
            return entries[Path(args[2]).name] if 'rev-parse' in args else ''
        with mock.patch('subprocess.check_output', side_effect=git_result), contextlib.redirect_stdout(io.StringIO()):
            with self.assertRaisesRegex(ValueError, 'mozc: repo がありません'):
                check(lock, repos, allowed)

    def ship_fixture(self):
        main, bare, worktree = (self.root / name for name in ('main', 'remote.git', 'task'))
        for args in [('git', 'init', '--bare', '--initial-branch=main', str(bare)),
                     ('git', 'init', '--initial-branch=main', str(main)),
                     ('git', '-C', str(main), 'config', 'user.name', 'Fixture'),
                     ('git', '-C', str(main), 'config', 'user.email', 'fixture@example.invalid'),
                     ('git', '-C', str(main), 'commit', '--allow-empty', '-qm', 'base'),
                     ('git', '-C', str(main), 'remote', 'add', 'origin', str(bare)),
                     ('git', '-C', str(main), 'push', '-u', 'origin', 'main'),
                     ('git', '-C', str(main), 'worktree', 'add', '-b', 'fix/fixture', str(worktree)),
                     ('git', '-C', str(worktree), 'commit', '--allow-empty', '-qm', 'task'),
                     ('git', '-C', str(worktree), 'push', '-u', 'origin', 'fix/fixture')]:
            result = self.run_cmd(*args)
            self.assertEqual(result.returncode, 0, result.stderr)
        self.env['GIT_MERGE_AUTOEDIT'] = 'no'
        self.env['GH_ARGS_LOG'] = str(self.root / 'gh-args')
        self.stub('gh', 'printf "%s\n" "$*" >> "$GH_ARGS_LOG"\nif [ "$2" = list ]; then echo 42; fi\n')
        return main, bare, worktree

    def ship_step(self, number, cwd):
        skill = (ROOT / '.claude/skills/x-ship/SKILL.md').read_text()
        section = skill.split(f'### {number}.', 1)[1].split('\n### ', 1)[0]
        script = re.search(r'```bash\n(.*?)\n```', section, re.S).group(1)
        return self.run_cmd('bash', '-c', script, cwd=cwd)

    def test_ship_independent_shells_push_ci_and_cleanup(self):
        main, bare, worktree = self.ship_fixture()
        self.assertEqual(self.ship_step(6, worktree).returncode, 0)
        merged = self.run_cmd('git', '-C', str(main), 'rev-parse', 'HEAD').stdout.strip()
        result = self.ship_step(7, worktree)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(self.run_cmd('git', '-C', str(bare), 'rev-parse', 'main').stdout.strip(), merged)
        args = (self.root / 'gh-args').read_text()
        self.assertIn(f'--commit {merged}', args)
        self.assertIn('--workflow test.yml --event push', args)
        self.assertIn('run watch 42 --exit-status', args)
        result = self.ship_step(8, worktree)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertFalse(worktree.exists())
        self.assertNotEqual(self.run_cmd('git', '-C', str(main), 'show-ref', '--verify', 'refs/heads/fix/fixture').returncode, 0)
        self.assertNotEqual(self.run_cmd('git', '-C', str(bare), 'show-ref', '--verify', 'refs/heads/fix/fixture').returncode, 0)

    def test_ship_no_push_state_survives_independent_shells(self):
        main, bare, worktree = self.ship_fixture()
        remote_head = self.run_cmd('git', '-C', str(bare), 'rev-parse', 'main').stdout
        self.env['SHIP_PUSH'] = 'no'
        self.assertEqual(self.ship_step(6, worktree).returncode, 0)
        del self.env['SHIP_PUSH']
        self.assertEqual(self.ship_step(7, worktree).returncode, 0)
        self.assertFalse((self.root / 'gh-args').exists())
        self.assertEqual(self.ship_step(8, worktree).returncode, 0)
        self.assertFalse(worktree.exists())
        self.assertEqual(self.run_cmd('git', '-C', str(bare), 'rev-parse', 'main').stdout, remote_head)
        self.assertEqual(self.run_cmd('git', '-C', str(bare), 'show-ref', '--verify', 'refs/heads/fix/fixture').returncode, 0)

    def test_ship_rejects_main_advanced_before_step(self):
        main, _bare, worktree = self.ship_fixture()
        self.assertEqual(self.run_cmd('git', '-C', str(main), 'commit', '--allow-empty', '-qm', 'concurrent main').returncode, 0)
        self.assertEqual(self.run_cmd('git', '-C', str(main), 'push', 'origin', 'main').returncode, 0)
        head = self.run_cmd('git', '-C', str(main), 'rev-parse', 'HEAD').stdout
        result = self.ship_step(6, worktree)
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(self.run_cmd('git', '-C', str(main), 'rev-parse', 'HEAD').stdout, head)
        self.assertTrue(worktree.exists())

    def test_node_active_path_states(self):
        live = self.node_archive()
        active = self.home / '.local/node'
        for kind in ('file', 'directory', 'broken-link', 'other-version'):
            with self.subTest(kind=kind):
                if active.is_symlink() or active.is_file():
                    active.unlink()
                elif active.is_dir():
                    active.rmdir()
                if kind == 'file':
                    active.write_text('keep')
                elif kind == 'directory':
                    active.mkdir()
                else:
                    active.symlink_to(self.home / kind)
                result = self.setup_cmd('--setup-node')
                if kind in ('file', 'directory'):
                    self.assertNotEqual(result.returncode, 0)
                    self.assertFalse(active.is_symlink())
                else:
                    self.assertEqual(result.returncode, 0, result.stderr)
                    self.assertEqual(active.readlink(), live)

    def test_node_placement_and_rollback_failures(self):
        live = self.node_archive()
        self.assertEqual(self.setup_cmd('--setup-node').returncode, 0)
        old = (live / 'bin/node').read_bytes()
        self.node_archive('echo v22.0.0\n# replacement\n')
        self.stub('mv', 'for arg in "$@"; do case "$arg" in */.install-*/node-*) exit 9;; esac; done\nexec /bin/mv "$@"\n')
        self.assertNotEqual(self.setup_cmd('--setup-node').returncode, 0)
        self.assertEqual((live / 'bin/node').read_bytes(), old)
        self.stub('mv', 'for arg in "$@"; do case "$arg" in */.install-*/node-*|*.bak) if [ -e "$arg" ]; then exit 9; fi;; esac; done\nexec /bin/mv "$@"\n')
        self.assertNotEqual(self.setup_cmd('--setup-node').returncode, 0)
        self.assertEqual((Path(str(live) + '.bak') / 'bin/node').read_bytes(), old)

    def test_node_move_completed_then_failed_or_signalled_restores_state(self):
        for kind in ('same-version', 'other-version', 'broken-link', 'absent'):
            phases = ('backup', 'install', 'link') if kind == 'same-version' else ('install', 'link')
            for phase in phases:
                for mode in ('error', 'INT', 'TERM'):
                    with self.subTest(kind=kind, phase=phase, mode=mode):
                        shutil.rmtree(self.home / '.local', ignore_errors=True)
                        live = self.node_archive()
                        active = self.home / '.local/node'
                        old_tree = live if kind == 'same-version' else live.parent / 'old-version'
                        if kind in ('same-version', 'other-version'):
                            old_tree.mkdir(parents=True)
                            (old_tree / 'precious').write_bytes(b'old node installation')
                            old_link = Path('share/nodejs') / old_tree.name
                        else:
                            old_link = Path('missing-node')
                        if kind != 'absent':
                            active.symlink_to(old_link)
                        destination = {'backup': Path(str(live) + '.bak'),
                                       'install': live, 'link': active}[phase]
                        marker = self.fail_after_move(destination, mode)
                        result = self.setup_cmd('--setup-node')
                        self.assertNotEqual(result.returncode, 0, result.stdout)
                        if mode != 'error':
                            self.assertEqual(result.returncode, {'INT': 130, 'TERM': 143}[mode])
                        self.assertTrue(marker.exists(), '故障点へ到達していない')
                        if kind == 'absent':
                            self.assertFalse(active.is_symlink())
                            self.assertFalse(active.exists())
                        else:
                            self.assertTrue(active.is_symlink(), result.stderr)
                            self.assertEqual(active.readlink(), old_link)
                        if kind in ('same-version', 'other-version'):
                            self.assertEqual((old_tree / 'precious').read_bytes(), b'old node installation')
                        if kind != 'same-version':
                            self.assertFalse(live.exists())
                        self.assertFalse(Path(str(live) + '.bak').exists())

    def test_node_archive_links_cannot_escape(self):
        live = self.node_archive()
        archive = next((self.home / '.local/downloads/node').glob('*.tar.xz'))
        for kind in (tarfile.SYMTYPE, tarfile.LNKTYPE):
            with self.subTest(kind=kind):
                with tarfile.open(archive, 'w:xz') as tar:
                    link = tarfile.TarInfo(live.name + '/escape')
                    link.type = kind
                    link.linkname = '../../outside'
                    tar.addfile(link)
                self.assertNotEqual(self.setup_cmd('--setup-node').returncode, 0)
                self.assertFalse(live.exists())

    def test_node_internal_leaf_symlink_is_supported(self):
        live = self.node_archive()
        src = self.root / 'node-src'
        (src / 'lib').mkdir()
        (src / 'bin/npm').rename(src / 'lib/npm-cli')
        (src / 'bin/npm').symlink_to('../lib/npm-cli')
        archive = next((self.home / '.local/downloads/node').glob('*.tar.xz'))
        with tarfile.open(archive, 'w:xz') as tar:
            tar.add(src, arcname=live.name)
        result = self.setup_cmd('--setup-node')
        self.assertEqual(result.returncode, 0, result.stderr)

    def test_node_rejects_members_below_archive_symlink(self):
        live = self.node_archive()
        archive = next((self.home / '.local/downloads/node').glob('*.tar.xz'))
        with tarfile.open(archive, 'w:xz') as tar:
            link = tarfile.TarInfo(live.name + '/alias')
            link.type, link.linkname = tarfile.SYMTYPE, '.'
            tar.addfile(link)
            entry = tarfile.TarInfo(live.name + '/alias/data')
            entry.size = 4
            tar.addfile(entry, io.BytesIO(b'data'))
        self.assertNotEqual(self.setup_cmd('--setup-node').returncode, 0)
        self.assertFalse(live.exists())

    def test_extract_keeps_eln_symlink_and_arbitrary_siblings(self):
        self.package_tree()
        external = self.root / 'external-cache'
        external.mkdir()
        (external / 'keep').write_text('keep')
        package = self.repo / 'var/package'
        package.mkdir(parents=True)
        (package / 'eln-cache').symlink_to(external)
        (package / 'unknown-data').write_bytes(b'unrecoverable')
        self.assertEqual(self.setup_cmd('--packing-package').returncode, 0)
        self.assertEqual(self.setup_cmd('--extract-package').returncode, 0)
        self.assertTrue((package / 'eln-cache').is_symlink())
        self.assertEqual((external / 'keep').read_text(), 'keep')
        self.assertEqual((package / 'unknown-data').read_bytes(), b'unrecoverable')

    def test_review_publication_failure_restores_all_old_outputs(self):
        prompt, reply, session = (self.root / name for name in ('prompt', 'reply', 'session'))
        prompt.write_text('review')
        for path in (reply, Path(str(reply) + '.log'), session):
            path.write_text('old ' + path.name)
        self.stub('codex', 'while [ "$#" -gt 0 ]; do if [ "$1" = -o ]; then shift; echo NEW > "$1"; fi; shift; done\necho "session id: new-id"\n')
        self.env['FAIL_DEST'] = str(session)
        self.stub('mv', 'if [ "${!#}" = "$FAIL_DEST" ]; then exit 9; fi\nexec /bin/mv "$@"\n')
        result = self.run_cmd('bash', str(ROOT / '.claude/scripts/codex-review.sh'), str(prompt), str(reply), str(session))
        self.assertNotEqual(result.returncode, 0)
        for path in (reply, Path(str(reply) + '.log'), session):
            self.assertEqual(path.read_text(), 'old ' + path.name)

    def test_review_move_completed_then_failed_or_signalled_restores_outputs(self):
        prompt, reply, session = (self.root / name for name in ('prompt', 'reply', 'session'))
        prompt.write_text('review')
        outputs = (reply, Path(str(reply) + '.log'), session)
        self.stub('codex', 'while [ "$#" -gt 0 ]; do if [ "$1" = -o ]; then shift; echo NEW > "$1"; fi; shift; done\necho "session id: new-id"\n')
        for existing in (True, False):
            for destination in outputs:
                for mode in ('error', 'INT', 'TERM'):
                    with self.subTest(existing=existing, destination=destination.name, mode=mode):
                        for path in outputs:
                            path.unlink(missing_ok=True)
                            if existing:
                                path.write_text('old ' + path.name)
                        marker = self.fail_after_move(destination, mode)
                        result = self.run_cmd('bash', str(ROOT / '.claude/scripts/codex-review.sh'),
                                              str(prompt), str(reply), str(session))
                        self.assertNotEqual(result.returncode, 0, result.stdout)
                        if mode != 'error':
                            self.assertEqual(result.returncode, {'INT': 130, 'TERM': 143}[mode])
                        self.assertTrue(marker.exists(), '故障点へ到達していない')
                        for path in outputs:
                            if existing:
                                self.assertEqual(path.read_text(), 'old ' + path.name)
                            else:
                                self.assertFalse(path.exists())

    def test_bench_manifest_excludes_failed_and_stale_trials(self):
        out = self.root / 'out'
        raw = out / 'raw/bare-run-abc123'
        raw.mkdir(parents=True)
        (raw / 'bare-1.log').write_text('MY_BENCH t1_window_setup=0.1\n')
        (raw / 'bare-2.log').write_text('MY_BENCH t1_window_setup=99.0\ninvalid: failed\n')
        (out / 'raw/bare-99.log').write_text('MY_BENCH t1_window_setup=999.0\n')
        (out / 'bare-manifest.tsv').write_text('raw/bare-run-abc123/bare-1.log\t100\n')
        result = self.run_cmd('bash', str(ROOT / 'tests/my-bench-summarize.sh'), str(out))
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn('t1_window_setup bare n=1 min=0.1000', result.stdout)
        self.assertNotIn('99.0000', result.stdout)

    def test_failed_bench_rerun_preserves_previous_manifest(self):
        fake = self.root / 'bench-repo'
        (fake / 'tests').mkdir(parents=True)
        script = fake / 'tests/my-bench-run.sh'
        shutil.copyfile(ROOT / 'tests/my-bench-run.sh', script)
        self.env.update(LOADAVG_MAX='100000', RETRY_MAX='0')
        self.stub('script', 'echo "MY_BENCH t1_window_setup=0.1"\n')
        out = self.root / 'out'
        result = self.run_cmd('bash', str(script), 'bare', '1', str(out))
        self.assertEqual(result.returncode, 0, result.stderr)
        manifest = (out / 'bare-manifest.tsv').read_bytes()
        log_path = out / manifest.decode().split('\t')[0]
        log = log_path.read_bytes()
        self.stub('script', 'exit 9\n')
        self.assertNotEqual(self.run_cmd('bash', str(script), 'bare', '1', str(out)).returncode, 0)
        self.assertEqual((out / 'bare-manifest.tsv').read_bytes(), manifest)
        self.assertEqual(log_path.read_bytes(), log)

    def test_lockfile_checker_detects_drift_missing_and_unlocked(self):
        check = runpy.run_path(str(ROOT / 'tests/my-check-lockfile.py'))['check']
        repos = self.root / 'repos'
        repo = repos / 'example'
        repo.mkdir(parents=True)
        self.assertEqual(self.run_cmd('git', 'init', '-q', str(repo)).returncode, 0)
        self.assertEqual(self.run_cmd('git', '-C', str(repo), '-c', 'user.name=Fixture', '-c', 'user.email=fixture@example.invalid', 'commit', '--allow-empty', '-qm', 'fixture').returncode, 0)
        head = self.run_cmd('git', '-C', str(repo), 'rev-parse', 'HEAD').stdout.strip()
        lock = self.root / 'lock.el'
        lock.write_text(f'(("example" . "{head}"))\n:epsilon\n')
        with contextlib.redirect_stdout(io.StringIO()):
            check(lock, repos)
            (repo / 'dirty').write_text('dirty')
            with self.assertRaises(ValueError):
                check(lock, repos)
            (repo / 'dirty').unlink()
            lock.write_text('(("example" . "' + '0' * 40 + '"))\n:epsilon\n')
            with self.assertRaises(ValueError):
                check(lock, repos)
            shutil.rmtree(repo)
            with self.assertRaises(ValueError):
                check(lock, repos)
            check(lock, repos, ['example'])
            (repos / 'unlocked').mkdir()
            with self.assertRaises(ValueError):
                check(lock, repos, ['example'])


if __name__ == '__main__':
    unittest.main()
