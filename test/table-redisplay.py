"""Check Emacs's real TTY cursor without third-party terminal dependencies."""

import codecs
import errno
import fcntl
import os
from pathlib import Path
import pty
import re
import select
import struct
import subprocess
import termios
import time
import unicodedata


class Cursor:
    """Track VT cursor movement and validate private OSC checkpoints."""

    def __init__(self):
        self.row = self.col = 0
        self.pending = ""
        self.checks = 0
        self.expected_checks = None
        self.errors = []

    def feed(self, text):
        self.pending += text
        while self.pending:
            char = self.pending[0]
            if char == "\x1b":
                match = re.match(r"\x1b\[([0-9;? >]*)([@-~])", self.pending)
                if match:
                    raw, command = match.groups()
                    params = [int(p or 0) for p in raw.split(";")] if not any(
                        c in raw for c in "? >"
                    ) else []
                    n = (params[0] if params else 0) or 1
                    if command in "Hf":
                        self.row = n - 1
                        self.col = ((params[1] if len(params) > 1 else 0) or 1) - 1
                    elif command == "A":
                        self.row = max(0, self.row - n)
                    elif command in "Be":
                        self.row += n
                    elif command in "Ca":
                        self.col += n
                    elif command == "D":
                        self.col = max(0, self.col - n)
                    elif command == "d":
                        self.row = n - 1
                    elif command in "G`":
                        self.col = n - 1
                    elif command == "r":
                        self.row = self.col = 0
                    self.pending = self.pending[match.end():]
                    continue
                if self.pending.startswith("\x1b]"):
                    end = self.pending.find("\a")
                    if end < 0:
                        return
                    payload = self.pending[2:end]
                    if payload.startswith("777;cursor;"):
                        expected = tuple(map(int, payload.split(";")[2:]))
                        self.checks += 1
                        if expected != (self.row, self.col):
                            self.errors.append((self.checks, expected, (self.row, self.col)))
                    elif payload.startswith("777;done;"):
                        self.expected_checks = int(payload.split(";")[2])
                    self.pending = self.pending[end + 1:]
                    continue
                if len(self.pending) < 2 or self.pending.startswith("\x1b["):
                    return
                if self.pending[1] in "()":
                    if len(self.pending) < 3:
                        return
                    self.pending = self.pending[3:]
                else:
                    self.pending = self.pending[2:]
                continue
            self.pending = self.pending[1:]
            if char == "\r":
                self.col = 0
            elif char == "\n":
                self.row += 1
            elif char == "\b":
                self.col = max(0, self.col - 1)
            elif char == "\t":
                self.col = (self.col // 8 + 1) * 8
            elif char >= " " and not unicodedata.combining(char):
                self.col += 2 if unicodedata.east_asian_width(char) in "WF" else 1


def main():
    root = Path(__file__).resolve().parent.parent
    master, slave = pty.openpty()
    fcntl.ioctl(slave, termios.TIOCSWINSZ, struct.pack("HHHH", 40, 120, 0, 0))
    command = [
        os.environ.get("EMACS", "emacs"), "-Q", "-nw",
        "--eval", "(progn (require 'package) (package-initialize) (setq load-prefer-newer t))",
        "-L", str(root), "-l", str(root / "test/markdown-ts-appear-table-redisplay-test.el"),
        "-f", "markdown-ts-appear-table-redisplay-run",
    ]
    def controlling_terminal():
        os.setsid()
        fcntl.ioctl(0, termios.TIOCSCTTY, 0)

    process = subprocess.Popen(command, cwd=root, stdin=slave, stdout=slave, stderr=slave,
                               preexec_fn=controlling_terminal,
                               env={**os.environ, "TERM": "xterm-256color"})
    os.close(slave)
    cursor = Cursor()
    decoder = codecs.getincrementaldecoder("utf-8")("replace")
    transcript = []
    deadline = time.monotonic() + 120
    try:
        while time.monotonic() < deadline:
            ready, _, _ = select.select([master], [], [], 0.2)
            if not ready:
                if process.poll() is not None:
                    break
                continue
            try:
                data = os.read(master, 65536)
            except OSError as error:
                if error.errno == errno.EIO:
                    break
                raise
            if not data:
                break
            text = decoder.decode(data)
            transcript.append(text)
            cursor.feed(text)
        else:
            raise TimeoutError("Emacs redisplay regression timed out")
        code = process.wait(timeout=5)
        if code or cursor.errors or cursor.checks == 0 or cursor.checks != cursor.expected_checks:
            print("".join(transcript)[-8000:])
            raise AssertionError(
                f"exit={code}, checkpoints={cursor.checks}, mismatches={cursor.errors[:20]}"
            )
        print(f"PASS: {cursor.checks} actual terminal cursor checkpoints")
    finally:
        if process.poll() is None:
            process.terminate()
            process.wait(timeout=5)
        os.close(master)


if __name__ == "__main__":
    main()
