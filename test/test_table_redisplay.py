"""Stream-boundary regressions for the real terminal cursor checker."""

from pathlib import Path
import runpy
import unittest

Cursor = runpy.run_path(str(Path(__file__).with_name("table-redisplay.py")))["Cursor"]


class CursorStreamTest(unittest.TestCase):
    def test_chunk_boundaries_preserve_cursor_and_checkpoints(self):
        stream = (
            "\x1b(Babc\r\n\t\b\x1b[3;5H汉\x1b[38;5;42m"
            "\x1b]777;cursor;2;6\a\x1b]777;reference;1\a"
            "\x1b[2C\x1b[2D\x1b]777;compare;1\a"
            "\x1b]777;watch;2;3\a\x1b[C\x1b]777;unwatch\a"
            "\x1b]777;done;3\a"
        )
        for split in range(len(stream) + 1):
            with self.subTest(split=split):
                cursor = Cursor()
                cursor.feed(stream[:split])
                cursor.feed(stream[split:])
                self.assertEqual((cursor.row, cursor.col), (2, 7))
                self.assertEqual(cursor.checks, 3)
                self.assertEqual(cursor.expected_checks, 3)
                self.assertEqual(cursor.errors, [])
                self.assertEqual(cursor.references, {})
                self.assertIsNone(cursor.watch)
                self.assertEqual(cursor.pending, "")
        cursor = Cursor()
        for char in stream:
            cursor.feed(char)
        self.assertEqual((cursor.row, cursor.col, cursor.checks), (2, 7, 3))
        self.assertEqual(cursor.errors, [])

    def test_incomplete_sequences_are_retained(self):
        cursor = Cursor()
        cursor.feed("plain\x1b[12;")
        self.assertEqual(cursor.pending, "\x1b[12;")
        cursor.feed("7H\x1b]777;cursor;11;")
        self.assertEqual(cursor.pending, "\x1b]777;cursor;11;")
        cursor.feed("6\a")
        self.assertEqual(cursor.errors, [])
        self.assertEqual(cursor.checks, 1)
        self.assertEqual(cursor.pending, "")

    def test_repaint_detection_survives_chunking(self):
        cursor = Cursor()
        for part in ("\x1b]777;watch;1;3\a\x1b[2;4H", "x\x1b[", "K\x1b]777;unwatch\a"):
            cursor.feed(part)
        self.assertEqual(cursor.errors, [(1, "no table repaint", 2)])

    def test_large_plain_chunk_before_escape(self):
        cursor = Cursor()
        cursor.feed("x" * 100_000 + "\x1b[2;3H\x1b]777;cursor;1;2\a")
        self.assertEqual(cursor.errors, [])
        self.assertEqual(cursor.checks, 1)
        self.assertEqual(cursor.pending, "")


if __name__ == "__main__":
    unittest.main()
