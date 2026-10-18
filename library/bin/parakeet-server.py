# /// script
# requires-python = ">=3.11"
# dependencies = ["parakeet-mlx"]
# ///
"""Resident Parakeet speech-to-text server for whisper.el.

Speaks the whisper.cpp server protocol that whisper.el's `remote' mode
uses: POST multipart/form-data with a `file' field to /inference, get
back {"text": ...}.  Keeping the model loaded cuts a dictation from ~3 s
(new Python process + model load per call) to ~0.3 s.

Run by launchd (~/Library/LaunchAgents/com.ilya.parakeet-server.plist):
    uv run --script parakeet-server.py [--port 8642]
"""

import argparse
import json
import os
import sys
import tempfile
import time
import wave
from email.parser import BytesParser
from email.policy import HTTP
from http.server import BaseHTTPRequestHandler, HTTPServer

import mlx.core as mx
from parakeet_mlx import from_pretrained

MODEL_ID = "mlx-community/parakeet-tdt-0.6b-v3"


def log(msg):
    print(f"{time.strftime('%F %T')} {msg}", file=sys.stderr, flush=True)


def uploaded_file(headers, body):
    """Return the bytes of the `file' field in a multipart BODY."""
    msg = BytesParser(policy=HTTP).parsebytes(
        b"Content-Type: " + headers["Content-Type"].encode() + b"\r\n\r\n" + body)
    for part in msg.iter_parts():
        if part.get_param("name", header="content-disposition") == "file":
            return part.get_payload(decode=True)
    return None


def transcribe(model, audio):
    with tempfile.NamedTemporaryFile(suffix=".wav") as f:
        f.write(audio)
        f.flush()
        text = model.transcribe(f.name).text.strip()
    # Drop MLX's buffer cache so the idle footprint is just the weights.
    mx.clear_cache()
    return text


def warm_up(model):
    """Transcribe a second of silence so the first real request is fast."""
    with tempfile.NamedTemporaryFile(suffix=".wav") as f:
        with wave.open(f.name, "wb") as w:
            w.setnchannels(1)
            w.setsampwidth(2)
            w.setframerate(16000)
            w.writeframes(b"\0\0" * 16000)
        model.transcribe(f.name)
    mx.clear_cache()


def make_handler(model):
    class Handler(BaseHTTPRequestHandler):
        def reply(self, status, payload):
            data = json.dumps(payload).encode()
            self.send_response(status)
            self.send_header("Content-Type", "application/json")
            self.send_header("Content-Length", str(len(data)))
            self.end_headers()
            self.wfile.write(data)

        def do_GET(self):
            self.reply(200, {"status": "ok", "model": MODEL_ID})

        def do_POST(self):
            if self.path.rstrip("/") != "/inference":
                return self.reply(404, {"error": "not found"})
            body = self.rfile.read(int(self.headers.get("Content-Length", 0)))
            audio = uploaded_file(self.headers, body)
            if not audio:
                return self.reply(400, {"error": "no `file' field"})
            t = time.perf_counter()
            try:
                text = transcribe(model, audio)
            except Exception as e:
                log(f"transcription failed: {e!r}")
                return self.reply(500, {"error": str(e)})
            log(f"{len(audio) / 32000:.1f}s audio in {time.perf_counter() - t:.2f}s")
            self.reply(200, {"text": text})

        def log_message(self, *args):
            pass

    return Handler


def main():
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--host", default="127.0.0.1")
    parser.add_argument("--port", type=int, default=8642)
    args = parser.parse_args()

    model = from_pretrained(MODEL_ID)
    warm_up(model)
    log(f"{MODEL_ID} ready on {args.host}:{args.port} (pid {os.getpid()})")
    HTTPServer((args.host, args.port), make_handler(model)).serve_forever()


if __name__ == "__main__":
    main()
