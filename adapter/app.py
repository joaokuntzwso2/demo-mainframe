import os
import time
import socket
import json
from flask import Flask, request, jsonify

app = Flask(__name__)

TK_HOST = os.getenv("TK_HOST", "tk")
TK_SOCKREADER_PORT = int(os.getenv("TK_SOCKREADER_PORT", "3505"))
PRT_FILE = os.getenv("PRT_FILE", "/demo/out/prt00e.txt")
POLL_MS = int(os.getenv("PRT_POLL_INTERVAL_MS", "200"))
TIMEOUT_SEC = int(os.getenv("PRT_TIMEOUT_SEC", "20"))

def submit_jcl(jcl_text: str) -> None:
    payload = jcl_text.replace("\r\n", "\n").replace("\r", "\n")
    if not payload.endswith("\n"):
        payload += "\n"

    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s.settimeout(8)
    s.connect((TK_HOST, TK_SOCKREADER_PORT))
    s.sendall(payload.encode("utf-8", errors="ignore"))
    s.shutdown(socket.SHUT_WR)
    s.close()

def wait_for_json_line(start_pos: int) -> dict:
    deadline = time.time() + TIMEOUT_SEC
    pos = start_pos

    while time.time() < deadline:
        if os.path.exists(PRT_FILE):
            with open(PRT_FILE, "r", errors="ignore") as f:
                f.seek(pos)
                chunk = f.read()
                pos = f.tell()

            lines = chunk.splitlines()
            json_lines = [ln.strip() for ln in lines if ln.strip().startswith("JSON:")]
            if json_lines:
                last = json_lines[-1][5:].strip()
                try:
                    return json.loads(last)
                except Exception:
                    return {"raw": last}

        time.sleep(POLL_MS / 1000.0)

    return {"error": "TIMEOUT_WAITING_FOR_MAINFRAME_OUTPUT", "prtFile": PRT_FILE}

@app.get("/health")
def health():
    return jsonify({
        "status": "ok",
        "tkHost": TK_HOST,
        "sockReaderPort": TK_SOCKREADER_PORT,
        "printerFile": PRT_FILE
    })

@app.post("/submit")
def submit():
    body = request.get_json(force=True, silent=True) or {}
    jcl = body.get("jcl")
    if not jcl:
        return jsonify({"error": "Missing field 'jcl'"}), 400

    start_pos = 0
    if os.path.exists(PRT_FILE):
        try:
            start_pos = os.path.getsize(PRT_FILE)
        except Exception:
            start_pos = 0

    submit_jcl(jcl)

    if body.get("expectJson", True):
        return jsonify(wait_for_json_line(start_pos))

    return jsonify({"status": "SUBMITTED"})

if __name__ == "__main__":
    # Important: listen on all interfaces so Docker port mapping works
    app.run(host="0.0.0.0", port=9000)
