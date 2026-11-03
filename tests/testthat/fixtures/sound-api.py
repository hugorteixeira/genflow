"""Loopback-only audio fixture. Never forwards requests to a provider."""
import http.server
import json
import pathlib
import sys
import urllib.parse

audio, port_file, log_file = map(pathlib.Path, sys.argv[1:])


class Handler(http.server.BaseHTTPRequestHandler):
    def log_message(self, *args):
        pass

    def reply(self, status, value, content_type="application/json"):
        data = value if isinstance(value, bytes) else json.dumps(value).encode()
        self.send_response(status)
        self.send_header("Content-Type", content_type)
        self.send_header("Content-Length", str(len(data)))
        self.end_headers()
        self.wfile.write(data)

    def do_POST(self):
        body = json.loads(self.rfile.read(int(self.headers.get("Content-Length", "0"))))
        with log_file.open("a") as log:
            log.write(json.dumps({"path": self.path, "body": body}) + "\n")
        route = urllib.parse.urlparse(self.path).path
        if route.startswith("/v1/"):
            if self.headers.get("xi-api-key") != "fixture-eleven":
                return self.reply(401, {"error": "credential"})
            prompt = body.get("text", body.get("prompt", ""))
            if prompt == "http503":
                return self.reply(503, {"error": "unavailable"})
            if prompt == "html":
                return self.reply(200, b"<html>not audio</html>", "text/html")
            if prompt == "redirect":
                self.send_response(307)
                self.send_header("Location", "/redirect-target")
                self.end_headers()
                return
            return self.reply(200, audio.read_bytes(), "application/octet-stream")
        if self.headers.get("Authorization") != "Bearer fixture-local":
            return self.reply(401, {"error": "credential"})
        if route == "/release_task":
            return self.reply(200, {"code": 200, "error": None, "data": {"task_id": "task-1"}})
        if route == "/query_result":
            return self.reply(200, {"code": 200, "error": None, "data": [{"task_id": "task-1", "status": 1,
                "result": json.dumps([{"file": "/v1/audio?path=fixture.wav"}])}]})
        self.reply(404, {})

    def do_GET(self):
        if self.headers.get("Authorization") != "Bearer fixture-local":
            return self.reply(401, {"error": "credential"})
        if self.path.startswith("/v1/audio?"):
            return self.reply(200, audio.read_bytes(), "audio/wav")
        if self.path == "/v1/models":
            return self.reply(200, {"code": 200, "error": None, "data": {"models": [{"name": "acestep-v15-turbo", "is_default": True}]}})
        self.reply(404, {})


server = http.server.ThreadingHTTPServer(("127.0.0.1", 0), Handler)
port_file.write_text(str(server.server_port))
server.serve_forever()
