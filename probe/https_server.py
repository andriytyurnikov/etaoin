"""Throwaway probe, fork-only. Serves the static test fixtures over TLS.

The repo's own test server is plain HTTP, and Safari's WebDriver does not honour
acceptInsecureCerts, so testing the https path needs a real server holding a cert
the system actually trusts. Deliberately dumb: no directory listings needed, no
concurrency, just enough to answer whether the scheme changes Safari's behaviour.

usage: https_server.py <port> <combined-pem> <docroot>
"""

import http.server
import os
import ssl
import sys

port, pem, docroot = int(sys.argv[1]), sys.argv[2], sys.argv[3]
os.chdir(docroot)

httpd = http.server.HTTPServer(("localhost", port),
                               http.server.SimpleHTTPRequestHandler)
ctx = ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)
ctx.load_cert_chain(pem)
httpd.socket = ctx.wrap_socket(httpd.socket, server_side=True)

print(f"serving {docroot} at https://localhost:{port}", flush=True)
httpd.serve_forever()
