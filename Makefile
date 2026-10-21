PROJECT_SYSTEM := beadwork
TEST_SYSTEM := beadwork/tests
BINARY_NAME := bw
include .cl-make/cl-project.mk

# ── Test wiring ───────────────────────────────────────────────────────────
#
# The CLI suite spawns bin/bw as a subprocess: a real argv, a real exit code,
# and a clean image per case (bd-orw).  That makes the binary part of the test
# surface, so `make test` must build it first -- otherwise a stale bin/bw would
# silently grade old code.  Focused runs (`make test-eval`) skip the rebuild
# and instead fail loudly in the harness if bin/bw is older than src/.
test: build
