#!/usr/bin/python

import time
import sys
import os
import signal


timeout=int(sys.argv[1])

args=["./test_all", "--host=gui-srva", "--port=1111"]+sys.argv[1:]

pid= os.spawnv(os.P_NOWAIT, args[0], args)

TEST_STATUS=None


print time.ctime()+": waiting for command for %i seconds..."%timeout
c= 0
while c < timeout:
    p, s= os.waitpid(pid, os.WNOHANG)
    if p != 0:
        break

    time.sleep(1)
    c+= 1

if c == timeout:
    print time.ctime()+": test program has timedout. Killing process..."
    TEST_STATUS=False
    os.kill(pid, signal.SIGTERM)
else:
    if os.WIFSIGNALED(s):
        print time.ctime()+": test program terminated with signal", os.WTERMSIG(s)
        TEST_STATUS=False
    else:
        print time.ctime()+": test program finished with status", os.WEXITSTATUS(s)
        TEST_STATUS=s==0

if not TEST_STATUS:
    print "::The test suite has FAILED."
    sys.exit(1)
else:
    print "::The test suite has PASSED."
    sys.exit(0)
