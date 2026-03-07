#include <signal.h>
#include <unistd.h>
#include <stdlib.h>

static int segv_exit_code = 0;

static void segv_handler(int sig) {
    (void)sig;
    _exit(segv_exit_code);
}

void install_segv_handler(int exit_code) {
    segv_exit_code = exit_code;
    struct sigaction sa;
    sa.sa_handler = segv_handler;
    sigemptyset(&sa.sa_mask);
    sa.sa_flags = 0;
    sigaction(SIGSEGV, &sa, NULL);
}
