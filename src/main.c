#include "exec-notify.h"

int main(int argc, char** argv) {
    return register_proc_msg_handler(&handle_msg);
}
