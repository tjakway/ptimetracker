#include "exec-notify.h"

int main(int argc, char** argv) {
    register_proc_msg_handler(handle_msg);
}
