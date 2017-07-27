#include "exec-notify.h"
#include "APIState.h"


int main(int argc, char** argv) {
    void* state = initializeAPIState();
    
    int res = startListening(state);

    freeAPIState(state);
    return res;
}
