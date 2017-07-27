#include "Util.h"
#include "exec-notify.h"
#include "APIState.h"

#include <iostream>
#include <stdexcept>
#include <functional>
#include <algorithm>
#include <chrono>


namespace ptimetracker {

const int EXEC_NOTIFY_BUFF_SIZE = 
    std::max<int>(std::max<int>(SEND_MESSAGE_SIZE, RECV_MESSAGE_SIZE), 1024);
const int MIN_RECV_SIZE = std::min<int>(SEND_MESSAGE_SIZE, RECV_MESSAGE_SIZE);

NEW_EXCEPTION_TYPE(MustBeRootException);
NEW_EXCEPTION_TYPE(NetlinkSocketException);
NEW_EXCEPTION_TYPE(MulticastMessageIgnoredException);

    //TODO: maybe pass a stream to write log messages to?
void listenForMessages(APIState* state,
        std::function<bool(APIState*, cn_msg*)> shouldContinue,
        std::function<void(void*, cn_msg*)> msgHandlerCallback)
{
    int sk_nl;
    int err;
    struct sockaddr_nl my_nla, kern_nla, from_nla;
    socklen_t from_nla_len;
    char buff[EXEC_NOTIFY_BUFF_SIZE];
    int rc = -1;
    struct nlmsghdr *nl_hdr;
    struct cn_msg *cn_hdr;
    enum proc_cn_mcast_op *mcop_msg;
    size_t recv_len = 0;

    if (getuid() != 0) {
            throw MustBeRootException("Only root can start/stop the fork connector\n");
    }

    setvbuf(stdout, NULL, _IONBF, 0);

    /*
        * Create an endpoint for communication. Use the kernel user
        * interface device (PF_NETLINK) which is a datagram oriented
        * service (SOCK_DGRAM). The protocol used is the connector
        * protocol (NETLINK_CONNECTOR)
        */
    sk_nl = socket(PF_NETLINK, SOCK_DGRAM, NETLINK_CONNECTOR);
    if (sk_nl == -1) {
        throw NetlinkSocketException("Socket sk_nl error");
    }
    my_nla.nl_family = AF_NETLINK;
    my_nla.nl_groups = CN_IDX_PROC;
    my_nla.nl_pid = getpid();

    kern_nla.nl_family = AF_NETLINK;
    kern_nla.nl_groups = CN_IDX_PROC;
    kern_nla.nl_pid = 1;

    err = bind(sk_nl, (struct sockaddr *)&my_nla, sizeof(my_nla));
    if (err == -1) {
            close(sk_nl);
            throw NetlinkSocketException("binding sk_nl error");
    }

    nl_hdr = (struct nlmsghdr *)buff;
    cn_hdr = (struct cn_msg *)NLMSG_DATA(nl_hdr);
    mcop_msg = (enum proc_cn_mcast_op*)&cn_hdr->data[0];

    printf("sending proc connector: PROC_CN_MCAST_LISTEN... ");
    memset(buff, 0, sizeof(buff));
    *mcop_msg = (enum proc_cn_mcast_op)PROC_CN_MCAST_LISTEN;

    /* fill the netlink header */
    nl_hdr->nlmsg_len = SEND_MESSAGE_LEN;
    nl_hdr->nlmsg_type = NLMSG_DONE;
    nl_hdr->nlmsg_flags = 0;
    nl_hdr->nlmsg_seq = 0;
    nl_hdr->nlmsg_pid = getpid();
    /* fill the connector header */
    cn_hdr->id.idx = CN_IDX_PROC;
    cn_hdr->id.val = CN_VAL_PROC;
    cn_hdr->seq = 0;
    cn_hdr->ack = 0;
    cn_hdr->len = sizeof(enum proc_cn_mcast_op);
    if (send(sk_nl, nl_hdr, nl_hdr->nlmsg_len, 0) != nl_hdr->nlmsg_len) {
            close(sk_nl);
            throw NetlinkSocketException("failed to send proc connector mcast ctl op!");
    }

    printf("sent\n");
    if (*mcop_msg == PROC_CN_MCAST_IGNORE) {
            throw MulticastMessageIgnoredException("Received PROC_CN_MCAST_IGNORE");
    }

    printf("Reading process events from proc connector.\n"
            "Hit Ctrl-C to exit\n");

    for(memset(buff, 0, sizeof(buff)), from_nla_len = sizeof(from_nla);
        ; memset(buff, 0, sizeof(buff)), from_nla_len = sizeof(from_nla)) {
            struct nlmsghdr *nlh = (struct nlmsghdr*)buff;
            memcpy(&from_nla, &kern_nla, sizeof(from_nla));
            recv_len = recvfrom(sk_nl, buff, EXEC_NOTIFY_BUFF_SIZE, 0,
                            (struct sockaddr*)&from_nla, &from_nla_len);
            if (from_nla.nl_pid != 0)
                    continue;
            if (recv_len < 1)
                    continue;

            while (true) {
                    if(!NLMSG_OK(nlh, recv_len))
                        break;

                    cn_hdr = (struct cn_msg*)NLMSG_DATA(nlh);
                    if (nlh->nlmsg_type == NLMSG_NOOP)
                            continue;
                    if ((nlh->nlmsg_type == NLMSG_ERROR) ||
                        (nlh->nlmsg_type == NLMSG_OVERRUN))
                            break;
                    //invoke the callback
                    msgHandlerCallback(state, cn_hdr);
                    if (nlh->nlmsg_type == NLMSG_DONE)
                            break;
                    nlh = NLMSG_NEXT(nlh, recv_len);

                    if(!shouldContinue(state, cn_hdr))
                        break;
            }
    }
}

} //namespace ptimetracker

namespace {
    static std::function<void(void*, cn_msg*)> handleMsgCallback = handle_msg;
}


extern "C" {
    int listenForMessagesForever(void* state) 
    {
        ptimetracker::returnOnException([&]() {
            auto shouldContinue = [](ptimetracker::APIState* a, cn_msg* b) { return true; };
            

            listenForMessages((ptimetracker::APIState*)state,
                    shouldContinue, handleMsgCallback);
            return 0;
        });
    }

    void listenUntilElapsed(void* state, unsigned long millis)
    {
        ptimetracker::returnOnException([&]() {
            //get current time & cast -> milliseconds using std::chrono functions
            const auto nowInMillis = []() {
                    return std::chrono::duration_cast<std::chrono::milliseconds>
                            (std::chrono::system_clock::now().time_since_epoch());
            };

            const std::chrono::milliseconds start = nowInMillis(),
                                            //convert parameter -> std::chrono::milliseconds
                                            _millis = std::chrono::milliseconds(millis);

            const auto shouldContinue = [&nowInMillis, &start, &_millis](ptimetracker::APIState* s, cn_msg* msg)
            {
                const std::chrono::milliseconds diff = nowInMillis() - start;
                
                //stop if too much time has elapsed
                if(diff > _millis) {
                    return false;
                } else {
                    return true;
                }
            };

            listenForMessages((ptimetracker::APIState*)state, shouldContinue, handleMsgCallback);
            return 0;
        });
    }
}
