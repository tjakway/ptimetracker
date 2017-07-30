/* exec-notify, so you can watch your acrobat reader or vim executing "bash -c"
 * commands ;-)
 * Requires some 2.6.x Linux kernel with proc connector enabled.
 *
 * $  cc -Wall -ansi -pedantic -std=c99 exec-notify.c
 *
 * (C) 2007-2010 Sebastian Krahmer <krahmer@suse.de> original netlink handling
 * stolen from an proc-connector example, copyright folows:
 */
/*
 *
 * Copyright (C) Matt Helsley, IBM Corp. 2005
 * Derived from fcctl.c by Guillaume Thouvenin
 * Original copyright notice follows:
 *
 * Copyright (C) 2005 BULL SA.
 * Written by Guillaume Thouvenin <guillaume.thouvenin@bull.net>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */
#include "exec-notify.h"
#include "APIState.h"

void handle_msg (void* state, struct cn_msg *cn_hdr)
{
	char cmdline[1024], fname1[1024], ids[1024], fname2[1024], buf[1024];
	int r = 0, fd, i;
	FILE *f = NULL;
	struct proc_event *ev = (struct proc_event *)cn_hdr->data;

	snprintf(fname1, sizeof(fname1), "/proc/%d/status", ev->event_data.exec.process_pid);
	snprintf(fname2, sizeof(fname2), "/proc/%d/cmdline", ev->event_data.exec.process_pid);

	f = fopen(fname1, "r");
	fd = open(fname2, O_RDONLY);

	memset(&cmdline, 0, sizeof(cmdline));
	memset(&ids, 0, sizeof(ids));

	while (f && fgets(buf, sizeof(buf), f) != NULL) {
		if (strstr(buf, "Uid")) {
			strtok(buf, "\n");
			snprintf(ids, sizeof(ids), "%s", buf);
		}
	}
	if (f)
		fclose(f);

	if (fd > 0) {
		r = read(fd, cmdline, sizeof(cmdline));
		close(fd);

		for (i = 0; r > 0 && i < r; ++i) {
			if (cmdline[i] == 0)
				cmdline[i] = ' ';
		}
	}

        const unsigned int LOG_BUF_LEN=500;
        char log[LOG_BUF_LEN];

	switch(ev->what){
	case PROC_EVENT_FORK:
		snprintf(log, LOG_BUF_LEN, 
                        "FORK:parent(pid,tgid)=%d,%d\tchild(pid,tgid)=%d,%d\t[%s]\n",
		       ev->event_data.fork.parent_pid,
		       ev->event_data.fork.parent_tgid,
		       ev->event_data.fork.child_pid,
		       ev->event_data.fork.child_tgid, cmdline);
                apiWriteLog(state, log);
		break;
	case PROC_EVENT_EXEC:
		snprintf(log, LOG_BUF_LEN,
                        "EXEC:pid=%d,tgid=%d\t[%s]\t[%s]\n",
		       ev->event_data.exec.process_pid,
		       ev->event_data.exec.process_tgid, ids, cmdline);
                apiWriteLog(state, log);

                execMatches(state, ev->event_data.exec.process_pid, PROC_START);

		break;
	case PROC_EVENT_EXIT:
		snprintf(log, LOG_BUF_LEN,
                        "EXIT:pid=%d,%d\texit code=%d\n",
		       ev->event_data.exit.process_pid,
		       ev->event_data.exit.process_tgid,
		       ev->event_data.exit.exit_code);
                apiWriteLog(state, log);

                //check callbacks
                execMatches(state, ev->event_data.exit.process_pid, PROC_END);

		break;
	case PROC_EVENT_UID:
		snprintf(log, LOG_BUF_LEN,
                        "UID:pid=%d,%d ruid=%d,euid=%d\n",
			ev->event_data.id.process_pid, ev->event_data.id.process_tgid,
			ev->event_data.id.r.ruid, ev->event_data.id.e.euid);
                apiWriteLog(state, log);
		break;
	default:
		break;
	}
}
