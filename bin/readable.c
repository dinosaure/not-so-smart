/* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. */

#include <caml/mlvalues.h>
#include <caml/unixsupport.h>
#include <poll.h>

CAMLprim value smart_unix_readable(value fd)
{
    struct pollfd pollfd;
    pollfd.fd = Int_val(fd);
    pollfd.events = POLLIN;
    pollfd.revents = 0;
    if (poll(&pollfd, 1, -1) < 0) uerror("readable", Nothing);
    return (Val_bool(1));
}
